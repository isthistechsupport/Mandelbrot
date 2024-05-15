module Mandelbrot

open SixLabors.ImageSharp
open SixLabors.ImageSharp.PixelFormats
open System.IO
open System.Threading.Tasks


type Mandelbrot () =
    static member private scale (x0:float) x1 y1 = (x0/x1) * y1


    static member private coordToMandelbrotDepth (x: float, y: float, x0, y0, i, maxDepth) =
        let rec loop (x: float, y: float, i) =
            let x2, y2 = x*x, y*y
            match x2 + y2 <= 4.0 && i < maxDepth with
            | true -> loop (x2 - y2 + x0, 2.0*x*y + y0, i + 1)
            | false -> i, maxDepth
        loop (x, y, i)


    static member private mandelbrotDepthToHsl (depth,  maxDepth) = (((Mandelbrot.scale (float depth) (float maxDepth) 360.0) + 180.0) % 360.0, 1.0, Mandelbrot.scale (float depth) (float maxDepth) 1.0)


    static member private hslToPixelTuple = function
        | _, _, lightness when lightness = 0.0 || lightness = 1.0 -> (0uy, 0uy, 0uy)
        | hue, saturation, lightness ->
            //let hue = 210.0
            //let saturation = 1.0
            let c = (1.0 - abs(2.0 * lightness - 1.0)) * saturation
            let x = c * (1.0 - abs((hue / 60.0) % 2.0 - 1.0))
            let m = lightness - c / 2.0
            let r', g', b' = 
                match int hue / 60 with
                | 0 -> c, x, 0.0
                | 1 -> x, c, 0.0
                | 2 -> 0.0, c, x
                | 3 -> 0.0, x, c
                | 4 -> x, 0.0, c
                | _ -> c, 0.0, x
            let r, g, (b: float) = r' + m, g' + m, b' + m
            byte <| System.Math.Round(r * 255.0), byte <| System.Math.Round(g * 255.0), byte <| System.Math.Round(b * 255.0)


    static member private pixelTupleToPixel (r, g, b) = new Rgb24(r, g, b)


    static member private nToWrittenPixel n imageSideLen windowSideLen x0 y0 maxDepth (image : Image<Rgb24>) =
        let x = n % imageSideLen
        let y = n / imageSideLen
        let pixel =
            (0.0, 0.0, ((Mandelbrot.scale (float x) (float imageSideLen) (windowSideLen)) + (x0)), ((Mandelbrot.scale (float y) (float imageSideLen) (windowSideLen)) + (y0)), 0, maxDepth)
            |> (Mandelbrot.coordToMandelbrotDepth >> Mandelbrot.mandelbrotDepthToHsl >> Mandelbrot.hslToPixelTuple >> Mandelbrot.pixelTupleToPixel)
        image[x, y] <- pixel


    static member dimsToMSetByteArray (?imageSideLen, ?windowSideLen, ?x, ?y, ?maxDepth) =
        let imageSideLen = defaultArg imageSideLen 1024
        let windowSideLen = defaultArg windowSideLen 3.0
        let x = defaultArg x -0.5
        let y = defaultArg y 0.0
        let maxDepth = defaultArg maxDepth 50
        let x0 = x - (windowSideLen / 2.0)
        let y0 = y - (windowSideLen / 2.0)
        let memStream = new MemoryStream()
        using (new Image<Rgb24>(imageSideLen, imageSideLen)) (fun image ->
            Parallel.ForEach ([0..(imageSideLen * imageSideLen - 1)], fun n -> Mandelbrot.nToWrittenPixel n imageSideLen windowSideLen x0 y0 maxDepth image)
            |> ignore
            image.SaveAsWebp memStream)
        memStream.ToArray()
