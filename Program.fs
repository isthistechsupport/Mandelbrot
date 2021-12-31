#if INTERACTIVE
#r "nuget: SixLabors.ImageSharp"
#endif

open System.IO
open SixLabors.ImageSharp
open SixLabors.ImageSharp.PixelFormats
open System.Diagnostics

let scale (x0:float) x1 y1 = (x0/x1) * y1

let rec coordToMSetValue = function
| x:float, y:float, x0, y0, i, maxDepth when x*x + y*y <= 4 && i <= maxDepth -> coordToMSetValue (x*x - y*y + x0, 2.*x*y + y0, x0, y0, i+1, maxDepth)
| _, _, _, _, i, maxDepth when i >= maxDepth -> 256
| _, _, _, _, i, maxDepth -> int <| System.Math.Round (scale (float i) (float maxDepth) 255.)

let mSetValueToPixelTuple = function
| x when x = 256 -> (0uy, 0uy, 0uy)
| x when x > 127 -> (byte x, 255uy, byte x)
| x -> (0uy, byte x, 0uy)

let pixelTupleToPixel (r, g, b) = new Rgb24(r, g, b)

let dimsToMSetImage imageSideLen windowSideLen x0 y0 maxDepth =
    using (new Image<Rgb24>(imageSideLen, imageSideLen)) (fun image ->
        Array2D.init imageSideLen imageSideLen (fun x y -> 
            (0.0, 0.0, ((scale (float x) (float imageSideLen) (windowSideLen)) + x0), ((scale (float y) (float imageSideLen) (windowSideLen)) + y0), 0, maxDepth))
        |> Array2D.map (coordToMSetValue >> mSetValueToPixelTuple >> pixelTupleToPixel)
        |> Array2D.iteri (fun x y rgb -> image[x, y] <- rgb)
        File.Open("mandelbrot.jpg", FileMode.OpenOrCreate) |> image.SaveAsJpeg)

[<EntryPoint>]
let main argv =
    let stopwatch = Stopwatch.StartNew()
    dimsToMSetImage (int argv[0]) (float argv[1]) (float argv[2]) (float argv[3]) (int argv[4])
    stopwatch.Stop()
    printfn "Image rendered. Total time: %s" <| stopwatch.Elapsed.ToString()
    0