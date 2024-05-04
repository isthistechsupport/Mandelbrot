#if INTERACTIVE
#r "nuget: Argu"
#r "nuget: SixLabors.ImageSharp"
#endif
open Argu
open System.IO
open SixLabors.ImageSharp
open SixLabors.ImageSharp.PixelFormats
open System.Diagnostics
open System.Threading.Tasks


type CliArguments =
    | [<AltCommandLine("-i")>] ImageSideLength of pixels: int
    | [<AltCommandLine("-w")>] WindowSideLength of length: float
    | [<AltCommandLine("-x")>] X of value: float
    | [<AltCommandLine("-y")>] Y of value: float
    | [<AltCommandLine("-d")>] MaxDepth of value: int
    | [<AltCommandLine("-a")>] Async
    | [<AltCommandLine("-s")>] Silent
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | ImageSideLength _ -> "specify the size of the image in pixels (default 1024)."
            | WindowSideLength _ -> "specify the size of the render window (default 3.0)."
            | X _ -> "specify the x coordinate of the center of the render window (default -0.5)"
            | Y _ -> "specify the y coordinate of the center of the render window (default 0)"
            | MaxDepth _ -> "specify the maximum recursion depth (default 50)."
            | Async -> "use asynchronous rendering."
            | Silent -> "suppress output."


let scale (x0:float) x1 y1 = (x0/x1) * y1


let rec coordToMSetValue = function
| x:float, y:float, x0, y0, i, maxDepth when x*x + y*y <= 4 && i <= maxDepth -> coordToMSetValue (x*x - y*y + x0, 2.0*x*y + y0, x0, y0, i+1, maxDepth)
| _, _, _, _, i, maxDepth when i >= maxDepth -> 256
| _, _, _, _, i, maxDepth -> int <| System.Math.Round (scale (float i) (float maxDepth) 255.0)


let mSetValueToPixelTuple = function
| x when x = 256 -> (255uy, 255uy, 255uy)
| x when x > 127 -> (byte x, 255uy, byte x)
| x -> (0uy, byte x, 0uy)


let pixelTupleToPixel (r, g, b) = new Rgb24(r, g, b)


let nToWrittenPixel n imageSideLen windowSideLen x0 y0 maxDepth (image : Image<Rgb24>) =
    let x = n % imageSideLen
    let y = n / imageSideLen
    let pixel = (0.0, 0.0, ((scale (float x) (float imageSideLen) (windowSideLen)) + (x0)), ((scale (float y) (float imageSideLen) (windowSideLen)) + (y0)), 0, maxDepth) |> (coordToMSetValue >> mSetValueToPixelTuple >> pixelTupleToPixel)
    image[x, y] <- pixel


let nToWrittenPixelAsync n imageSideLen windowSideLen x0 y0 maxDepth (image : Image<Rgb24>) =
    task {
        let x = n % imageSideLen
        let y = n / imageSideLen
        let pixel = (0.0, 0.0, ((scale (float x) (float imageSideLen) (windowSideLen)) + (x0)), ((scale (float y) (float imageSideLen) (windowSideLen)) + (y0)), 0, maxDepth) |> (coordToMSetValue >> mSetValueToPixelTuple >> pixelTupleToPixel)
        image[x, y] <- pixel
    }


let dimsToMSetImage imageSideLen windowSideLen x0 y0 maxDepth async =
    using (new Image<Rgb24>(imageSideLen, imageSideLen)) (fun image ->
        match async with
        | true ->
            [0..(imageSideLen * imageSideLen - 1)]
            |> Seq.map (fun n -> nToWrittenPixelAsync n imageSideLen windowSideLen x0 y0 maxDepth image)
            |> Task.WhenAll
            |> Task.WaitAll
        | false ->
            [0..(imageSideLen * imageSideLen - 1)]
            |> Seq.iter (fun n -> nToWrittenPixel n imageSideLen windowSideLen x0 y0 maxDepth image)
        using (File.Open("mandelbrot.jpg", FileMode.OpenOrCreate)) (fun fileStream ->
            image.SaveAsJpeg fileStream))


[<EntryPoint>]
let main argv =
    let errorHandler = ProcessExiter(colorizer = function ErrorCode.HelpText -> None | _ -> Some System.ConsoleColor.Red)
    let parser = ArgumentParser.Create<CliArguments>(errorHandler = errorHandler)
    let results = parser.Parse argv
    let imageSideLen = results.GetResult (ImageSideLength, 1024)
    let windowSideLen = results.GetResult (WindowSideLength, 3.0)
    let x = results.GetResult (X, -0.5)
    let y = results.GetResult (Y, 0.0)
    let maxDepth = results.GetResult (MaxDepth, 50)
    let async = results.Contains Async
    let silent = results.Contains Silent
    let stopwatch = Stopwatch.StartNew()
    let x0 = x - (windowSideLen / 2.0)
    let y0 = y - (windowSideLen / 2.0)
    if not silent then
        printfn "Rendering image of size %d x %d with window size %.2f centered at (%.2f, %.2f) and origin at (%.2f, %.2f) with max depth %d." imageSideLen imageSideLen windowSideLen x y x0 y0 maxDepth
    dimsToMSetImage imageSideLen windowSideLen x0 y0 maxDepth async
    stopwatch.Stop()
    let time = stopwatch.Elapsed.TotalMilliseconds
    if not silent then
        printfn "Image rendered in %.2f ms." time
    0
