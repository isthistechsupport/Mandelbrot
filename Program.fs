﻿#if INTERACTIVE
#r "nuget: Argu"
#r "nuget: SixLabors.ImageSharp"
#endif
open Argu
open System.IO
open SixLabors.ImageSharp
open SixLabors.ImageSharp.PixelFormats
open System.Diagnostics
open System.Threading.Tasks
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running
open BenchmarkDotNet.Jobs


type ThreadingType =
    | SyncThreading = 0
    | AsyncThreading = 1
    | TaskThreading = 2
    | ParallelThreading = 3


type CliArguments =
    | [<AltCommandLine("-i")>] ImageSideLength of pixels: int
    | [<AltCommandLine("-w")>] WindowSideLength of length: float
    | [<AltCommandLine("-x")>] X of value: float
    | [<AltCommandLine("-y")>] Y of value: float
    | [<AltCommandLine("-d")>] MaxDepth of value: int
    | [<AltCommandLine("-a")>] Multithreading of ThreadingType
    | [<AltCommandLine("-s")>] Silent
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | ImageSideLength _ -> "specify the size of the image in pixels (default 1024)."
            | WindowSideLength _ -> "specify the size of the render window (default 3.0)."
            | X _ -> "specify the x coordinate of the center of the render window (default -0.5)"
            | Y _ -> "specify the y coordinate of the center of the render window (default 0)"
            | MaxDepth _ -> "specify the maximum recursion depth (default 50)."
            | Multithreading _ -> "use asynchronous rendering. Options are: 0-Sync, 1-Async, 2-Task & 3-Parallel. Default is Sync."
            | Silent -> "suppress output."


let scale (x0:float) x1 y1 = (x0/x1) * y1


let coordToMandelbrotDepth (x: float, y: float, x0, y0, i, maxDepth) =
    let rec loop (x: float, y: float, i) =
        let x2, y2 = x*x, y*y
        match x2 + y2 <= 4.0 && i < maxDepth with
        | true -> loop (x2 - y2 + x0, 2.0*x*y + y0, i + 1)
        | false -> i, maxDepth
    loop (x, y, i)


let mandelbrotDepthToLightness (depth,  maxDepth) = scale (float depth) (float maxDepth) 1.0


let lightnessToPixelTuple = function
| lightness when lightness = 0.0 || lightness = 1.0 -> (0uy, 0uy, 0uy)
| lightness ->
    let hue = 210.0
    let saturation = 1.0
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
    let r, g, b = r' + m, g' + m, b' + m
    byte <| System.Math.Round(r * 255.0), byte <| System.Math.Round(g * 255.0), byte <| System.Math.Round(b * 255.0)


let pixelTupleToPixel (r, g, b) = new Rgb24(r, g, b)


let nToWrittenPixel n imageSideLen windowSideLen x0 y0 maxDepth (image : Image<Rgb24>) =
    let x = n % imageSideLen
    let y = n / imageSideLen
    let pixel = (0.0, 0.0, ((scale (float x) (float imageSideLen) (windowSideLen)) + (x0)), ((scale (float y) (float imageSideLen) (windowSideLen)) + (y0)), 0, maxDepth) |> (coordToMandelbrotDepth >> mandelbrotDepthToLightness >> lightnessToPixelTuple >> pixelTupleToPixel)
    image[x, y] <- pixel


let nToWrittenPixelAsyncTask n imageSideLen windowSideLen x0 y0 maxDepth (image : Image<Rgb24>) =
    task {
        nToWrittenPixel n imageSideLen windowSideLen x0 y0 maxDepth image
    }


let nToWrittenPixelAsync n imageSideLen windowSideLen x0 y0 maxDepth (image : Image<Rgb24>) =
    async {
        nToWrittenPixel n imageSideLen windowSideLen x0 y0 maxDepth image
    }



let dimsToMSetImage imageSideLen windowSideLen x0 y0 maxDepth (threading: ThreadingType) =
    using (new Image<Rgb24>(imageSideLen, imageSideLen)) (fun image ->
        match threading with
        | ThreadingType.ParallelThreading ->
            Parallel.ForEach ([0..(imageSideLen * imageSideLen - 1)], fun n -> nToWrittenPixel n imageSideLen windowSideLen x0 y0 maxDepth image)
            |> ignore
        | ThreadingType.TaskThreading ->
            [0..(imageSideLen * imageSideLen - 1)]
            |> Seq.map (fun n -> nToWrittenPixelAsyncTask n imageSideLen windowSideLen x0 y0 maxDepth image)
            |> Task.WhenAll
            |> Task.WaitAll
        | ThreadingType.AsyncThreading ->
            [0..(imageSideLen * imageSideLen - 1)]
            |> Seq.map (fun n -> nToWrittenPixelAsync n imageSideLen windowSideLen x0 y0 maxDepth image)
            |> Async.Parallel
            |> Async.RunSynchronously
            |> ignore
        | _ ->
            [0..(imageSideLen * imageSideLen - 1)]
            |> Seq.iter (fun n -> nToWrittenPixel n imageSideLen windowSideLen x0 y0 maxDepth image)
        using (File.Open("mandelbrot.jpg", FileMode.OpenOrCreate)) (fun fileStream ->
            image.SaveAsJpeg fileStream))


[<SimpleJob(RuntimeMoniker.Net80)>]
type Benchmarks() =
    [<Benchmark(Baseline = true)>]
    member _.RenderImageSync() =
        dimsToMSetImage 1024 3.0 (-2.0) (-1.5) 50 ThreadingType.SyncThreading

    [<Benchmark>]
    member _.RenderImageAsync() =
        dimsToMSetImage 1024 3.0 (-2.0) (-1.5) 50 ThreadingType.AsyncThreading

    [<Benchmark>]
    member _.RenderImageTask() =
        dimsToMSetImage 1024 3.0 (-2.0) (-1.5) 50 ThreadingType.TaskThreading

    [<Benchmark>]
    member _.RenderImageParallel() =
        dimsToMSetImage 1024 3.0 (-2.0) (-1.5) 50 ThreadingType.ParallelThreading


//[<EntryPoint>]
let main argv =
    let errorHandler = ProcessExiter(colorizer = function ErrorCode.HelpText -> None | _ -> Some System.ConsoleColor.Red)
    let parser = ArgumentParser.Create<CliArguments>(errorHandler = errorHandler)
    let results = parser.Parse argv
    let imageSideLen = results.GetResult (ImageSideLength, 1024)
    let windowSideLen = results.GetResult (WindowSideLength, 3.0)
    let x = results.GetResult (X, -0.5)
    let y = results.GetResult (Y, 0.0)
    let maxDepth = results.GetResult (MaxDepth, 50)
    let threading = results.GetResult (Multithreading, ThreadingType.SyncThreading)
    let silent = results.Contains Silent
    let stopwatch = Stopwatch.StartNew()
    let x0 = x - (windowSideLen / 2.0)
    let y0 = y - (windowSideLen / 2.0)
    if not silent then
        printfn "Rendering image of size %d x %d with window size %.2f centered at (%.2f, %.2f) and origin at (%.2f, %.2f) with max depth %d." imageSideLen imageSideLen windowSideLen x y x0 y0 maxDepth
    dimsToMSetImage imageSideLen windowSideLen x0 y0 maxDepth threading
    stopwatch.Stop()
    let time = stopwatch.Elapsed.TotalMilliseconds
    if not silent then
        printfn "Image rendered in %.2f ms." time
    0


[<EntryPoint>]
let benchmarker argv =
    BenchmarkSwitcher.FromAssembly(typeof<Benchmarks>.Assembly).Run(argv) |> ignore
    0
