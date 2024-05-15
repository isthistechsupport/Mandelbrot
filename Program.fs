#if INTERACTIVE
#r "nuget: Suave"
#r "nuget: SixLabors.ImageSharp"
#endif
open Suave
open Suave.Filters
open Suave.Writers
open Suave.Operators
open Suave.Successful
open Suave.Utils.Collections
open System
open System.Threading
open Mandelbrot


let parseIntOption = function
| Some (s:string) ->
    match Int32.TryParse s with
    | true, i -> Some i
    | _ -> None
| _ -> None


let parseFloatOption = function
| Some (s:string) ->
    match Double.TryParse s with
    | true, f -> Some f
    | _ -> None
| _ -> None


let renderImage q =
    let imageSideLen = (q ^^ "i") |> Option.ofChoice |> parseIntOption |> Option.defaultValue 1024
    let windowSideLen = (q ^^ "w") |> Option.ofChoice |> parseFloatOption |> Option.defaultValue 3.0
    let x = (q ^^ "x") |> Option.ofChoice |> parseFloatOption |> Option.defaultValue -0.5
    let y = (q ^^ "y") |> Option.ofChoice |> parseFloatOption |> Option.defaultValue 0.0
    let maxDepth = (q ^^ "d") |> Option.ofChoice |> parseIntOption |> Option.defaultValue 50
    Mandelbrot.dimsToMSetByteArray (imageSideLen, windowSideLen, x, y, maxDepth)


let mimeTypes =
  defaultMimeTypesMap
    @@ (function | ".webp" -> createMimeType "image/webp" true | _ -> None)


let joinFormData (req: HttpRequest) =
    [for value in req.multiPartFields do
        yield fst value, snd value |> Some]
    |> List.append req.form


let app =
    choose [
            GET >=> choose [
                path "/" >=>
                    Files.file "index.html"
                path "/script.js" >=>
                    Files.file "script.js"
                path "/styles.css" >=>
                    Files.file "styles.css"
                path "/favicon.ico" >=>
                    Files.file "favicon.ico"
                path "/mandelbrot.webp" >=>
                    Files.file "mandelbrot.webp" >=> setHeader "Content-Type" "image/webp"
                RequestErrors.NOT_FOUND "Page not found."]
            POST >=> choose [
                path "/api/render" >=>
                    addHeader "Content-Type" "image/webp" >=> request (fun (r: HttpRequest) -> ok (r |> joinFormData |> renderImage))
                RequestErrors.BAD_REQUEST "Bad request"]
            OPTIONS >=> 
                request (fun r ->
                    setHeader "Allow" "GET, POST"
                    >=> setHeader "Access-Control-Allow-Methods" "GET, POST"
                    >=> setHeader "Access-Control-Allow-Origin" "*"
                    >=> setHeader "Access-Control-Allow-Headers" "Content-Type"
                    >=> OK "")
            RequestErrors.BAD_REQUEST "Bad request"]


let printRequestInfo (ctx: HttpContext) =
    ctx |> logFormat |> printfn "%s"
    succeed ctx


[<EntryPoint>]
let main _ =
    let cts = new CancellationTokenSource()
    let conf = { defaultConfig with cancellationToken = cts.Token; mimeTypesMap = mimeTypes}
    let _, server = startWebServerAsync conf (app >=> fun ctx -> ctx |> logFormat |> printfn "%s"; succeed ctx)
        
    Async.Start(server, cts.Token)
    printfn "Make requests now"
    Console.ReadKey true |> ignore
        
    cts.Cancel()
    0
