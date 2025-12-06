// For more information see https://aka.ms/fsharp-console-apps
// printfn "Hello from F#"
open System.Numerics

[<EntryPoint>]
let main argv =
    if argv.Length > 0 then
        printfn "Hello, %s!" argv.[0]
    else
        printfn "Hello, world!"
    0 // プログラムの終了コード