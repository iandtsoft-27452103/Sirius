[<AutoOpen>]
module IO
open System.IO
let ReadRecords(str_file_name: string):Record[] =
    let mutable records:Record[] = Array.empty
    lines<-File.ReadAllLines(str_file_name)
    for line in lines do
        let mutable r = Record()
        r.str_moves <- Array.empty
        let l = line.Split([|','|])
        if l.[0] = "B" then
            r.winner <- uint8(0)
        else if l.[0] = "W" then
            r.winner <- uint8(1)
        else
            r.winner <- uint8(2)
        for i = 2 to l.Length - 1 do
            r.str_moves <- Array.append r.str_moves [|l.[i]|]
        r.ply <- uint16(r.str_moves.Length)
        records <- Array.append records [|r|]
    records

type TestData =
    struct
        val mutable comments:string
        val mutable rets: string
    end

let ReadTestFile(str_file_name: string):TestData[] = 
    let mutable data:TestData[] = Array.empty
    lines<-File.ReadAllLines(str_file_name)
    let mutable flag = 0
    let mutable d = TestData()
    for line in lines do
        if flag = 0 then
            d <- TestData()
            d.comments <- line
        else
            d.rets <- line
            data <- Array.append data[|d|]
        flag <- flag ^^^ 1
    data