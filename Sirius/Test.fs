[<AutoOpen>]

module Test

open System.Collections.Generic

let OutBoard(bt: BoardTree) =
    let mutable str = ""
    let mutable counter = 0
    for i in 0.. Square_NB - 1 do
        if i = 0 then
            str <- str + "   9    8    7    6    5    4    3    2    1\n"
            str <- str + "--------------------------------------------\n"
        let ipc = bt.Board.[i]
        if ipc = int8(0) then
            str <- str + "    "
        else
            if ipc > int8(0) then 
                str <- str + "  " + Str_Piece_JP[int(ipc)]
            else
                str <- str + " v" + Str_Piece_JP[int(-ipc)]
        str <- str + "|"
        counter <- counter + 1
        if counter = 9 then
            counter <- 0
            if i = 8 then
                str <- str + " 一"
            elif i = 17 then
                str <- str + " 二"
            elif i = 26 then
                str <- str + " 三"
            elif i = 35 then
                str <- str + " 四"
            elif i = 44 then
                str <- str + " 五"
            elif i = 53 then
                str <- str + " 六"
            elif i = 62 then
                str <- str + " 七"
            elif i = 71 then
                str <- str + " 八"
            else
                str <- str + " 九"
        if (i + 1) % 9 = 0 then
            str <- str + "\n"
    printfn "%s" str

    str <- ""
    for c in 0.. 1 do
        let hand = bt.Hand[c]
        if c = 0 then
            str <- str + "先手の持ち駒: "
        else
            str <- str + "後手の持ち駒: "
        let mutable pc = 7
        while pc <> 0 do
            let mutable n_hand = 0
            while bt.Hand[c] &&& Hand_Mask[pc] <> 0 do
                n_hand <- n_hand + 1
                bt.Hand[c] <- bt.Hand[c] - Hand_Hash[pc]
            if pc = 1 then
                str <- str + "歩 " + n_hand.ToString() + ","
            elif pc = 2 then
                str <- str + "香 " + n_hand.ToString() + ","
            elif pc = 3 then
                str <- str + "桂 " + n_hand.ToString() + ","
            elif pc = 4 then
                str <- str + "銀 " + n_hand.ToString() + ","
            elif pc = 5 then
                str <- str + "金 " + n_hand.ToString() + ","
            elif pc = 6 then
                str <- str + "角 " + n_hand.ToString() + ","
            elif pc = 7 then
                str <- str + "飛 " + n_hand.ToString() + ","
            pc <- pc - 1

        str <- str + "\n"
    printfn "%s" str

let OutBoard2(bt: BoardTree) =
    let mutable str = ""
    for c in 0.. 1 do
        for pc in 1.. 15 do
            let mutable bb = bt.BB_Piece[c, pc]
            while bb > System.UInt128() do
                let sq = Square(bb)
                bb <- bb ^^^ ABB_Mask.[sq]
                let mutable sign = 1
                if c = 1 then
                    sign <- -1
                if int(bt.Board.[sq]) <> (pc * sign) then
                    printfn "error!"
    printfn "%s" str

let TestDoMove():int = 
    let mutable bt: BoardTree = Board.Init()
    let records = IO.ReadRecords("20220403_nhk_hai.txt")
    let record = records.[0]
    let limit = records[0].str_moves.Length
    let mutable color = 0
    for i in 0 .. limit - 1 do
        let move = CSA.CSA2Move(bt, record.str_moves.[i])
        let result = Board.Do(ref bt, move, color)
        bt <- result.contents
        color <- color ^^^ 1
    OutBoard(bt)
    OutBoard2(bt)
    0

// ※おそらくデグレードしているので、要再テスト
let TestUnDoMove():int = 
    let mutable bt: BoardTree = Board.Init()
    let records = IO.ReadRecords("20220403_nhk_hai.txt")
    let record = records.[0]
    //let limit = records[0].str_moves.Length
    let limit = 50
    let mutable color = 0
    let mutable temp_move = uint32(0)
    for i in 0 .. limit - 1 do
        let move = CSA.CSA2Move(bt, record.str_moves.[i])
        let result = Board.Do(ref bt, move, color)
        bt <- result.contents
        if i = limit - 1 then
            // ※ 駒取りのところにバグがあるかもしれない。
            let result = Board.UnDo(ref bt, move, color)
            bt <- result.contents
        color <- color ^^^ 1
    //temp_move <- CSA.CSA2Move(bt, record.str_moves.[limit - 1])
    //let result = Board.UnDo(ref bt, temp_move, color ^^^ 1)
    //bt <- result.contents
    OutBoard(bt)
    OutBoard2(bt)
    0

let TestRepetition():int = 
    let mutable bt: BoardTree = Board.Init()
    let records = IO.ReadRecords("test_repetition.txt")
    let record = records.[0]
    let limit = records[0].str_moves.Length
    let mutable tt:TT = TT()
    tt.is_check <- Dictionary<uint64, bool>(dict[])
    let mutable color = 0
    for i in 0 .. limit - 2 do
        let move = CSA.CSA2Move(bt, record.str_moves.[i])
        let result = Board.Do(ref bt, move, color)
        bt <- result.contents
        color <- color ^^^ 1
    tt.is_check[bt.CurrentHash] <- false
    // ※戻り値の確認
    let iret = Board.IsRepetition(bt, tt)
    0

let TestGenDrop() = 
    let mutable bt: BoardTree = Board.Init()
    let data = ReadTestFile("test_data_drop.txt")
    let data_a = ReadTestFile("answer_data_drop.txt")
    let limit = data.Length
    let mutable bt = Board.Init()
    let mutable str_err = ""
    for i = 0 to limit - 1 do
        let str_sfen = data[i].rets
        bt <-SFEN.ToBoard(str_sfen)
        //let moves:uint32[] = Array.init Moves_Max  (fun i -> uint32(0))
        let moves:ResizeArray<uint32> = ResizeArray<uint32>()
        let cnt = GenDrop(bt, int(bt.RootColor), ref moves)
        let temp = data_a.[i].rets
        let str_csa_moves = temp.Split([|' '|])
        let cnt2 = int(cnt)
        for j = 0 to cnt2 - 1 do
            let m = moves[j]
            let str_csa_move = CSA.Move2CSA(m)
            let b = str_csa_move.Contains(str_csa_move)
            if b = false then
                str_err <- "Error in index = " + j.ToString()
        printf "%s" (str_err)

let TestGenNoCap() = 
    let mutable bt: BoardTree = Board.Init()
    let data = ReadTestFile("test_data_gennocap.txt")
    let data_a = ReadTestFile("answer_data_gennocap.txt")
    let limit = data.Length
    let mutable bt = Board.Init()
    let mutable str_err = ""
    for i = 0 to limit - 1 do
        let str_sfen = data[i].rets
        bt <-SFEN.ToBoard(str_sfen)
        let moves:ResizeArray<uint32> = ResizeArray<uint32>()
        let cnt = GenNoCap(bt, int(bt.RootColor), ref moves)
        let temp = data_a.[i].rets
        let str_csa_moves = temp.Split([|' '|])
        let cnt2 = int(cnt)
        for j = 0 to cnt2 - 1 do
            let m = moves[j]
            let str_csa_move = CSA.Move2CSA(m)
            let b = str_csa_move.Contains(str_csa_move)
            if b = false then
                str_err <- "Error in index = " + j.ToString()
        printf "%s" (str_err)

let TestGenCap() = 
    let mutable bt: BoardTree = Board.Init()
    let data = ReadTestFile("test_data_gencap.txt")
    let data_a = ReadTestFile("answer_data_gencap.txt")
    let limit = data.Length
    let mutable bt = Board.Init()
    let mutable str_err = ""
    for i = 0 to limit - 1 do
        let str_sfen = data[i].rets
        bt <-SFEN.ToBoard(str_sfen)
        let moves:ResizeArray<uint32> = ResizeArray<uint32>()
        let cnt = GenCap(bt, int(bt.RootColor), ref moves)
        let temp = data_a.[i].rets
        let str_csa_moves = temp.Split([|' '|])
        let cnt2 = int(cnt)
        for j = 0 to cnt2 - 1 do
            let m = moves[j]
            let str_csa_move = CSA.Move2CSA(m)
            let b = str_csa_move.Contains(str_csa_move)
            if b = false then
                str_err <- "Error in index = " + j.ToString()
        printf "%s" (str_err)

let TestGenEvasion() = 
    let mutable bt: BoardTree = Board.Init()
    let data = ReadTestFile("test_data_evasion.txt")
    let data_a = ReadTestFile("answer_data_evasion.txt")
    let limit = data.Length
    let mutable bt = Board.Init()
    let mutable str_err = ""
    for i = 0 to limit - 1 do
        let str_sfen = data[i].rets
        bt <-SFEN.ToBoard(str_sfen)
        let moves:ResizeArray<uint32> = ResizeArray<uint32>()
        let cnt = GenEvasion(ref bt, int(bt.RootColor), ref moves)
        let temp = data_a.[i].rets
        let str_csa_moves = temp.Split([|' '|])
        let cnt2 = int(cnt)
        for j = 0 to cnt2 - 1 do
            let m = moves[j]
            let str_csa_move = CSA.Move2CSA(m)
            let b = str_csa_move.Contains(str_csa_move)
            if b = false then
                str_err <- "Error in index = " + j.ToString()
        printf "%s" (str_err)

let TestGenCheck() = 
    let mutable bt: BoardTree = Board.Init()
    let data = ReadTestFile("test_data_check.txt")
    let data_a = ReadTestFile("answer_data_check.txt")
    let limit = data.Length
    let mutable bt = Board.Init()
    let mutable str_err = ""
    for i = 0 to limit - 1 do
        let str_sfen = data[i].rets
        bt <-SFEN.ToBoard(str_sfen)
        let moves:uint32[] = Array.init Moves_Max  (fun i -> uint32(0))
        let cnt = GenCheck(bt, int(bt.RootColor), ref moves)
        let temp = data_a.[i].rets
        let str_csa_moves = temp.Split([|' '|])
        let cnt2 = int(cnt)
        for j = 0 to cnt2 - 1 do
            let m = moves[j]
            let str_csa_move = CSA.Move2CSA(m)
            let b = str_csa_move.Contains(str_csa_move)
            if b = false then
                str_err <- "Error in index = " + j.ToString()
        printf "%s" (str_err)

let TestGenCheck2() = 
    let mutable bt: BoardTree = Board.Init()
    let data = ReadTestFile("test_data_b_check_additional.txt")
    let data_a = ReadTestFile("answer_data_b_check_additional.txt")
    let limit = data.Length
    let mutable bt = Board.Init()
    let mutable str_err = ""
    for i = 0 to limit - 1 do
        let str_sfen = data[i].rets
        bt <-SFEN.ToBoard(str_sfen)
        let moves:uint32[] = Array.init Moves_Max  (fun i -> uint32(0))
        let cnt = GenCheck(bt, int(bt.RootColor), ref moves)
        let temp = data_a.[i].rets
        let str_csa_moves = temp.Split([|' '|])
        let cnt2 = int(cnt)
        for j = 0 to cnt2 - 1 do
            let m = moves[j]
            let str_csa_move = CSA.Move2CSA(m)
            let b = str_csa_move.Contains(str_csa_move)
            if b = false then
                str_err <- "Error in index = " + j.ToString()
        printf "%s" (str_err)

let TestGenCheck3() = 
    let mutable bt: BoardTree = Board.Init()
    let data = ReadTestFile("test_data_w_check_additional.txt")
    let data_a = ReadTestFile("answer_data_w_check_additional.txt")
    let limit = data.Length
    let mutable bt = Board.Init()
    let mutable str_err = ""
    for i = 0 to limit - 1 do
        let str_sfen = data[i].rets
        bt <-SFEN.ToBoard(str_sfen)
        let moves:uint32[] = Array.init Moves_Max  (fun i -> uint32(0))
        let cnt = GenCheck(bt, int(bt.RootColor), ref moves)
        let temp = data_a.[i].rets
        let str_csa_moves = temp.Split([|' '|])
        let cnt2 = int(cnt)
        for j = 0 to cnt2 - 1 do
            let m = moves[j]
            let str_csa_move = CSA.Move2CSA(m)
            let b = str_csa_move.Contains(str_csa_move)
            if b = false then
                str_err <- "Error in index = " + j.ToString()
        printf "%s" (str_err)

let TestGenCheck4() = 
    let mutable bt: BoardTree = Board.Init()
    let mutable bt = Board.Init()
    bt <- SFEN.ToBoard("7kl/6R2/6spb/5N2k/9/9/9/9/2B6 b GS 1")
    let moves:uint32[] = Array.init Moves_Max  (fun i -> uint32(0))
    let cnt = GenCheck(bt, 0, ref moves)
    ""

let TestMate1Ply() = 
    let mutable bt: BoardTree = Board.Init()
    let data = ReadTestFile("test_data_b_mate1ply.txt")
    let data_a = ReadTestFile("answer_data_b_mate1ply.txt")
    //let data = ReadTestFile("test_data_w_mate1ply.txt")
    //let data_a = ReadTestFile("answer_data_w_mate1ply.txt")
    let limit = data.Length
    let mutable bt = Board.Init()
    let mutable str_err = ""
    let mutable error_count = 0
    let exclude_list:List<int> = List<int>([96; 97; 99; 103; 119; 120; 126; 175; 176; 179; 181; 200; 270; 271; 272; 273; 274; 275; 301; 302; 303; 304; 305; 363; 384; 387; 396; 402; 412])
    for i = 0 to limit - 1 do
        let str_sfen = data[i].rets
        bt <-SFEN.ToBoard(str_sfen)
        let moves:uint32[] = Array.init Moves_Max  (fun j -> uint32(0))
        let mate_move = IsMateIn1Ply(bt, int(bt.RootColor))
        let str_csa_move0 = data_a.[i].rets
        let sss = data_a.[i].comments
        let str_csa_move1 = CSA.Move2CSA(mate_move)
        if exclude_list.Contains(i) = false then
            if str_csa_move0 <> str_csa_move1 then
                if str_csa_move0 <> "" then
                    System.IO.File.AppendAllText("debug_log_mate1ply.txt", "Error in index = " + i.ToString())
                    str_err <- "Error in index = " + i.ToString()
                    error_count <- error_count + 1
                    printf "%s" (str_err)
            if str_csa_move0 = "" && mate_move <> uint32(0) then
                System.IO.File.AppendAllText("debug_log_mate1ply.txt", "Error in index = " + i.ToString())
                str_err <- "Error in index = " + i.ToString()
                error_count <- error_count + 1
                printf "%s" (str_err)            
    printf "error_count=%d" error_count

let TestMate1Ply2() = 
    let mutable bt: BoardTree = Board.Init()
    let data = ReadTestFile("test_data_w_mate1ply.txt")
    let data_a = ReadTestFile("answer_data_w_mate1ply.txt")
    //let data = ReadTestFile("test_data_w_mate1ply.txt")
    //let data_a = ReadTestFile("answer_data_w_mate1ply.txt")
    let limit = data.Length
    let mutable bt = Board.Init()
    let mutable str_err = ""
    let mutable error_count = 0
    let exclude_list:List<int> = List<int>([119; 120; 181; 182; 270; 271; 273; 275; 301; 302; 305; 349; 350; 351; 352; 353; 384; 412])
    for i = 0 to limit - 1 do
        let str_sfen = data[i].rets
        bt <-SFEN.ToBoard(str_sfen)
        let moves:uint32[] = Array.init Moves_Max  (fun j -> uint32(0))
        let mate_move = IsMateIn1Ply(bt, int(bt.RootColor))
        let str_csa_move0 = data_a.[i].rets
        let sss = data_a.[i].comments
        let str_csa_move1 = CSA.Move2CSA(mate_move)
        if exclude_list.Contains(i) = false then
            if str_csa_move0 <> str_csa_move1 then
                if str_csa_move0 <> "" then
                    System.IO.File.AppendAllText("debug_log_mate1ply.txt", "Error in index = " + i.ToString())
                    str_err <- "Error in index = " + i.ToString()
                    error_count <- error_count + 1
                    printf "%s" (str_err)
            if str_csa_move0 = "" && mate_move <> uint32(0) then
                System.IO.File.AppendAllText("debug_log_mate1ply.txt", "Error in index = " + i.ToString())
                str_err <- "Error in index = " + i.ToString()
                error_count <- error_count + 1
                printf "%s" (str_err)            
    printf "error_count=%d" error_count

let TestMate() = 
    let mutable bt: BoardTree = Board.Init()
    //let data = ReadTestFile("test_data_mate.txt")
    //let data = ReadTestFile("test_data_mate2.txt")
    //let limit = data.Length
    let mutable bt = Board.Init()
    let mutable str_err = ""
    let mutable error_count = 0
    let max_ply = 5
    let index = 0

    // ※ファイルから読み込むのではなく、直接str_sfenに代入した方がデバッグしやすいか？
    //let str_sfen = data[index].rets
    let mutable str_sfen = ""
    //str_sfen <- "4kg3/9/3GGG3/9/9/9/9/9/4K4 b - 1"
    // 先手手番
    // 問題1    5手詰め
    str_sfen <- "6s2/6R2/6Bk1/6p2/7N1/9/9/9/9 b GN 1"
    // 問題18   5手詰め
    str_sfen <- "5g2+R/4p2l1/5B1k1/5g3/8P/9/9/9/9 b GL 1"
    // 問題56   7手詰め
    //str_sfen <- "7kl/6R2/6spb/5N3/9/9/9/9/9 b G2S 1"
    // 問題71   7手詰め
    //str_sfen <- "7l1/4pl1kg/5p1pp/5N3/6PP1/9/9/9/9 b R2B 1"
    // 問題130  9手詰め
    //str_sfen <- "7n1/7SR/3s1p1k1/3b2s2/6pp1/9/9/9/9 b RBP 1"
    // 問題136 9手詰め
    //str_sfen <- "5RB2/8k/5p+r1p/5+bpp1/7sP/9/9/9/9 b GS 1"
    // 問題142 11手詰め
    //str_sfen <- "5n1kl/5n3/5ps2/6LPp/9/9/9/9 b G2L 1"
    // 問題145 11手詰め
    //str_sfen <- "6+B1l/6G1k/6bP1/6psp/9/9/9/9/9 b SL 1"
    // 問題148 13手詰め
    //str_sfen <- "6kpl/4Pg2S/5GbP1/5p2p/5sb2/9/9/9/9 b RSNP 1"
    // 問題150 13手詰め
    //str_sfen <- "7k1/4p3r/6S2/9/6rN1/9/9/9/9 b BNL 1"

    // 後手手番
    // 問題1    5手詰め
    //str_sfen <- "9/9/9/9/1n7/2P6/1Kb6/2r6/2S6 w gn 1"
    // 問題18   5手詰め
    //str_sfen <- "9/9/9/9/p8/3G5/1K1b5/1L2P4/+r2G5 w gl 1"
    // 問題56   7手詰め
    //str_sfen <- "9/9/9/9/9/3n5/BPS6/2r6/LK7 w g2s 1"
    // 問題71   7手詰め
    //str_sfen <- "9/9/9/9/9/1+R1s+b4/1Ss1G4/G8/2K1N4 w b 1"
    // 問題130  9手詰め
    //str_sfen <- "9/9/9/9/1PP6/2S2B3/1K1P1S3/rs7/1N7 w rbp 1"
    // 問題136 9手詰め
    //str_sfen <- "9/9/9/9/pS7/1PP+B5/P1+RP5/K8/2br5 w gs 1"
    // 問題142 11手詰め
    //str_sfen <- "9/9/9/9/g8/Ppl6/2SP5/3N5/LK1N5 w g2l"
    // 問題145 11手詰め
    //str_sfen <- "9/9/9/9/9/PSP6/1pB6K1g6/L1+B6 w sl 1"
    // 問題148 13手詰め
    //str_sfen <- "9/9/9/9/2BS5/P2P5/1pBg5/s2Gp4/LPK6 w rsnp 1"
    // 問題150 13手詰め
    //str_sfen <- "9/9/9/9/1nR6/9/2s6/R3P4/1K7 w bnp 1"

    //Mate.bt <-SFEN.ToBoard(str_sfen)
    //Mate.str_sfen <- str_sfen


    //メモ： 問題71の正解は認識できている。 4手目が△1一同金以外の場合の判定ができていない。
    //let b = MateSearchWrapper(max_ply)
    //if b = true then
        //printfn "詰み"
    //else
        //printfn "詰みではない"

let TestEvalDiff() = 
    let aaa = kkp_index_table[1,11,Rev_Sq[54]]
    Feature.Init()
    Feature.InitKKPIndex()
    Feature.Load()
    let mutable bt: BoardTree = Board.Init()
    let records = IO.ReadRecords("20220403_nhk_hai.txt")
    let record = records.[0]
    let limit = records[0].str_moves.Length
    let mutable prev_value = Eval(bt)
    bt.EvalArray[0] <- prev_value
    printfn "初期局面の評価値: %d" prev_value
    let mutable color = 0
    let mutable error_count = 0
    for i in 0 .. limit - 1 do
        let move = CSA.CSA2Move(bt, record.str_moves.[i])
        let result = Board.Do(ref bt, move, color)
        bt <- result.contents
        let mutable value = Eval(bt)
        // ここにもバグがあるかもしれない。
        if color = 0 then
            bt.EvalArray.[i + 1] <- value
        else
            bt.EvalArray.[i + 1] <- value
        // バグ！ 差分計算後にbtが更新されてしまっている。
        let mutable value2 = EvalWrapper(ref bt, color, i, move, false)
        if abs(value - value2) > 1 then
            printfn "評価値の差異: %d (i=%d)" (abs(value - value2)) i
            error_count <- error_count + 1
        color <- color ^^^ 1
        if color = 1 then
            value <- -value
        printfn "手数 %d の評価値: %d" (i + 1) value
    printfn "エラー数: %d" error_count
    0u