[<AutoOpen>]
module SFEN

let ToSFEN(bt:BoardTree, color:Color):string = 
    let mutable str_piece:string = ""
    let mutable str_sfen:string = ""
    let mutable flag:bool = false
    let mutable i = 0
    let mutable empty_count = 0
    while i < Square_NB do
        let str_piece:string = Str_SFEN_Pc[int(bt.Board.[i])]
        if str_piece = "" then
            empty_count <- empty_count + 1
            flag <- true
        else
            if flag then
                str_sfen <- str_sfen + string empty_count
                empty_count <- 0
                flag <- false
            str_sfen <- str_sfen + str_piece
        if i <> Square_NB - 1 && FileTable[i] = File.File9 then
            if empty_count > 0 then
                flag <- false
                str_sfen <- str_sfen + string empty_count
                empty_count <- 0
            str_sfen <- str_sfen + "/"
        str_sfen <- str_sfen + Str_Color[color]
        i <- i + 1
    i <- 0
    let mutable j = 0
    let mutable k = 0
    while i < Color_NB do
        while j < Piece_NB do
            let num = (bt.Hand[i] &&& Hand_Mask[j]) >>> Hand_Rev_Bit[j]
            if num > 0 then
                if num > 1 then
                    k <- -Sign_Table[int(color)] * j;
                str_sfen <- str_sfen + Str_SFEN_Pc[k]
            else
                if num < 5 then
                    k <- -Sign_Table[int(color)] * j
                else
                    k <- -Sign_Table[int(color)]
                str_sfen <- str_sfen + num.ToString() + Str_SFEN_Pc[k]
            j <- j + 1
        i <- i + 1
    if bt.Hand[0] = 0 && bt.Hand[1] = 0 then
        str_sfen <- str_sfen + "-"
    str_sfen <- str_sfen + " 1"
    str_sfen

let ToBoard(str_sfen:string):BoardTree = 
    let mutable color = 0
    let mutable flag:bool = false
    let mutable bt:BoardTree = Board.Init()
    bt <- Board.Clear()
    let mutable int_pc = 0
    let mutable str_temp = str_sfen.Split([|' '|])
    let str_board:string = str_temp[0]
    let mutable limit = str_board.Length
    let mutable sq = 0
    let mutable i = 0
    let mutable empty_num = 0
    for i = 0 to limit - 1 do
        let s = str_board.Substring(i, 1)
        if s= "+" then
            flag <- true          
        else
            if Int_Empty_Num.ContainsKey(s) then
                empty_num <- Int_Empty_Num[s]
                for j = 0 to empty_num - 1 do
                    bt.Board.[sq] <- int8(Piece.Empty)
                    sq <- sq + 1
            else
                if Int_Pc.ContainsKey(s) then
                    int_pc <- Int_Pc[s]
                    if int_pc > 0 then
                        //int_pc <- Int_Pc[s]
                        if flag then
                            int_pc <- int_pc + Promote
                            flag <- false
                        bt.BB_Piece.[int(Color.Black), int_pc] <- bt.BB_Piece.[int(Color.Black), int_pc] ||| ABB_Mask[sq]
                        bt.BB_Occupied.[int(Color.Black)] <- bt.BB_Occupied.[int(Color.Black)] ||| ABB_Mask[sq]
                        if int_pc = int(Piece.King) then
                            bt.SQ_King.[int(Color.Black)] <- uint8(sq)
                    else
                        //int_pc <- Int_Pc[s]
                        if flag then
                            int_pc <- int_pc - Promote
                            flag <- false
                        bt.BB_Piece.[int(Color.White), -int_pc] <- bt.BB_Piece.[int(Color.White), -int_pc] ||| ABB_Mask[sq]
                        bt.BB_Occupied.[int(Color.White)] <- bt.BB_Occupied.[int(Color.White)] ||| ABB_Mask[sq]
                        if -int_pc = int(Piece.King) then
                            bt.SQ_King.[int(Color.White)] <- uint8(sq)
                    bt.Board.[sq] <- int8(int_pc)
                    sq <- sq + 1
    let str_color:string = str_temp[1]
    bt.RootColor <- Num_Color[str_color]
    let str_hand = str_temp[2]
    limit <- str_hand.Length
    flag <- false
    let mutable running:bool = true
    let mutable num = 1
    let mutable index = 0
    while index < limit do
        let s = str_hand.Substring(index, 1)
        if s = "-" then
            running <- false
        else if s = "1" && flag = false then
            flag <- true
        else
            if Int_Hand_Num.ContainsKey(s) then
                num <- Int_Hand_Num[s]
                flag <- true
            else
                int_pc <- Int_Pc[s]
                if int_pc > 0 then
                    color <- int(Color.Black)
                else
                    color <- int(Color.White)
                    int_pc <- -int_pc
                let mutable j = 0
                while j < num do
                    bt.Hand[color] <- bt.Hand[color] + Hand_Hash[int_pc]
                    j <- j + 1
                num <- 1
        index <- index + 1
    bt.CurrentHash <- HashFunc(bt)     
    bt.Hash[0] <- bt.PrevHash
    bt.Hash[1] <- bt.CurrentHash
    bt.Ply <- uint16(1)
    bt
