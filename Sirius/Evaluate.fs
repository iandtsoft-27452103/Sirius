[<AutoOpen>]
module Evaluate

type KKPSet = 
    struct
        val mutable value: int
        val mutable li0: List<int>
        val mutable li1: List<int>
        member this.Item with get (i: int) = this.li0.[i] + this.li1.[i]
    end

type PieceListSet = 
    struct
        val mutable li0: List<int>
        val mutable li1: List<int>
        member this.Item with get (i: int) = this.li0.[i] + this.li1.[i]
    end

let MakeListWithKKP(bt:BoardTree):KKPSet = 
    let mutable kkp_set: KKPSet = Unchecked.defaultof<KKPSet>
    kkp_set.li0 <- []
    kkp_set.li1 <- []
    let sq_bk0 = bt.SQ_King.[0]
    let sq_wk0 = bt.SQ_King.[1]
    let sq_bk1 = Rev_Sq[int(bt.SQ_King.[0])]
    let sq_wk1 = Rev_Sq[int(bt.SQ_King.[1])]
    let mutable kkp_score = 0
    let mutable n_hand = 0
    for c in 0 .. Color_NB - 1 do
        for pc in int(Piece.Pawn) .. int(Piece.Rook) do
            n_hand <- (bt.Hand[c] &&& Hand_Mask[pc]) >>> Hand_Rev_Bit[pc]
            for i in 0 .. n_hand - 1 do
                let index = kkp_hand_start_index[pc] + i
                if c = int(Color.Black) then
                    kkp_score <- kkp_score + int(fv_kkp[int(sq_bk0), int(sq_wk0), int(index)])
                else
                    kkp_score <- kkp_score - int(fv_kkp[int(sq_bk1), int(sq_wk1), int(index)])
                kkp_set.li0 <- kkp_set.li0 @ [pp_hand_start_index[c, pc] + i]
                kkp_set.li1 <- kkp_set.li1 @ [pp_hand_start_index[c ^^^ 1, pc] + i]
    for c in 0 .. Color_NB - 1 do
        for pc in int(Piece.Pawn) .. int(Piece.Dragon) do
            if pc <> int(Piece.None) && pc <> int(Piece.King) then
                let mutable bb = bt.BB_Piece.[c, pc]
                while bb > System.UInt128() do
                    let sq = Square(bb)
                    bb <- bb ^^^ ABB_Mask.[sq]
                    if c = int(Color.Black) then
                        kkp_score <- kkp_score + int(fv_kkp[int(sq_bk0), int(sq_wk0), kkp_index_table[c, pc, sq]])
                    else
                        kkp_score <- kkp_score - int(fv_kkp[int(sq_bk1), int(sq_wk1), kkp_index_table[c, pc, Rev_Sq[sq]]])
                    kkp_set.li0 <- kkp_set.li0 @ [pp_index_table[c, pc, sq]]
                    kkp_set.li1 <- kkp_set.li1 @ [pp_index_table[c ^^^ 1, pc, Rev_Sq[sq]]]
    kkp_set.value <- kkp_score
    kkp_set

let MakeList(bt:BoardTree):PieceListSet = 
    let mutable piece_list_set: PieceListSet = Unchecked.defaultof<PieceListSet>
    piece_list_set.li0 <- []
    piece_list_set.li1 <- []
    let mutable n_hand = 0
    for c in 0 .. Color_NB - 1 do
        for pc in int(Piece.Pawn) .. int(Piece.Rook) do
            n_hand <- (bt.Hand[c] &&& Hand_Mask[pc]) >>> Hand_Rev_Bit[pc]
            for i in 0 .. n_hand - 1 do
                let index = kkp_hand_start_index[pc] + i
                piece_list_set.li0 <- piece_list_set.li0 @ [pp_hand_start_index[c, pc] + i]
                piece_list_set.li1 <- piece_list_set.li1 @ [pp_hand_start_index[c ^^^ 1, pc] + i]
    for c in 0 .. Color_NB - 1 do
        for pc in int(Piece.Pawn) .. int(Piece.Dragon) do
            if pc <> int(Piece.None) && pc <> int(Piece.King) then
                let mutable bb = bt.BB_Piece.[c, pc]
                while bb > System.UInt128() do
                    let sq = Square(bb)
                    bb <- bb ^^^ ABB_Mask.[sq]
                    piece_list_set.li0 <- piece_list_set.li0 @ [pp_index_table[c, pc, sq]]
                    piece_list_set.li1 <- piece_list_set.li1 @ [pp_index_table[c ^^^ 1, pc, Rev_Sq[sq]]]
    piece_list_set

// btには手を指した後の局面が入っているものと仮定する。colorはdrop_moveを指した方の手番。
let CalcDiffDrop(bt:BoardTree ref, color:int, drop_move:uint32):int = 
    let piece_num = 38
    let mutable score = 0
    let drop_sq = int(Move.To(drop_move))
    let drop_piece = int(Move.PieceType(drop_move))
    let after_hand_num = ((bt.contents.Hand[color] &&& Hand_Mask[drop_piece]) >>> Hand_Rev_Bit[drop_piece])
    let mutable sq_bk = 0
    let mutable sq_wk = 0
    if color = int(Color.Black) then
        sq_bk <- int(bt.contents.SQ_King.[0])
        sq_wk <- int(bt.contents.SQ_King.[1])
    else
        sq_bk <- int(Rev_Sq.[int(bt.contents.SQ_King.[0])])
        sq_wk <- int(Rev_Sq.[int(bt.contents.SQ_King.[1])])
    // 手を指した後の枚数がn枚だったら、手を指す前の枚数はn + 1枚。
    let mutable index = kkp_hand_start_index[drop_piece] + after_hand_num
    //score <- score - int(fv_kkp[sq_bk, sq_wk, index])
    if color = int(Color.Black) then
        score <- score - int(fv_kkp[sq_bk, sq_wk, index])
        index <- kkp_index_table[color, drop_piece, drop_sq]
        score <- score + int(fv_kkp[sq_bk, sq_wk, index])
    else
        score <- score + int(fv_kkp[sq_bk, sq_wk, index])
        index <- kkp_index_table[color, drop_piece, Rev_Sq[drop_sq]]
        score <- score - int(fv_kkp[sq_bk, sq_wk, index])
    let piece_list_set0 = MakeList(bt.contents)// 1手指した後の駒リストが入っている。
    let mutable temp_bt = UnDo(ref bt.contents, drop_move, color)
    bt.contents <- temp_bt.contents
    let piece_list_set1 = MakeList(bt.contents)// 1手指す前の駒リストが入っている。
    sq_bk <- int(bt.contents.SQ_King.[0])
    sq_wk <- Rev_Sq[int(bt.contents.SQ_King.[1])]
    temp_bt <- Do(ref bt.contents, drop_move, color)
    bt.contents <- temp_bt.contents

    // 持ち駒の差分計算 => 1枚分減算する。
    index <- pp_hand_start_index[color, drop_piece] + after_hand_num
    let mutable pos = List.findIndex(fun x -> x = index) piece_list_set1.li0
    for s in 0.. pos do
        score <- score - int(fv_kpp[sq_bk, index, piece_list_set1.li0[s]])
    for s in pos + 1.. piece_num - 1 do
        score <- score - int(fv_kpp[sq_bk, piece_list_set1.li0[s],index])
    index <- pp_hand_start_index[color ^^^ 1, drop_piece] + after_hand_num
    pos <- List.findIndex(fun x -> x = index) piece_list_set1.li1
    for s in 0.. pos do
        score <- score + int(fv_kpp[sq_wk, index, piece_list_set1.li1[s]])
    for s in pos + 1.. piece_num - 1 do
        score <- score + int(fv_kpp[sq_wk, piece_list_set1.li1[s], index])

    // 打った位置の差分計算
    index <- pp_index_table[color, drop_piece, drop_sq]
    let index2 = pp_index_table[color ^^^ 1, drop_piece, Rev_Sq[drop_sq]]
    pos <- List.findIndex(fun x -> x = index) piece_list_set0.li0
    for s in 0.. pos do
        score <- score + int(fv_kpp[sq_bk, index, piece_list_set0.li0[s]])
    for s in pos + 1.. piece_num - 1 do
        score <- score + int(fv_kpp[sq_bk, piece_list_set0.li0[s], index])
    pos <- List.findIndex(fun x -> x = index2) piece_list_set0.li1
    for s in 0.. pos do
        score <- score - int(fv_kpp[sq_wk, index2, piece_list_set0.li1[s]])
    for s in pos + 1.. piece_num - 1 do
        score <- score - int(fv_kpp[sq_wk, piece_list_set0.li1[s], index2])
    score <- score / FV_SCALE
    score

let CalcDiffNoCapNoPro(bt:BoardTree ref, color:int, move:uint32):int = 
    let piece_num = 38
    let mutable score = 0
    let ifrom = int(Move.From(move))
    let ito = int(Move.To(move))
    let ipiece = int(Move.PieceType(move))
    let mutable sq_bk = 0
    let mutable sq_wk = 0
    if color = int(Color.Black) then
        sq_bk <- int(bt.contents.SQ_King.[0])
        sq_wk <- int(bt.contents.SQ_King.[1])
    else
        sq_bk <- int(Rev_Sq.[int(bt.contents.SQ_King.[0])])
        sq_wk <- int(Rev_Sq.[int(bt.contents.SQ_King.[1])])
    let mutable index = 0
    if color = 0 then
        index <- kkp_index_table[color, ipiece, ifrom]
        score <- score - int(fv_kkp[sq_bk, sq_wk, index])
        index <- kkp_index_table[color, ipiece, ito]
        score <- score + int(fv_kkp[sq_bk, sq_wk, index])
    else
        index <- kkp_index_table[color, ipiece, Rev_Sq[ifrom]]
        score <- score + int(fv_kkp[sq_bk, sq_wk, index])
        index <- kkp_index_table[color, ipiece, Rev_Sq[ito]]
        score <- score - int(fv_kkp[sq_bk, sq_wk, index])
    let piece_list_set0 = MakeList(bt.contents)// 1手指した後の駒リストが入っている。
    let mutable temp_bt = UnDo(ref bt.contents, move, color)
    bt.contents <- temp_bt.contents
    let piece_list_set1 = MakeList(bt.contents)// 1手指す前の駒リストが入っている。
    sq_bk <- int(bt.contents.SQ_King.[0])
    sq_wk <- Rev_Sq[int(bt.contents.SQ_King.[1])]
    temp_bt <- Do(ref bt.contents, move, color)
    bt.contents <- temp_bt.contents

    // 移動元の差分計算
    index <- pp_index_table[color, ipiece, ifrom]
    let mutable index2 = pp_index_table[color ^^^ 1, ipiece, Rev_Sq.[ifrom]]
    let mutable pos = List.findIndex(fun x -> x = index) piece_list_set1.li0
    for s in 0.. pos do
        score <- score - int(fv_kpp[sq_bk, index, piece_list_set1.li0[s]])
    for s in pos + 1.. piece_num - 1 do
        score <- score - int(fv_kpp[sq_bk, piece_list_set1.li0[s],index])
    pos <- List.findIndex(fun x -> x = index2) piece_list_set1.li1
    for s in 0.. pos do
        score <- score + int(fv_kpp[sq_wk, index2, piece_list_set1.li1[s]])
    for s in pos + 1.. piece_num - 1 do
        score <- score + int(fv_kpp[sq_wk, piece_list_set1.li1[s], index2])

    // 移動先の差分計算
    index <- pp_index_table[color, ipiece, ito]
    index2 <- pp_index_table[color ^^^ 1, ipiece, Rev_Sq.[ito]]
    pos <- List.findIndex(fun x -> x = index) piece_list_set0.li0
    for s in 0.. pos do
        score <- score + int(fv_kpp[sq_bk, index, piece_list_set0.li0[s]])
    for s in pos + 1.. piece_num - 1 do
        score <- score + int(fv_kpp[sq_bk, piece_list_set0.li0[s],index])
    pos <- List.findIndex(fun x -> x = index2) piece_list_set0.li1
    for s in 0.. pos do
        score <- score - int(fv_kpp[sq_wk, index2, piece_list_set0.li1[s]])
    for s in pos + 1.. piece_num - 1 do
        score <- score - int(fv_kpp[sq_wk, piece_list_set0.li1[s], index2])
    score <- score / FV_SCALE
    score

let CalcDiffNoCapPro(bt:BoardTree ref, color:int, move:uint32):int = 
    let piece_num = 38
    let mutable score = 0
    let ifrom = int(Move.From(move))
    let ito = int(Move.To(move))
    let mutable ipiece = int(Move.PieceType(move))
    let mutable sq_bk = 0
    let mutable sq_wk = 0
    if color = int(Color.Black) then
        sq_bk <- int(bt.contents.SQ_King.[0])
        sq_wk <- int(bt.contents.SQ_King.[1])
    else
        sq_bk <- int(Rev_Sq.[int(bt.contents.SQ_King.[0])])
        sq_wk <- int(Rev_Sq.[int(bt.contents.SQ_King.[1])])
    let mutable index = 0
    if color = 0 then
        index <- kkp_index_table[color, ipiece, ifrom]
        score <- score - int(fv_kkp[sq_bk, sq_wk, index])
        index <- kkp_index_table[color, ipiece + Promote, ito]
        score <- score + int(fv_kkp[sq_bk, sq_wk, index])
    else
        index <- kkp_index_table[color, ipiece, Rev_Sq[ifrom]]
        score <- score + int(fv_kkp[sq_bk, sq_wk, index])
        index <- kkp_index_table[color, ipiece + Promote, Rev_Sq[ito]]
        score <- score - int(fv_kkp[sq_bk, sq_wk, index])
    let piece_list_set0 = MakeList(bt.contents)// 1手指した後の駒リストが入っている。
    let mutable temp_bt = UnDo(ref bt.contents, move, color)
    bt.contents <- temp_bt.contents
    let piece_list_set1 = MakeList(temp_bt.contents)// 1手指す前の駒リストが入っている。
    sq_bk <- int(bt.contents.SQ_King.[0])
    sq_wk <- Rev_Sq[int(bt.contents.SQ_King.[1])]
    temp_bt <- Do(ref bt.contents, move, color)
    bt.contents <- temp_bt.contents

    // 移動元の差分計算
    index <- pp_index_table[color, ipiece, ifrom]
    let mutable index2 = pp_index_table[color ^^^ 1, ipiece, Rev_Sq.[ifrom]]
    let mutable pos = List.findIndex(fun x -> x = index) piece_list_set1.li0
    for s in 0.. pos do
        score <- score - int(fv_kpp[sq_bk, index, piece_list_set1.li0[s]])
    for s in pos + 1.. piece_num - 1 do
        score <- score - int(fv_kpp[sq_bk, piece_list_set1.li0[s],index])
    pos <- List.findIndex(fun x -> x = index2) piece_list_set1.li1
    for s in 0.. pos do
        score <- score + int(fv_kpp[sq_wk, index2, piece_list_set1.li1[s]])
    for s in pos + 1.. piece_num - 1 do
        score <- score + int(fv_kpp[sq_wk, piece_list_set1.li1[s], index2])

    ipiece <- ipiece + Promote

    // 移動先の差分計算
    index <- pp_index_table[color, ipiece, ito]
    index2 <- pp_index_table[color ^^^ 1, ipiece, Rev_Sq.[ito]]
    pos <- List.findIndex(fun x -> x = index) piece_list_set0.li0
    for s in 0.. pos do
        score <- score + int(fv_kpp[sq_bk, index, piece_list_set0.li0[s]])
    for s in pos + 1.. piece_num - 1 do
        score <- score + int(fv_kpp[sq_bk, piece_list_set0.li0[s],index])
    pos <- List.findIndex(fun x -> x = index2) piece_list_set0.li1
    for s in 0.. pos do
        score <- score - int(fv_kpp[sq_wk, index2, piece_list_set0.li1[s]])
    for s in pos + 1.. piece_num - 1 do
        score <- score - int(fv_kpp[sq_wk, piece_list_set0.li1[s], index2])
    score <- score / FV_SCALE
    score

let CalcDiffCapNoPro(bt:BoardTree ref, color:int, move:uint32):int = 
    let piece_num = 38
    let mutable score = 0
    let ifrom = int(Move.From(move))
    let ito = int(Move.To(move))
    let ipiece = int(Move.PieceType(move))
    let mutable icap_pc = int(Move.CapPiece(move))
    let mutable kkp_icap_pc = icap_pc
    if icap_pc > int(Piece.King) then
        kkp_icap_pc <- icap_pc - Promote
    let hand_num = (bt.contents.Hand.[color] &&& Hand_Mask.[kkp_icap_pc]) >>> Hand_Rev_Bit.[kkp_icap_pc]
    let mutable sq_bk = 0
    let mutable sq_wk = 0
    let mutable sq_bk2 = 0
    let mutable sq_wk2 = 0
    if color = int(Color.Black) then
        sq_bk <- int(bt.contents.SQ_King.[0])
        sq_wk <- int(bt.contents.SQ_King.[1])
        sq_bk2 <- int(Rev_Sq.[int(bt.contents.SQ_King.[0])])
        sq_wk2 <- int(Rev_Sq.[int(bt.contents.SQ_King.[1])])
    else
        sq_bk <- int(Rev_Sq.[int(bt.contents.SQ_King.[0])])
        sq_wk <- int(Rev_Sq.[int(bt.contents.SQ_King.[1])])
        sq_bk2 <- int(bt.contents.SQ_King.[0])
        sq_wk2 <- int(bt.contents.SQ_King.[1])
    let mutable index = 0
    if color = 0 then
        index <- kkp_index_table[color, ipiece, ifrom]
        score <- score - int(fv_kkp[sq_bk, sq_wk, index])
        index <- kkp_index_table[color, ipiece, ito]
        score <- score + int(fv_kkp[sq_bk, sq_wk, index])
        index <- kkp_index_table[color ^^^ 1, icap_pc, Rev_Sq[ito]]
        score <- score + int(fv_kkp[sq_bk2, sq_wk2, index])
        index <- kkp_hand_start_index[kkp_icap_pc] + hand_num - 1
        score <- score + int(fv_kkp[sq_bk, sq_wk, index])
    else
        index <- kkp_index_table[color, ipiece, Rev_Sq[ifrom]]
        score <- score + int(fv_kkp[sq_bk, sq_wk, index])
        index <- kkp_index_table[color, ipiece, Rev_Sq[ito]]
        score <- score - int(fv_kkp[sq_bk, sq_wk, index])
        index <- kkp_index_table[color ^^^ 1, icap_pc, ito]
        score <- score - int(fv_kkp[sq_bk2, sq_wk2, index])
        index <- kkp_hand_start_index[kkp_icap_pc] + hand_num - 1
        score <- score - int(fv_kkp[sq_bk, sq_wk, index])
    let piece_list_set0 = MakeList(bt.contents)// 1手指した後の駒リストが入っている。
    let mutable temp_bt = UnDo(ref bt.contents, move, color)
    bt.contents <- temp_bt.contents
    let piece_list_set1 = MakeList(temp_bt.contents)// 1手指す前の駒リストが入っている。
    sq_bk <- int(bt.contents.SQ_King.[0])
    sq_wk <- Rev_Sq[int(bt.contents.SQ_King.[1])]
    temp_bt <- Do(ref bt.contents, move, color)
    bt.contents <- temp_bt.contents

    let mutable counter = 0
    //printf "Samuel Gustavson\n"

    // 移動元の差分計算
    index <- pp_index_table[color, ipiece, ifrom]
    let mutable index2 = pp_index_table[color ^^^ 1, ipiece, Rev_Sq.[ifrom]]
    let mutable pos = List.findIndex(fun x -> x = index) piece_list_set1.li0
    for s in 0.. pos do
        score <- score - int(fv_kpp[sq_bk, index, piece_list_set1.li0[s]])
        //printfn "sq_bk=%d, l0=%d, l1=%d" sq_bk index piece_list_set1.li0[s]
        counter <- counter + 1
    for s in pos + 1.. piece_num - 1 do
        score <- score - int(fv_kpp[sq_bk, piece_list_set1.li0[s], index])
        //printfn "sq_bk=%d, l0=%d, l1=%d" sq_bk piece_list_set1.li0[s] index
        counter <- counter + 1
    pos <- List.findIndex(fun x -> x = index2) piece_list_set1.li1
    for s in 0.. pos do
        score <- score + int(fv_kpp[sq_wk, index2, piece_list_set1.li1[s]])
        //printfn "sq_wk=%d, l0=%d, l1=%d" sq_wk index2 piece_list_set1.li1[s]
        counter <- counter + 1
    for s in pos + 1.. piece_num - 1 do
        score <- score + int(fv_kpp[sq_wk, piece_list_set1.li1[s], index2])
        //printfn "sq_wk=%d, l0=%d, l1=%d" sq_wk piece_list_set1.li1[s] index2
        counter <- counter + 1
    let mutable index_from0 = index
    let mutable index_from1 = index2

    // 移動先の差分計算
    index <- pp_index_table[color, ipiece, ito]
    index2 <- pp_index_table[color ^^^ 1, ipiece, Rev_Sq.[ito]]
    pos <- List.findIndex(fun x -> x = index) piece_list_set0.li0
    for s in 0.. pos do
        score <- score + int(fv_kpp[sq_bk, index, piece_list_set0.li0[s]])
        //printfn "sq_bk=%d, l0=%d, l1=%d" sq_bk index piece_list_set0.li0[s]
        counter <- counter + 1
    for s in pos + 1.. piece_num - 1 do
        score <- score + int(fv_kpp[sq_bk, piece_list_set0.li0[s],index])
        //printfn "sq_bk=%d, l0=%d, l1=%d" sq_bk piece_list_set0.li0[s] index
        counter <- counter + 1
    pos <- List.findIndex(fun x -> x = index2) piece_list_set0.li1
    for s in 0.. pos do
        score <- score - int(fv_kpp[sq_wk, index2, piece_list_set0.li1[s]])
        //printfn "sq_wk=%d, l0=%d, l1=%d" sq_wk index2 piece_list_set0.li1[s]
        counter <- counter + 1
    for s in pos + 1.. piece_num - 1 do
        score <- score - int(fv_kpp[sq_wk, piece_list_set0.li1[s], index2])
        //printfn "sq_wk=%d, l0=%d, l1=%d" sq_wk piece_list_set0.li1[s] index2
        counter <- counter + 1
    let mutable index_to0 = index
    let mutable index_to1 = index2

    // 取られた駒の差分計算
    index <- pp_index_table[color ^^^ 1, icap_pc, ito]
    index2 <- pp_index_table[color, icap_pc, Rev_Sq[ito]]
    pos <- List.findIndex(fun x -> x = index) piece_list_set1.li0
    counter <- 0
    for s in 0.. pos do
        if piece_list_set1.li0[s] <> index_from0 then
            score <- score - int(fv_kpp[sq_bk, index, piece_list_set1.li0[s]])
            counter <- counter + 1
            //printfn "-sq_bk=%d, l0=%d, l1=%d" sq_bk index piece_list_set1.li0[s]
    for s in pos + 1.. piece_num - 1 do
        if piece_list_set1.li0[s] <> index_from0 then
            score <- score - int(fv_kpp[sq_bk, piece_list_set1.li0[s], index])
            counter <- counter + 1
            //printfn "-sq_bk=%d, l0=%d, l1=%d" sq_bk piece_list_set1.li0[s] index
    pos <- List.findIndex(fun x -> x = index2) piece_list_set1.li1
    for s in 0.. pos do
        if piece_list_set1.li1[s] <> index_from1 then
            score <- score + int(fv_kpp[sq_wk, index2, piece_list_set1.li1[s]])
            counter <- counter + 1
            //printfn "+sq_wk=%d, l0=%d, l1=%d" sq_wk index2 piece_list_set1.li1[s]
    for s in pos + 1.. piece_num - 1 do
        if piece_list_set1.li1[s] <> index_from1 then
            score <- score + int(fv_kpp[sq_wk, piece_list_set1.li1[s], index2])
            counter <- counter + 1
            //printfn "+sq_wk=%d, l0=%d, l1=%d" sq_wk piece_list_set1.li1[s] index2

    // 駒台の差分計算 => 1枚分加算する。
    index <- pp_hand_start_index[color, kkp_icap_pc] + hand_num - 1
    index2 <- pp_hand_start_index[color ^^^ 1, kkp_icap_pc] + hand_num - 1
    pos <- List.findIndex(fun x -> x = index) piece_list_set0.li0
    for s in 0.. pos do
        if piece_list_set0.li0[s] <> index_to0 then
            score <- score + int(fv_kpp[sq_bk, index, piece_list_set0.li0[s]])
            counter <- counter + 1
            //printfn "+sq_bk=%d, l0=%d, l1=%d" sq_bk index piece_list_set0.li0[s]
    for s in pos + 1.. piece_num - 1 do
        if piece_list_set0.li0[s] <> index_to0 then
            score <- score + int(fv_kpp[sq_bk, piece_list_set0.li0[s], index])
            counter <- counter + 1
            //printfn "+sq_bk=%d, l0=%d, l1=%d" sq_bk piece_list_set0.li0[s] index
    pos <- List.findIndex(fun x -> x = index2) piece_list_set0.li1
    for s in 0.. pos do
        if piece_list_set0.li1[s] <> index_to1 then
            score <- score - int(fv_kpp[sq_wk, index2, piece_list_set0.li1[s]])
            counter <- counter + 1
            //printfn "-sq_wk=%d, l0=%d, l1=%d" sq_wk index2 piece_list_set0.li1[s]
    for s in pos + 1.. piece_num - 1 do
        if piece_list_set0.li1[s] <> index_to1 then
            score <- score - int(fv_kpp[sq_wk, piece_list_set0.li1[s], index2])
            counter <- counter + 1
            //printfn "-sq_wk=%d, l0=%d, l1=%d" sq_wk piece_list_set0.li1[s] index2
    score <- score / FV_SCALE
    score

let CalcDiffCapPro(bt:BoardTree ref, color:int, move:uint32):int = 
    let piece_num = 38
    let mutable score = 0
    let ifrom = int(Move.From(move))
    let ito = int(Move.To(move))
    let ipiece = int(Move.PieceType(move))
    let ipc_promo = ipiece + Promote
    let mutable icap_pc = int(Move.CapPiece(move))
    let mutable kkp_icap_pc = icap_pc
    if icap_pc > int(Piece.King) then
        kkp_icap_pc <- icap_pc - Promote
    let hand_num = (bt.contents.Hand.[color] &&& Hand_Mask.[kkp_icap_pc]) >>> Hand_Rev_Bit.[kkp_icap_pc]
    let mutable sq_bk = 0
    let mutable sq_wk = 0
    let mutable sq_bk2 = 0
    let mutable sq_wk2 = 0
    if color = int(Color.Black) then
        sq_bk <- int(bt.contents.SQ_King.[0])
        sq_wk <- int(bt.contents.SQ_King.[1])
        sq_bk2 <- int(Rev_Sq.[int(bt.contents.SQ_King.[0])])
        sq_wk2 <- int(Rev_Sq.[int(bt.contents.SQ_King.[1])])
    else
        sq_bk <- int(Rev_Sq.[int(bt.contents.SQ_King.[0])])
        sq_wk <- int(Rev_Sq.[int(bt.contents.SQ_King.[1])])
        sq_bk2 <- int(bt.contents.SQ_King.[0])
        sq_wk2 <- int(bt.contents.SQ_King.[1])
    let mutable index = 0
    if color = 0 then
        index <- kkp_index_table[color, ipiece, ifrom]
        score <- score - int(fv_kkp[sq_bk, sq_wk, index])
        index <- kkp_index_table[color, ipc_promo, ito]
        score <- score + int(fv_kkp[sq_bk, sq_wk, index])
        index <- kkp_index_table[color ^^^ 1, icap_pc, Rev_Sq[ito]]
        score <- score + int(fv_kkp[sq_bk2, sq_wk2, index])
        index <- kkp_hand_start_index[kkp_icap_pc] + hand_num - 1
        score <- score + int(fv_kkp[sq_bk, sq_wk, index])
    else
        index <- kkp_index_table[color, ipiece, Rev_Sq[ifrom]]
        score <- score + int(fv_kkp[sq_bk, sq_wk, index])
        index <- kkp_index_table[color, ipc_promo, Rev_Sq[ito]]
        score <- score - int(fv_kkp[sq_bk, sq_wk, index])
        index <- kkp_index_table[color ^^^ 1, icap_pc, ito]
        score <- score - int(fv_kkp[sq_bk2, sq_wk2, index])
        index <- kkp_hand_start_index[kkp_icap_pc] + hand_num - 1
        score <- score - int(fv_kkp[sq_bk, sq_wk, index])
    let piece_list_set0 = MakeList(bt.contents)// 1手指した後の駒リストが入っている。
    let mutable temp_bt = UnDo(ref bt.contents, move, color)
    bt.contents <- temp_bt.contents
    let piece_list_set1 = MakeList(temp_bt.contents)// 1手指す前の駒リストが入っている。
    sq_bk <- int(bt.contents.SQ_King.[0])
    sq_wk <- Rev_Sq[int(bt.contents.SQ_King.[1])]
    temp_bt <- Do(ref bt.contents, move, color)
    bt.contents <- temp_bt.contents

    // 移動元の差分計算
    index <- pp_index_table[color, ipiece, ifrom]
    let mutable index2 = pp_index_table[color ^^^ 1, ipiece, Rev_Sq.[ifrom]]
    let mutable pos = List.findIndex(fun x -> x = index) piece_list_set1.li0
    for s in 0.. pos do
        score <- score - int(fv_kpp[sq_bk, index, piece_list_set1.li0[s]])
    for s in pos + 1.. piece_num - 1 do
        score <- score - int(fv_kpp[sq_bk, piece_list_set1.li0[s],index])
    pos <- List.findIndex(fun x -> x = index2) piece_list_set1.li1
    for s in 0.. pos do
        score <- score + int(fv_kpp[sq_wk, index2, piece_list_set1.li1[s]])
    for s in pos + 1.. piece_num - 1 do
        score <- score + int(fv_kpp[sq_wk, piece_list_set1.li1[s], index2])

    let mutable index_from0 = index
    let mutable index_from1 = index2

    // 移動先の差分計算
    index <- pp_index_table[color, ipc_promo, ito]
    index2 <- pp_index_table[color ^^^ 1, ipc_promo, Rev_Sq.[ito]]
    pos <- List.findIndex(fun x -> x = index) piece_list_set0.li0
    for s in 0.. pos do
        score <- score + int(fv_kpp[sq_bk, index, piece_list_set0.li0[s]])
    for s in pos + 1.. piece_num - 1 do
        score <- score + int(fv_kpp[sq_bk, piece_list_set0.li0[s],index])
    pos <- List.findIndex(fun x -> x = index2) piece_list_set0.li1
    for s in 0.. pos do
        score <- score - int(fv_kpp[sq_wk, index2, piece_list_set0.li1[s]])
    for s in pos + 1.. piece_num - 1 do
        score <- score - int(fv_kpp[sq_wk, piece_list_set0.li1[s], index2])

    let mutable index_to0 = index
    let mutable index_to1 = index2

    // 取られた駒の差分計算
    index <- pp_index_table[color ^^^ 1, icap_pc, ito]
    index2 <- pp_index_table[color, icap_pc, Rev_Sq[ito]]
    pos <- List.findIndex(fun x -> x = index) piece_list_set1.li0
    for s in 0.. pos do
        if piece_list_set1.li0[s] <> index_from0 then
            score <- score - int(fv_kpp[sq_bk, index, piece_list_set1.li0[s]])
            //printfn "-sq_bk=%d, l0=%d, l1=%d" sq_bk index piece_list_set1.li0[s]
    for s in pos + 1.. piece_num - 1 do
        if piece_list_set1.li0[s] <> index_from0 then
            score <- score - int(fv_kpp[sq_bk, piece_list_set1.li0[s], index])
            //printfn "-sq_bk=%d, l0=%d, l1=%d" sq_bk piece_list_set1.li0[s] index
    pos <- List.findIndex(fun x -> x = index2) piece_list_set1.li1
    for s in 0.. pos do
        if piece_list_set1.li1[s] <> index_from1 then
            score <- score + int(fv_kpp[sq_wk, index2, piece_list_set1.li1[s]])
            //printfn "+sq_wk=%d, l0=%d, l1=%d" sq_wk index2 piece_list_set1.li1[s]
    for s in pos + 1.. piece_num - 1 do
        if piece_list_set1.li1[s] <> index_from1 then
            score <- score + int(fv_kpp[sq_wk, piece_list_set1.li1[s], index2])
            //printfn "+sq_wk=%d, l0=%d, l1=%d" sq_wk piece_list_set1.li1[s] index2

    // 駒台の差分計算 => 1枚分加算する。
    index <- pp_hand_start_index[color, kkp_icap_pc] + hand_num - 1
    index2 <- pp_hand_start_index[color ^^^ 1, kkp_icap_pc] + hand_num - 1
    pos <- List.findIndex(fun x -> x = index) piece_list_set0.li0
    for s in 0.. pos do
        if piece_list_set0.li0[s] <> index_to0 then
            score <- score + int(fv_kpp[sq_bk, index, piece_list_set0.li0[s]])
            //printfn "+sq_bk=%d, l0=%d, l1=%d" sq_bk index piece_list_set0.li0[s]
    for s in pos + 1.. piece_num - 1 do
        if piece_list_set0.li0[s] <> index_to0 then
            score <- score + int(fv_kpp[sq_bk, piece_list_set0.li0[s], index])
            //printfn "+sq_bk=%d, l0=%d, l1=%d" sq_bk piece_list_set0.li0[s] index
    pos <- List.findIndex(fun x -> x = index2) piece_list_set0.li1
    for s in 0.. pos do
        if piece_list_set0.li1[s] <> index_to1 then
            score <- score - int(fv_kpp[sq_wk, index2, piece_list_set0.li1[s]])
            //printfn "-sq_wk=%d, l0=%d, l1=%d" sq_wk index2 piece_list_set0.li1[s]
    for s in pos + 1.. piece_num - 1 do
        if piece_list_set0.li1[s] <> index_to1 then
            score <- score - int(fv_kpp[sq_wk, piece_list_set0.li1[s], index2])
            //printfn "-sq_wk=%d, l0=%d, l1=%d" sq_wk piece_list_set0.li1[s] index2
    score <- score / FV_SCALE
    score

let Eval(bt:BoardTree):int = 
    let piece_sum = 38
    let mutable score = 0
    let sq_bk = int(bt.SQ_King.[0])
    let sq_wk = Rev_Sq.[int(bt.SQ_King.[1])]
    let kkp_set = MakeListWithKKP(bt)
    score <- kkp_set.value
    //if bt.Ply = uint16(42) || bt.Ply = uint16(43)then
        //for i in 0 ..37 do
            //let k0 = kkp_set.li0.[i]
            //let k1 = kkp_set.li1.[i]
            //printfn "l0=%d, l1=%d" k0 k1
    for i in 0 .. piece_sum - 1 do
        let k0 = kkp_set.li0.[i]
        let k1 = kkp_set.li1.[i]
        let mutable j = 0
        while j <= i do
            let l0 = kkp_set.li0.[j]
            let l1 = kkp_set.li1.[j]
            score <- score + int(fv_kpp[sq_bk, k0, l0])
            score <- score - int(fv_kpp[sq_wk, k1, l1])
            //if bt.Ply = uint16(42) || bt.Ply = uint16(43)then
                //printfn "+sq_bk=%d, l0=%d, l1=%d" sq_bk k0 l0
                //printfn "-sq_wk=%d, l0=%d, l1=%d" sq_wk k1 l1
            j <- j + 1
    score <- score / FV_SCALE
    score

let EvalWrapper(bt:BoardTree ref, color:int, ply:int, move:uint32, is_root:bool):int = 
    let mutable score = 0
    let ifrom = int(Move.From(move))
    if is_root = true then
        score <- Eval(bt.contents)
        if color = 0 then
            score <- -score
    else
        if ifrom >= Square_NB then
            score <- bt.contents.EvalArray.[ply]
            let mutable value = CalcDiffDrop(ref bt.contents, color, move)
            //if color = 1 then
                //value <- -value
            score <- score + value
        else
            let ipiece = int(Move.PieceType(move))
            let icap_pc = int(Move.CapPiece(move))
            let is_promo = int(Move.FlagPromo(move))
            if ipiece = int(Piece.King) then
                score <- Eval(bt.contents)
                // ※バグがあるかもしれない。
                //if color = 0 then
                    //score <- -score
            else
                if icap_pc > 0 then
                    if is_promo = 1 then
                        score <- bt.contents.EvalArray[ply]
                        let mutable value = CalcDiffCapPro(bt, color, move)
                        //if color = 1 then
                            //value <- -value
                        score <- score + value
                    else
                        score <- bt.contents.EvalArray[ply]
                        let mutable value = CalcDiffCapNoPro(bt, color, move)
                        //if color = 1 then
                            //value <- -value
                        score <- score + value
                else
                    if is_promo = 1 then
                        score <- bt.contents.EvalArray[ply]
                        let mutable value = CalcDiffNoCapPro(bt, color, move)
                        //if color = 1 then
                            //value <- -value
                        score <- score + value
                    else
                        score <- bt.contents.EvalArray[ply]
                        let mutable value = CalcDiffNoCapNoPro(bt, color, move)
                        //if color = 1 then
                            //value <- -value
                        score <- score + value
    score

