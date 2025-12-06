[<AutoOpen>]
module Board

let HashFunc(bt: BoardTree):uint64 = 
    let mutable h:uint64 = uint64(0)
    for i in 0.. Color_NB - 1 do
        for j in 1.. Piece_NB - 1 do
            let mutable bb = bt.BB_Piece.[i, j]
            while bb <> System.UInt128.Zero do
                let sq = BitOperation.Square(bb)
                bb <- bb ^^^ ABB_Mask.[sq]
                h <- h ^^^ Rand[i, j, sq]
    h

let Init():BoardTree = 
    let mutable bt:BoardTree = BoardTree()
    let mutable b = Array.init Square_NB (fun i -> int8(0))
    bt.Board<-b
    bt.Board.[0] <- -int8(Piece.Lance)
    bt.Board.[1] <- -int8(Piece.Knight)
    bt.Board.[2] <- -int8(Piece.Silver)
    bt.Board.[3] <- -int8(Piece.Gold)
    bt.Board.[4] <- -int8(Piece.King)
    bt.Board.[5] <- -int8(Piece.Gold)
    bt.Board.[6] <- -int8(Piece.Silver)
    bt.Board.[7] <- -int8(Piece.Knight)
    bt.Board.[8] <- -int8(Piece.Lance)
    bt.Board.[10] <- -int8(Piece.Rook)
    bt.Board.[16] <- -int8(Piece.Bishop)
    bt.Board.[18] <- -int8(Piece.Pawn)
    bt.Board.[19] <- -int8(Piece.Pawn)
    bt.Board.[20] <- -int8(Piece.Pawn)
    bt.Board.[21] <- -int8(Piece.Pawn)
    bt.Board.[22] <- -int8(Piece.Pawn)
    bt.Board.[23] <- -int8(Piece.Pawn)
    bt.Board.[24] <- -int8(Piece.Pawn)
    bt.Board.[25] <- -int8(Piece.Pawn)
    bt.Board.[26] <- -int8(Piece.Pawn)
    bt.Board.[54] <- int8(Piece.Pawn)
    bt.Board.[55] <- int8(Piece.Pawn)
    bt.Board.[56] <- int8(Piece.Pawn)
    bt.Board.[57] <- int8(Piece.Pawn)
    bt.Board.[58] <- int8(Piece.Pawn)
    bt.Board.[59] <- int8(Piece.Pawn)
    bt.Board.[60] <- int8(Piece.Pawn)
    bt.Board.[61] <- int8(Piece.Pawn)
    bt.Board.[62] <- int8(Piece.Pawn)
    bt.Board.[64] <- int8(Piece.Bishop)
    bt.Board.[70] <- int8(Piece.Rook)
    bt.Board.[72] <- int8(Piece.Lance)
    bt.Board.[73] <- int8(Piece.Knight)
    bt.Board.[74] <- int8(Piece.Silver)
    bt.Board.[75] <- int8(Piece.Gold)
    bt.Board.[76] <- int8(Piece.King)
    bt.Board.[77] <- int8(Piece.Gold)
    bt.Board.[78] <- int8(Piece.Silver)
    bt.Board.[79] <- int8(Piece.Knight)
    bt.Board.[80] <- int8(Piece.Lance)
    let mutable p = Array2D.init Color_NB Piece_NB (fun i j -> System.UInt128())
    bt.BB_Piece <- p
    bt.BB_Piece.[0, 1] <- System.UInt128.Parse("133955584")
    bt.BB_Piece.[0, 2] <- System.UInt128.Parse("257")
    bt.BB_Piece.[0, 3] <- System.UInt128.Parse("130")
    bt.BB_Piece.[0, 4] <- System.UInt128.Parse("68")
    bt.BB_Piece.[0, 5] <- System.UInt128.Parse("40")
    bt.BB_Piece.[0, 6] <- System.UInt128.Parse("65536")
    bt.BB_Piece.[0, 7] <- System.UInt128.Parse("1024")
    bt.BB_Piece.[0, 8] <- System.UInt128.Parse("16")
    bt.BB_Piece.[1, 1] <- System.UInt128.Parse("9205357638345293824")
    bt.BB_Piece.[1, 2] <- System.UInt128.Parse("1213648186097498819919872")
    bt.BB_Piece.[1, 3] <- System.UInt128.Parse("613907642773053877780480")
    bt.BB_Piece.[1, 4] <- System.UInt128.Parse("321120920835135874531328")
    bt.BB_Piece.[1, 5] <- System.UInt128.Parse("188894659314785808547840")
    bt.BB_Piece.[1, 6] <- System.UInt128.Parse("18446744073709551616")
    bt.BB_Piece.[1, 7] <- System.UInt128.Parse("1180591620717411303424")
    bt.BB_Piece.[1, 8] <- System.UInt128.Parse("75557863725914323419136")
    let mutable o = Array.init Color_NB (fun i -> System.UInt128())
    bt.BB_Occupied <- o
    bt.BB_Occupied.[0] <- System.UInt128.Parse("134022655")
    bt.BB_Occupied.[1] <- System.UInt128.Parse("2414337516468818170347520")
    let mutable sk = Array.init Color_NB (fun i -> uint8(0))
    bt.SQ_King <- sk
    bt.SQ_King.[0] <- uint8(76)
    bt.SQ_King.[1] <- uint8(4)
    let mutable h = Array.init Color_NB (fun i -> 0)
    bt.Hand <- h
    bt.Hash <- Array.init (Max_Ply + 1) (fun i -> uint64(0))
    bt.RootColor <- Color.Black
    bt.Ply <- uint16(1)
    bt.CurrentHash <- HashFunc(bt)
    bt.PrevHash<-uint64(0)
    bt.EvalArray <- Array.init (Max_Ply + 1) (fun i -> 0)
    bt

let Clear():BoardTree = 
    let mutable bt:BoardTree = BoardTree()
    let mutable b = Array.init Square_NB (fun i -> int8(0))
    bt.Board <- b
    let mutable p = Array2D.init Color_NB Piece_NB (fun i j -> System.UInt128())
    bt.BB_Piece <- p
    let mutable o = Array.init Color_NB (fun i -> System.UInt128())
    bt.BB_Occupied <- o
    let mutable sk = Array.init Color_NB (fun i -> uint8(0))
    bt.SQ_King <- sk
    let mutable h = Array.init Color_NB (fun i -> 0)
    bt.Hand <- h
    bt.RootColor <- Color.Black
    bt.Ply <- uint16(1)
    bt.Hash <- Array.init (Max_Ply + 1) (fun i -> uint64(0))
    bt.CurrentHash <- HashFunc(bt)
    bt.PrevHash<-uint64(0)
    bt

let Do(bt:BoardTree ref, move:uint32, color:int) = 
    bt.contents.PrevHash <- bt.contents.CurrentHash
    let ifrom = Move.From(move)
    let ito = Move.To(move)
    let ipiece = Move.PieceType(move)
    let ipromote = Move.FlagPromo(move)
    if ifrom >= uint32(Square_NB) then
        bt.contents.BB_Piece[color, int(ipiece)] <- bt.contents.BB_Piece[color, int(ipiece)] ^^^ ABB_Mask.[int(ito)]
        bt.contents.CurrentHash <- bt.contents.CurrentHash ^^^ Rand[color, int(ipiece), int(ito)]
        bt.contents.Hand.[color] <- bt.contents.Hand.[color] - Hand_Hash[int(ipiece)]
        bt.contents.Board.[int(ito)] <- int8(-Sign_Table[color] * int(ipiece))
        bt.contents.BB_Occupied.[color] <- bt.contents.BB_Occupied.[color] ^^^ ABB_Mask.[int(ito)]
    else
        let bb_set_clear = ABB_Mask.[int(ifrom)] ||| ABB_Mask.[int(ito)]
        bt.contents.BB_Occupied.[color] <- bt.contents.BB_Occupied.[color] ^^^ bb_set_clear
        bt.contents.Board.[int(ifrom)] <- int8(Piece.Empty)
        if ipromote > uint32(0) then
            bt.contents.BB_Piece[color, int(ipiece)] <- bt.contents.BB_Piece[color, int(ipiece)] ^^^ ABB_Mask.[int(ifrom)]
            bt.contents.BB_Piece[color, int(ipiece) + Promote] <- bt.contents.BB_Piece[color, int(ipiece) + Promote] ^^^ ABB_Mask.[int(ito)]
            bt.contents.CurrentHash <- bt.contents.CurrentHash ^^^ Rand[color, int(ipiece), int(ifrom)]
            bt.contents.CurrentHash <- bt.contents.CurrentHash ^^^ Rand[color, int(ipiece) + Promote, int(ito)]
            bt.contents.Board.[int(ito)] <- int8(-Sign_Table[color] * (int(ipiece) + Promote))
        else
            if ipiece = uint32(Piece.King) then
                bt.contents.SQ_King.[color] <- uint8(ito)
            bt.contents.BB_Piece[color, int(ipiece)] <- bt.contents.BB_Piece[color, int(ipiece)] ^^^ bb_set_clear
            bt.contents.CurrentHash <- bt.contents.CurrentHash ^^^ Rand[color, int(ipiece), int(ifrom)]
            bt.contents.CurrentHash <- bt.contents.CurrentHash ^^^ Rand[color, int(ipiece), int(ito)]
            bt.contents.Board.[int(ito)] <- int8(-Sign_Table[color] * int(ipiece))
        let icap_piece = Move.CapPiece(move)
        let mutable index = icap_piece
        if icap_piece > uint32(0) then
            if icap_piece > uint32(Piece.King) then
                index <- index - uint32(Promote)
            bt.contents.Hand.[color] <- bt.contents.Hand.[color] + Hand_Hash[int(index)]
            bt.contents.BB_Piece[color ^^^ 1, int(icap_piece)] <- bt.contents.BB_Piece[color ^^^ 1, int(icap_piece)] ^^^ ABB_Mask.[int(ito)]
            bt.contents.CurrentHash <- bt.contents.CurrentHash ^^^ Rand[color ^^^ 1, int(icap_piece), int(ito)]
            bt.contents.BB_Occupied.[color ^^^ 1] <- bt.contents.BB_Occupied.[color ^^^ 1] ^^^ ABB_Mask.[int(ito)]
    bt.contents.Hash[int(bt.contents.Ply)] <- bt.contents.PrevHash
    bt.contents.Hash[int(bt.contents.Ply) + 1] <- bt.contents.CurrentHash
    bt.contents.Ply <- bt.contents.Ply + uint16(1)
    bt

let UnDo(bt:BoardTree ref, move:uint32, color:int) = 
    bt.contents.CurrentHash <- bt.contents.PrevHash
    let ifrom = Move.From(move)
    let ito = Move.To(move)
    let ipiece = Move.PieceType(move)
    let ipromote = Move.FlagPromo(move)
    if ifrom >= uint32(Square_NB) then
        bt.contents.BB_Piece[color, int(ipiece)] <- bt.contents.BB_Piece[color, int(ipiece)] ^^^ ABB_Mask.[int(ito)]
        bt.contents.Hand.[color] <- bt.contents.Hand.[color] + Hand_Hash[int(ipiece)]
        bt.contents.Board.[int(ito)] <- int8(Piece.Empty)
        bt.contents.BB_Occupied.[color] <- bt.contents.BB_Occupied.[color] ^^^ ABB_Mask.[int(ito)]
    else
        let bb_set_clear = ABB_Mask.[int(ifrom)] ||| ABB_Mask.[int(ito)]
        bt.contents.BB_Occupied.[color] <- bt.contents.BB_Occupied.[color] ^^^ bb_set_clear
        bt.contents.Board.[int(ifrom)] <- int8(-Sign_Table[color] * int(ipiece))
        if ipromote > uint32(0) then
            bt.contents.BB_Piece[color, int(ipiece)] <- bt.contents.BB_Piece[color, int(ipiece)] ^^^ ABB_Mask[int(ifrom)]
            bt.contents.BB_Piece[color, int(ipiece) + Promote] <- bt.contents.BB_Piece[color, int(ipiece) + Promote]^^^ ABB_Mask[int(ito)]
        else
            if ipiece = uint32(Piece.King) then
                bt.contents.SQ_King.[color] <- uint8(ifrom)
            bt.contents.BB_Piece[color, int(ipiece)] <- bt.contents.BB_Piece[color, int(ipiece)] ^^^ bb_set_clear
        let icap_piece = Move.CapPiece(move)
        let mutable index = icap_piece
        if icap_piece > uint32(0) then
            if icap_piece > uint32(Piece.King) then
                index <- index - uint32(Promote)
            bt.contents.Hand.[color] <- bt.contents.Hand.[color] - Hand_Hash[int(index)]
            bt.contents.BB_Piece[color ^^^ 1, int(icap_piece)] <- bt.contents.BB_Piece[color ^^^ 1, int(icap_piece)] ^^^ ABB_Mask.[int(ito)]
            bt.contents.BB_Occupied.[color ^^^ 1] <- bt.contents.BB_Occupied.[color ^^^ 1] ^^^ ABB_Mask.[int(ito)]
            bt.contents.Board.[int(ito)] <- int8(-Sign_Table[color ^^^ 1] * int(icap_piece))
        else
            bt.contents.Board.[int(ito)] <- int8(Piece.Empty)
    bt.contents.PrevHash <- bt.contents.Hash[int(bt.contents.Ply) - 2];
    bt.contents.Hash[int(bt.contents.Ply)] <- uint64(0); //配列のインデックスが1ずれていないかチェックする
    bt.contents.Ply <- bt.contents.Ply - uint16(1);
    bt

let DoNull(bt:BoardTree ref) = 
    bt.contents.Hash[int(bt.contents.Ply) + 1] <- bt.contents.CurrentHash
    bt.contents.Ply <- bt.contents.Ply + uint16(1)
    bt

let UnDoNull(bt:BoardTree ref) = 
    bt.contents.Hash[int(bt.contents.Ply)] <- uint64(0)
    bt.contents.Ply <- bt.contents.Ply - uint16(1)

// 戻り値
// 0: 宣言勝ちの局面ではない。
// 1: 先手の勝ち
// 2: 後手の勝ち
let IsDeclarationWin(bt:BoardTree):int = 
    let bb0 = bt.BB_Piece[0, int(Piece.King)] &&& BB_White_Position
    let bb1 = bt.BB_Piece[1, int(Piece.King)] &&& BB_Black_Position
    let mutable black_score = 0
    let mutable white_score = 0
    let mutable b_tekijin_piece_count = 0
    let mutable w_tekijin_piece_count = 0
    let b_hand_piece_count = [| 0; 0; 0; 0; 0; 0; 0; 0; |]
    let w_hand_piece_count = [| 0; 0; 0; 0; 0; 0; 0; 0; |]
    let b_board_piece_count= [| 0; 0; 0; 0; 0; 0; 0; 0; |]
    let w_board_piece_count= [| 0; 0; 0; 0; 0; 0; 0; 0; |]
    let mutable iret = 0
    if bb0 = System.UInt128() && bb1 = System.UInt128() then
        iret <- 0
    else
        if bb0 > System.UInt128() then
            for i in int(Piece.Pawn) .. int(Piece.Rook) do
                b_hand_piece_count[i] <- (bt.Hand[0] &&& Hand_Mask[i]) >>> Hand_Rev_Bit[i]
                if i >= int(Piece.Bishop) then
                    black_score <- black_score + 5 * b_hand_piece_count[i]
                else
                    black_score <- black_score +  b_hand_piece_count[i]
            for i in int(Piece.Pawn) .. int(Piece.Dragon) do
                if i <> int(Piece.None) then
                    let mutable bb_object = bt.BB_Piece[0, i] &&& BB_Rev_Color_Position[0]
                    b_board_piece_count[i] <- int(System.UInt128.PopCount(bb_object))
                    b_tekijin_piece_count <- b_tekijin_piece_count + b_board_piece_count[i]
                    let bb_temp = BB_DMZ ||| BB_Rev_Color_Position[1]
                    bb_object <- bb_temp &&& bt.BB_Piece[0, i]
                    b_board_piece_count[i] <- b_board_piece_count[i] + int(System.UInt128.PopCount(bb_object))
                    if i <> int(Piece.King) then
                        if i = int(Piece.Bishop) || i = int(Piece.Rook) || i >= int(Piece.Horse) then
                            black_score <- black_score + 5 * b_board_piece_count[i]
                        else
                            black_score <- black_score +  b_board_piece_count[i]
        if bb1 > System.UInt128() then
            for i in int(Piece.Pawn) .. int(Piece.Rook) do
                w_hand_piece_count[i] <- (bt.Hand[1] &&& Hand_Mask[i]) >>> Hand_Rev_Bit[i]
                if i >= int(Piece.Bishop) then
                    white_score <- white_score + 5 * w_hand_piece_count[i]
                else
                    white_score <- white_score +  w_hand_piece_count[i]
            for i in int(Piece.Pawn) .. int(Piece.Dragon) do
                if i <> int(Piece.None) then
                    let mutable bb_object = bt.BB_Piece[1, i] &&& BB_Rev_Color_Position[1]
                    w_board_piece_count[i] <- int(System.UInt128.PopCount(bb_object))
                    w_tekijin_piece_count <- w_tekijin_piece_count + w_board_piece_count[i]
                    let bb_temp = BB_DMZ ||| BB_Rev_Color_Position[0]
                    bb_object <- bb_temp &&& bt.BB_Piece[1, i]
                    w_board_piece_count[i] <- w_board_piece_count[i] + int(System.UInt128.PopCount(bb_object))
                    if i <> int(Piece.King) then
                        if i = int(Piece.Bishop) || i = int(Piece.Rook) || i >= int(Piece.Horse) then
                            white_score <- white_score + 5 * b_board_piece_count[i]
                        else
                            white_score <- white_score +  b_board_piece_count[i]
    if bb0 > System.UInt128() && black_score >= 28 && b_tekijin_piece_count >= 10 then
        iret <- 1
    if bb1 > System.UInt128() && white_score >= 27 && w_tekijin_piece_count >= 10 then
        iret <- 2
    iret

let IsRepetition(bt:BoardTree, tt:TT):int = 
    let mutable iret = 0
    let limit = int(bt.Ply) - 12// 12 -> 13とした
    if limit >= 1 then
        let mutable counter = 0
        let mutable i = int(bt.Ply)
        while i >= limit do
            if bt.CurrentHash = bt.Hash[i] then
                counter <- counter + 1
            i <- i - 1
        // 手抜きのため、同一局面3回で千日手と判定する。
        if counter > 2 then
            if tt.is_check.ContainsKey(bt.CurrentHash) then
                let b = tt.is_check[bt.CurrentHash]
                if b = false then
                    iret <- 1
                else
                    iret <- 2
    iret

let StoreHash(tt:TT ref, k:uint64, v:int, c:int, ch:bool, m:uint32, param_ply:int16) = 
    let lockObj = obj()

    let threadSafeFunction () =
        lock lockObj (fun () ->
            tt.contents.value.[k] <- v
        )
    let threadSafeFunction () =
        lock lockObj (fun () ->
            tt.contents.color.[k] <- c
        )
    let threadSafeFunction () =
        lock lockObj (fun () ->
            tt.contents.is_check.[k] <- ch
        )
    let threadSafeFunction () =
        lock lockObj (fun () ->
            tt.contents.move.[k] <- m
        )
    let threadSafeFunction () =
        lock lockObj (fun () ->
            tt.contents.ply.[k] <- param_ply
        )
    0u