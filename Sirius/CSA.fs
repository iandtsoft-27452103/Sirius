[<AutoOpen>]
module CSA

let CSA2Move(bt:BoardTree, str_csa:string):uint32 = 
    let mutable ifrom = uint32(CSA_TO_SQ[str_csa[0..1]])
    let ito = uint32(CSA_TO_SQ[str_csa[2..3]])
    let mutable piece = int(CSA_TO_PC[str_csa[4..5]])
    let mutable temp_pc = int8(0)
    if ifrom = uint32(Square_NB) then
        ifrom <- ifrom + uint32(piece) - 1u
    else
        temp_pc <- abs(bt.Board[int(ifrom)])
    let mutable flag_promo:int = 0
    if piece > int(Piece.King) && temp_pc <> int8(piece) then
        flag_promo <- 1
        piece <- int(temp_pc)
    let icap_piece = uint32(abs(bt.Board[int(ito)]))
    let move = Move.Pack(ifrom, ito, uint32(piece), icap_piece, uint32(flag_promo))
    move

let Move2CSA(move:uint32):string = 
    let ifrom = Move.From(move)
    let ito = Move.To(move)
    let ipiece = Move.PieceType(move)
    let ipromote = Move.FlagPromo(move)
    let mutable str_move:string = ""
    if ifrom >= uint32(Square_NB) then
        let str_from:string = "00"
        str_move<-str_from
    else
        let str_from:string = Str_CSA[int(ifrom)]
        str_move<-str_from
    let str_to:string = Str_CSA[int(ito)]
    str_move <- str_move + str_to
    if ipromote = uint32(1) then
        let str_piece:string = Str_Piece[int(ipiece) + Promote]
        str_move <- str_move + str_piece
    else
        let str_piece:string = Str_Piece[int(ipiece)]
        str_move<-str_move + str_piece
    str_move