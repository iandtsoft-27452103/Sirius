[<AutoOpen>]
module GenMoves

let GenDrop(bt: BoardTree, color:int, moves:ResizeArray<uint32> ref) = 
    let mutable bb_piece_can_drop:System.UInt128[] = Array.init (Piece_Can_Drop_NB + 1) (fun i  -> System.UInt128())
    //let mutable move_index = moves.contents.Length - 1
    let mutable move_index = 0
    let bb_occupied = bt.BB_Occupied.[0] ||| bt.BB_Occupied.[1]
    let bb_empty = bb_occupied ^^^ System.UInt128.MaxValue &&& BB_Full
    if bt.Hand[color]  &&& Hand_Mask[int(Piece.Pawn)] > 0 then
        for i = int(File.File1) to int(File.File9) do
            let bb = BB_File.[i] &&& bt.BB_Piece[color, int(Piece.Pawn)]
            if bb = System.UInt128(uint64(0), uint64(0)) then
                bb_piece_can_drop.[int(Piece.Pawn)] <- bb_piece_can_drop.[int(Piece.Pawn)] ||| BB_File.[i]
        let sq = bt.SQ_King[color ^^^ 1] + uint8(Delta_Table[color ^^^ 1])
        let mutable bb = System.UInt128()
        if sq >= uint8(0) && sq < uint8(Square_NB) then
            bb <- bb_piece_can_drop.[int(Piece.Pawn)] ||| ABB_Mask[int(sq)]
            if bt.Board[int(sq)] = int8(Piece.Empty) && bb > System.UInt128() then
                if IsMatePawnDrop(bt, int(sq), color ^^^ 1) then
                    bb_piece_can_drop.[int(Piece.Pawn)] <- bb_piece_can_drop.[int(Piece.Pawn)] ^^^ ABB_Mask.[int(sq)]
    bb_piece_can_drop.[int(Piece.Pawn)] <- BB_Pawn_Lance_Can_Drop.[color] &&& bb_piece_can_drop.[int(Piece.Pawn)]
    bb_piece_can_drop.[int(Piece.Lance)] <- BB_Pawn_Lance_Can_Drop.[color] &&& bb_empty
    bb_piece_can_drop.[int(Piece.Knight)] <- BB_Knight_Can_Drop.[color] &&& bb_empty
    bb_piece_can_drop.[int(Piece.Silver)] <- BB_Others_Can_Drop &&& bb_empty
    bb_piece_can_drop.[int(Piece.Gold)] <- bb_piece_can_drop.[int(Piece.Silver)]
    bb_piece_can_drop.[int(Piece.Bishop)] <- bb_piece_can_drop.[int(Piece.Silver)]
    bb_piece_can_drop.[int(Piece.Rook)] <- bb_piece_can_drop.[int(Piece.Silver)]
    for i = (int)Piece.Pawn to (int)Piece.Rook do
        if (bt.Hand.[color] &&& Hand_Mask.[i]) > 0 then
            let mutable bb = bb_piece_can_drop.[i]
            while bb > System.UInt128() do
                let ifrom = Square_NB + i - 1
                let ito = Square(bb)
                bb <- bb ^^^ ABB_Mask.[ito]
                let move = Move.Pack(uint32(ifrom), uint32(ito), uint32(i), 0u, 0u)
                moves.contents.[move_index] <- move
                move_index <- move_index + 1
    move_index

let GenNoCap(bt: BoardTree, color:int, moves:ResizeArray<uint32> ref) = 
    //let mutable move_index = moves.contents.Length - 1
    let mutable move_index = 0
    let bb_occupied = bt.BB_Occupied.[0] ||| bt.BB_Occupied.[1]
    let bb_empty = bb_occupied ^^^ System.UInt128.MaxValue &&& BB_Full
    let mutable bb_from = bt.BB_Piece[color, int(Piece.Pawn)]
    while bb_from > System.UInt128() do
        let ifrom = Square(bb_from)
        bb_from <- bb_from ^^^ ABB_Mask.[ifrom]
        let mutable bb_to = ABB_Piece_Attacks[color, int(Piece.Pawn), ifrom] &&& bb_empty
        while bb_to > System.UInt128() do
            let ito = Square(bb_to)
            bb_to <- bb_to ^^^ ABB_Mask.[ito]
            let mutable flag_promo = uint32(0)
            if (BB_Rev_Color_Position[color] &&& ABB_Mask.[ito]) > System.UInt128() then
                flag_promo <- uint32(1)
            let move = Move.Pack(uint32(ifrom), uint32(ito), uint32(Piece.Pawn), 0u, flag_promo)
            moves.contents.[move_index] <- move
            move_index <- move_index + 1
    bb_from <- bt.BB_Piece[color, int(Piece.Knight)]
    while bb_from > System.UInt128() do
        let ifrom = Square(bb_from)
        bb_from <- bb_from ^^^ ABB_Mask.[ifrom]
        let mutable bb_to = ABB_Piece_Attacks[color, int(Piece.Knight), ifrom] &&& bb_empty
        while bb_to > System.UInt128() do
            let ito = Square(bb_to)
            bb_to <- bb_to ^^^ ABB_Mask.[ito]
            let bb_can_promote = BB_Rev_Color_Position.[color] &&& ABB_Mask.[ito]
            if bb_can_promote > System.UInt128() then
                let move = Move.Pack(uint32(ifrom), uint32(ito), uint32(Piece.Knight), 0u, uint32(1))
                moves.contents.[move_index] <- move
                move_index <- move_index + 1
            if (BB_Knight_Must_Promote[color] &&& ABB_Mask[ito]) = System.UInt128() then
                let move = Move.Pack(uint32(ifrom), uint32(ito), uint32(Piece.Knight), 0u, 0u)
                moves.contents.[move_index] <- move
                move_index <- move_index + 1
    bb_from <- bt.BB_Piece[color, int(Piece.Silver)]
    while bb_from > System.UInt128() do
        let ifrom = Square(bb_from)
        bb_from <- bb_from ^^^ ABB_Mask.[ifrom]
        let mutable bb_to = ABB_Piece_Attacks[color, int(Piece.Silver), ifrom] &&& bb_empty
        while bb_to > System.UInt128() do
            let ito = Square(bb_to)
            bb_to <- bb_to ^^^ ABB_Mask.[ito]
            let bb_can_promote = BB_Rev_Color_Position.[color] &&& ABB_Mask.[ito]
            if bb_can_promote > System.UInt128() then
                let move_pro = Move.Pack(uint32(ifrom), uint32(ito), uint32(Piece.Silver), 0u, uint32(1))
                moves.contents.[move_index] <- move_pro
                move_index <- move_index + 1
            let move = Move.Pack(uint32(ifrom), uint32(ito), uint32(Piece.Knight), 0u, 0u)
            moves.contents.[move_index] <- move
            move_index <- move_index + 1
    let piece_list = [| Piece.Gold; Piece.King; Piece.Pro_Pawn; Piece.Pro_Lance; Piece.Pro_Knight; Piece.Pro_Silver |]
    for i = 0 to piece_list.Length - 1 do
        let piece = piece_list.[i]
        bb_from <- bt.BB_Piece[color, int(piece)]
        while bb_from > System.UInt128() do
            let ifrom = Square(bb_from)
            bb_from <- bb_from ^^^ ABB_Mask.[ifrom]
            let mutable bb_to = ABB_Piece_Attacks[color, int(piece), ifrom] &&& bb_empty
            while bb_to > System.UInt128() do
                let ito = Square(bb_to)
                bb_to <- bb_to ^^^ ABB_Mask.[ito]
                let move = Move.Pack(uint32(ifrom), uint32(ito), uint32(piece), 0u, 0u)
                moves.contents.[move_index] <- move
                move_index <- move_index + 1
    bb_from <- bt.BB_Piece[color, int(Piece.Lance)]
    while bb_from > System.UInt128() do
        let ifrom = Square(bb_from)
        bb_from <- bb_from ^^^ ABB_Mask.[ifrom]
        let mutable bb_to = ABB_Lance_Attacks[color, ifrom][ABB_Lance_Mask_Ex[color, ifrom] &&& bb_occupied] &&& bb_empty
        while bb_to > System.UInt128() do
            let ito = Square(bb_to)
            bb_to <- bb_to ^^^ ABB_Mask.[ito]
            let bb_can_promote = BB_Rev_Color_Position.[color] &&& ABB_Mask.[ito]
            if bb_can_promote > System.UInt128() then
                let move_pro = Move.Pack(uint32(ifrom), uint32(ito), uint32(Piece.Lance), 0u, uint32(1))
                moves.contents.[move_index] <- move_pro
                move_index <- move_index + 1
            if (BB_Knight_Must_Promote.[color] &&& ABB_Mask.[ito]) = System.UInt128() then
                let move = Move.Pack(uint32(ifrom), uint32(ito), uint32(Piece.Lance), 0u, 0u)
                moves.contents.[move_index] <- move
                move_index <- move_index + 1
    bb_from <- bt.BB_Piece[color, int(Piece.Bishop)]
    while bb_from > System.UInt128() do
        let ifrom = Square(bb_from)
        bb_from <- bb_from ^^^ ABB_Mask.[ifrom]
        let mutable bb_to = ABB_Diagonal_Attacks[ifrom][ABB_Diagonal_Mask_Ex[ifrom] &&& bb_occupied] &&& bb_empty
        while bb_to > System.UInt128() do
            let ito = Square(bb_to)
            bb_to <- bb_to ^^^ ABB_Mask.[ito]
            let mutable flag_promo = uint32(0)
            if (BB_Rev_Color_Position[color] &&& (ABB_Mask.[ito] ||| ABB_Mask.[ifrom]) > System.UInt128()) then
                flag_promo <- uint32(1)
            let move = Move.Pack(uint32(ifrom), uint32(ito), uint32(Piece.Bishop), 0u, flag_promo)
            moves.contents.[move_index] <- move
            move_index <- move_index + 1
    bb_from <- bt.BB_Piece[color, int(Piece.Horse)]
    while bb_from > System.UInt128() do
        let ifrom = Square(bb_from)
        bb_from <- bb_from ^^^ ABB_Mask.[ifrom]
        let mutable bb_to = (ABB_Diagonal_Attacks[ifrom][ABB_Diagonal_Mask_Ex[ifrom] &&& bb_occupied] ||| ABB_Piece_Attacks[color, int(Piece.King), ifrom]) &&& bb_empty
        while bb_to > System.UInt128() do
            let ito = Square(bb_to)
            bb_to <- bb_to ^^^ ABB_Mask.[ito]
            let move = Move.Pack(uint32(ifrom), uint32(ito), uint32(Piece.Horse), 0u, 0u)
            moves.contents.[move_index] <- move
            move_index <- move_index + 1
    bb_from <- bt.BB_Piece[color, int(Piece.Rook)]
    while bb_from > System.UInt128() do
        let ifrom = Square(bb_from)
        bb_from <- bb_from ^^^ ABB_Mask.[ifrom]
        let mutable bb_to = ABB_Cross_Attacks[ifrom][ABB_Cross_Mask_Ex[ifrom] &&& bb_occupied] &&& bb_empty
        while bb_to > System.UInt128() do
            let ito = Square(bb_to)
            bb_to <- bb_to ^^^ ABB_Mask.[ito]
            let mutable flag_promo = uint32(0)
            if (BB_Rev_Color_Position[color] &&& (ABB_Mask.[ito] ||| ABB_Mask.[ifrom]) > System.UInt128()) then
                flag_promo <- uint32(1)
            let move = Move.Pack(uint32(ifrom), uint32(ito), uint32(Piece.Rook), 0u, flag_promo)
            moves.contents.[move_index] <- move
            move_index <- move_index + 1
    bb_from <- bt.BB_Piece[color, int(Piece.Dragon)]
    while bb_from > System.UInt128() do
        let ifrom = Square(bb_from)
        bb_from <- bb_from ^^^ ABB_Mask.[ifrom]
        let mutable bb_to = (ABB_Cross_Attacks[ifrom][ABB_Cross_Mask_Ex[ifrom] &&& bb_occupied] ||| ABB_Piece_Attacks[color, int(Piece.King), ifrom]) &&& bb_empty
        while bb_to > System.UInt128() do
            let ito = Square(bb_to)
            bb_to <- bb_to ^^^ ABB_Mask.[ito]
            let move = Move.Pack(uint32(ifrom), uint32(ito), uint32(Piece.Dragon), 0u, 0u)
            moves.contents.[move_index] <- move
            move_index <- move_index + 1
    move_index

let GenCap(bt: BoardTree, color:int, moves:ResizeArray<uint32> ref) = 
    //let mutable move_index = moves.contents.Length - 1
    let mutable move_index = 0
    let bb_occupied = bt.BB_Occupied.[0] ||| bt.BB_Occupied.[1]
    let bb_can_cap = bt.BB_Occupied[color ^^^ 1]
    let mutable bb_from = bt.BB_Piece[color, (int)Piece.Pawn]
    while bb_from > System.UInt128() do
        let ifrom = Square(bb_from)
        bb_from <- bb_from ^^^ ABB_Mask.[ifrom]
        let mutable bb_to = ABB_Piece_Attacks[color, int(Piece.Pawn), ifrom] &&& bb_can_cap
        while bb_to > System.UInt128() do
            let ito = Square(bb_to)
            bb_to <- bb_to ^^^ ABB_Mask.[ito]
            let mutable flag_promo = uint32(0)
            if (BB_Rev_Color_Position[color] &&& ABB_Mask.[ito]) > System.UInt128() then
                flag_promo <- uint32(1)
            let move = Move.Pack(uint32(ifrom), uint32(ito), uint32(Piece.Pawn), uint32(abs(bt.Board.[ito])), flag_promo)
            moves.contents.[move_index] <- move
            move_index <- move_index + 1
    bb_from <- bt.BB_Piece[color, (int)Piece.Knight]
    while bb_from > System.UInt128() do
        let ifrom = Square(bb_from)
        bb_from <- bb_from ^^^ ABB_Mask.[ifrom]
        let mutable bb_to = ABB_Piece_Attacks[color, int(Piece.Knight), ifrom] &&& bb_can_cap
        while bb_to > System.UInt128() do
            let ito = Square(bb_to)
            bb_to <- bb_to ^^^ ABB_Mask.[ito]
            let bb_can_promote = BB_Rev_Color_Position.[color] &&& ABB_Mask.[ito]
            if bb_can_promote > System.UInt128() then
                let move = Move.Pack(uint32(ifrom), uint32(ito), uint32(Piece.Knight), uint32(abs(bt.Board.[ito])), uint32(1))
                moves.contents.[move_index] <- move
                move_index <- move_index + 1
            if (BB_Knight_Must_Promote[color] &&& ABB_Mask[ito]) = System.UInt128() then
                let move = Move.Pack(uint32(ifrom), uint32(ito), uint32(Piece.Knight), uint32(abs(bt.Board.[ito])), 0u)
                moves.contents.[move_index] <- move
                move_index <- move_index + 1
    bb_from <- bt.BB_Piece[color, (int)Piece.Silver]
    while bb_from > System.UInt128() do
        let ifrom = Square(bb_from)
        bb_from <- bb_from ^^^ ABB_Mask.[ifrom]
        let mutable bb_to = ABB_Piece_Attacks[color, int(Piece.Silver), ifrom] &&& bb_can_cap
        while bb_to > System.UInt128() do
            let ito = Square(bb_to)
            bb_to <- bb_to ^^^ ABB_Mask.[ito]
            let bb_can_promote = BB_Rev_Color_Position.[color] &&& ABB_Mask.[ito]
            if bb_can_promote > System.UInt128() then
                let move_pro = Move.Pack(uint32(ifrom), uint32(ito), uint32(Piece.Silver), uint32(abs(bt.Board.[ito])), uint32(1))
                moves.contents.[move_index] <- move_pro
                move_index <- move_index + 1
            let move = Move.Pack(uint32(ifrom), uint32(ito), uint32(Piece.Silver), uint32(abs(bt.Board.[ito])), 0u)
            moves.contents.[move_index] <- move
            move_index <- move_index + 1
    let piece_list = [| Piece.Gold; Piece.King; Piece.Pro_Pawn; Piece.Pro_Lance; Piece.Pro_Knight; Piece.Pro_Silver |]
    for i = 0 to piece_list.Length - 1 do
        let piece = piece_list.[i]
        bb_from <- bt.BB_Piece[color, int(piece)]
        while bb_from > System.UInt128() do
            let ifrom = Square(bb_from)
            bb_from <- bb_from ^^^ ABB_Mask.[ifrom]
            let mutable bb_to = ABB_Piece_Attacks[color, int(piece), ifrom] &&& bb_can_cap
            while bb_to > System.UInt128() do
                let ito = Square(bb_to)
                bb_to <- bb_to ^^^ ABB_Mask.[ito]
                let move = Move.Pack(uint32(ifrom), uint32(ito), uint32(piece), uint32(abs(bt.Board.[ito])), 0u)
                moves.contents.[move_index] <- move
                move_index <- move_index + 1
    bb_from <- bt.BB_Piece[color, int(Piece.Lance)]
    while bb_from > System.UInt128() do
        let ifrom = Square(bb_from)
        bb_from <- bb_from ^^^ ABB_Mask.[ifrom]
        let mutable bb_to = ABB_Lance_Attacks[color, ifrom][ABB_Lance_Mask_Ex[color, ifrom] &&& bb_occupied] &&& bb_can_cap
        while bb_to > System.UInt128() do
            let ito = Square(bb_to)
            bb_to <- bb_to ^^^ ABB_Mask.[ito]
            let bb_can_promote = BB_Rev_Color_Position.[color] &&& ABB_Mask.[ito]
            if bb_can_promote > System.UInt128() then
                let move_pro = Move.Pack(uint32(ifrom), uint32(ito), uint32(Piece.Lance), uint32(abs(bt.Board.[ito])), uint32(1))
                moves.contents.[move_index] <- move_pro
                move_index <- move_index + 1
            if (BB_Knight_Must_Promote.[color] &&& ABB_Mask.[ito]) = System.UInt128() then
                let move = Move.Pack(uint32(ifrom), uint32(ito), uint32(Piece.Lance), uint32(abs(bt.Board.[ito])), 0u)
                moves.contents.[move_index] <- move
                move_index <- move_index + 1
    bb_from <- bt.BB_Piece[color, int(Piece.Bishop)]
    while bb_from > System.UInt128() do
        let ifrom = Square(bb_from)
        bb_from <- bb_from ^^^ ABB_Mask.[ifrom]
        let mutable bb_to = ABB_Diagonal_Attacks[ifrom][ABB_Diagonal_Mask_Ex[ifrom] &&& bb_occupied] &&& bb_can_cap
        while bb_to > System.UInt128() do
            let ito = Square(bb_to)
            bb_to <- bb_to ^^^ ABB_Mask.[ito]
            let mutable flag_promo = uint32(0)
            if (BB_Rev_Color_Position[color] &&& (ABB_Mask.[ito] ||| ABB_Mask.[ifrom]) > System.UInt128()) then
                flag_promo <- uint32(1)
            let move = Move.Pack(uint32(ifrom), uint32(ito), uint32(Piece.Bishop), uint32(abs(bt.Board.[ito])), flag_promo)
            moves.contents.[move_index] <- move
            move_index <- move_index + 1
    bb_from <- bt.BB_Piece[color, int(Piece.Horse)]
    while bb_from > System.UInt128() do
        let ifrom = Square(bb_from)
        bb_from <- bb_from ^^^ ABB_Mask.[ifrom]
        let mutable bb_to = (ABB_Diagonal_Attacks[ifrom][ABB_Diagonal_Mask_Ex[ifrom] &&& bb_occupied] ||| ABB_Piece_Attacks[color, int(Piece.King), ifrom]) &&& bb_can_cap
        while bb_to > System.UInt128() do
            let ito = Square(bb_to)
            bb_to <- bb_to ^^^ ABB_Mask.[ito]
            let move = Move.Pack(uint32(ifrom), uint32(ito), uint32(Piece.Horse), uint32(abs(bt.Board.[ito])), 0u)
            moves.contents.[move_index] <- move
            move_index <- move_index + 1
    bb_from <- bt.BB_Piece[color, int(Piece.Rook)]
    while bb_from > System.UInt128() do
        let ifrom = Square(bb_from)
        bb_from <- bb_from ^^^ ABB_Mask.[ifrom]
        let mutable bb_to = ABB_Cross_Attacks[ifrom][ABB_Cross_Mask_Ex[ifrom] &&& bb_occupied] &&& bb_can_cap
        while bb_to > System.UInt128() do
            let ito = Square(bb_to)
            bb_to <- bb_to ^^^ ABB_Mask.[ito]
            let mutable flag_promo = uint32(0)
            if (BB_Rev_Color_Position[color] &&& (ABB_Mask.[ito] ||| ABB_Mask.[ifrom]) > System.UInt128()) then
                flag_promo <- uint32(1)
            let move = Move.Pack(uint32(ifrom), uint32(ito), uint32(Piece.Rook), uint32(abs(bt.Board.[ito])), flag_promo)
            moves.contents.[move_index] <- move
            move_index <- move_index + 1
    bb_from <- bt.BB_Piece[color, int(Piece.Dragon)]
    while bb_from > System.UInt128() do
        let ifrom = Square(bb_from)
        bb_from <- bb_from ^^^ ABB_Mask.[ifrom]
        let mutable bb_to = (ABB_Cross_Attacks[ifrom][ABB_Cross_Mask_Ex[ifrom] &&& bb_occupied] ||| ABB_Piece_Attacks[color, int(Piece.King), ifrom]) &&& bb_can_cap
        while bb_to > System.UInt128() do
            let ito = Square(bb_to)
            bb_to <- bb_to ^^^ ABB_Mask.[ito]
            let move = Move.Pack(uint32(ifrom), uint32(ito), uint32(Piece.Dragon), uint32(abs(bt.Board.[ito])), 0u)
            moves.contents.[move_index] <- move
            move_index <- move_index + 1
    move_index

let GenEvasion(bt: BoardTree ref, color:int, moves:ResizeArray<uint32> ref) = 
    //let mutable move_index = moves.contents.Length - 1
    let mutable move_index = 0
    let mutable move:uint32 = Move.Init()
    let mutable bb_piece_can_drop:System.UInt128[] = Array.init (Piece_Can_Drop_NB + 1) (fun i  -> System.UInt128())
    let mutable flag:bool = false
    let sq_king = bt.contents.SQ_King[color]
    let mutable ifrom = sq_king
    let bb_occupied = bt.contents.BB_Occupied[0] ||| bt.contents.BB_Occupied[1]
    bt.contents.BB_Occupied.[color] <- bt.contents.BB_Occupied[color] ^^^ ABB_Mask.[int(ifrom)]
    let mutable bb_empty = bb_occupied ^^^ System.UInt128.MaxValue &&& BB_Full
    let mutable bb_not_color:System.UInt128 = bt.contents.BB_Occupied[color ^^^ 1] ||| bb_empty
    bb_not_color <- bb_not_color &&& BB_Full
    let mutable bb_to:System.UInt128 = ABB_Piece_Attacks[color, int(Piece.King), int(sq_king)] &&& bb_not_color
    while bb_to > System.UInt128() do
        let ito = Square(bb_to)
        if IsAttacked(bt.contents, ito, color) = System.UInt128() then
            move <- Move.Pack(uint32(ifrom), uint32(ito), uint32(Piece.King), uint32(abs(bt.contents.Board.[ito])), 0u)
            moves.contents.[move_index] <- move
            move_index <- move_index + 1
        bb_to <- bb_to ^^^ ABB_Mask.[ito]
    bt.contents.BB_Occupied.[color] <- bt.contents.BB_Occupied[color] ^^^ ABB_Mask.[int(ifrom)]
    let bb_checker = AttacksToPiece(bt.contents, int(sq_king), color ^^^ 1)
    let checker_num:int = int(System.UInt128.PopCount(bb_checker))
    if bb_checker > System.UInt128() && int(checker_num) <> 2 then
        let sq_checker = Square(bb_checker)
        let mutable bb_cap_checker = AttacksToPiece(bt.contents, sq_checker, color)
        let mutable ipiece = abs(bt.contents.Board.[int(ifrom)])
        let mutable ito = sq_checker
        while bb_cap_checker <> System.UInt128() do
            let ifrom = Square(bb_cap_checker)
            bb_cap_checker <- bb_cap_checker ^^^ ABB_Mask.[ifrom]
            if ifrom <> int(sq_king) then
                ipiece <- abs(bt.contents.Board[ifrom])
                let idirec = Adirec[ifrom, ito]
                let ipc = int(ipiece)
                flag <- false
                if IsPinnedOnKing(bt.contents, ifrom, idirec, color) = System.UInt128() then
                    if (Array.contains ipc Set_Piece_Can_Promote0) && (ABB_Piece_Attacks[color, (int)ipiece, ifrom] &&& ABB_Mask.[sq_checker]) > System.UInt128() && (ABB_Piece_Attacks[color, (int)ipiece, ifrom] &&& BB_Rev_Color_Position.[color]) > System.UInt128() then
                        move <- Move.Init()
                        move <- Move.Pack(uint32(ifrom), uint32(ito), uint32(ipiece), uint32(abs(bt.contents.Board.[ito])), 1u)
                        let result = Do(bt, move, color)
                        bt.contents <- result.contents
                        //bt.contents <- temp_bt.contents// ここは要確認
                        if IsAttacked(bt.contents, int(sq_king), color) = System.UInt128() then
                            moves.contents.[move_index] <- move
                            move_index <- move_index + 1
                        let result = UnDo(bt, move, color)
                        bt.contents <- result.contents
                        if ipiece = int8(Piece.Pawn) then
                            flag <- true
                    if (Array.contains ipc Set_Piece_Can_Promote1) then
                        if (BB_Rev_Color_Position.[color] &&& ABB_Mask.[ifrom]) > System.UInt128() || (BB_Rev_Color_Position.[color] &&& ABB_Mask.[ito]) > System.UInt128() then
                            move <- Move.Init()
                            move <- Move.Pack(uint32(ifrom), uint32(ito), uint32(ipiece), uint32(abs(bt.contents.Board.[ito])), 1u)
                            let result = Do(bt, move, color)
                            bt.contents <- result.contents
                            if IsAttacked(bt.contents, int(sq_king), color) = System.UInt128() then
                                moves.contents.[move_index] <- move
                                move_index <- move_index + 1
                            let result = UnDo(bt, move, color)
                            bt.contents <- result.contents
                            if ipiece <> int8(Piece.Silver) then
                                flag <- true
                    if flag = false then
                        move <- Move.Init()
                        move <- Move.Pack(uint32(ifrom), uint32(ito), uint32(ipiece), uint32(abs(bt.contents.Board.[ito])), 0u)
                        let result = Do(bt, move, color)
                        bt.contents <- result.contents
                        if IsAttacked(bt.contents, int(sq_king), color) = System.UInt128() then
                            moves.contents.[move_index] <- move
                            move_index <- move_index + 1
                        let result = UnDo(bt, move, color)
                        bt.contents <- result.contents
        let checker = int(abs(bt.contents.Board.[sq_checker]))
        if Array.contains checker Set_Long_Attack_Pieces && (bb_checker &&& ABB_Piece_Attacks[color, int(Piece.King), int(sq_king)]) = System.UInt128() then
            let mutable bb_inter = ABB_Obstacles[int(sq_king), sq_checker]
            while bb_inter <> System.UInt128() do
                let ito = Square(bb_inter)
                bb_inter <- bb_inter ^^^ ABB_Mask.[ito]
                let mutable bb_defender = AttacksToPiece(bt.contents, ito, color)
                while bb_defender <> System.UInt128() do
                    let ifrom = Square(bb_defender)
                    bb_defender <- bb_defender ^^^ ABB_Mask.[ifrom]
                    if ifrom <> int(sq_king) then
                        ipiece <- abs(bt.contents.Board[ifrom])
                        let idirec = Adirec[ifrom, ito]
                        let ipc = int(ipiece)
                        flag <- false
                        if idirec = int(Direction.Direc_Misc) || IsPinnedOnKing(bt.contents, ifrom, idirec, color) = System.UInt128() then
                            if Array.contains ipc Set_Piece_Can_Promote0 then
                                if ipiece <> int8(Piece.Lance) && (ABB_Piece_Attacks[color, int(ipiece), ifrom] &&& BB_Rev_Color_Position[color]) > System.UInt128() then
                                    move <- Move.Init()
                                    move <- Move.Pack(uint32(ifrom), uint32(ito), uint32(ipiece), uint32(abs(bt.contents.Board.[ito])), 1u)
                                    moves.contents.[move_index] <- move
                                    move_index <- move_index + 1
                                else if ipiece = int8(Piece.Lance) then
                                    //bb_occupied <- bt.BB_Occupied[0] ||| bt.BB_Occupied[1]
                                    if ((ABB_Lance_Attacks[color, ifrom][ABB_File_Mask_Ex[ifrom] &&& bb_occupied]) > System.UInt128() && (BB_Rev_Color_Position[color] &&& ABB_Mask.[ito]) > System.UInt128()) then
                                        move <- Move.Init()
                                        move <- Move.Pack(uint32(ifrom), uint32(ito), uint32(ipiece), uint32(abs(bt.contents.Board.[ito])), 1u)
                                        moves.contents.[move_index] <- move
                                        move_index <- move_index + 1
                            if Array.contains ipc Set_Piece_Can_Promote1 then
                                if ((BB_Rev_Color_Position[color] &&& ABB_Mask.[ifrom]) > System.UInt128() || (BB_Rev_Color_Position[color] &&& ABB_Mask.[ito]) > System.UInt128()) then
                                    move <- Move.Init()
                                    move <- Move.Pack(uint32(ifrom), uint32(ito), uint32(ipiece), uint32(abs(bt.contents.Board.[ito])), 1u)
                                    moves.contents.[move_index] <- move
                                    move_index <- move_index + 1
                                    if ipiece <> int8(Piece.Silver) then
                                        flag <- true
                            if flag = false then
                                if (((ipiece = int8(Piece.Knight) || ipiece = int8(Piece.Lance)) && (BB_Knight_Must_Promote[color] &&& ABB_Mask.[ito]) > System.UInt128())) = false then
                                    move <- Move.Init()
                                    move <- Move.Pack(uint32(ifrom), uint32(ito), uint32(ipiece), uint32(abs(bt.contents.Board.[ito])), 0u)
                                    moves.contents.[move_index] <- move
                                    move_index <- move_index + 1                                        
        bb_empty <- ABB_Obstacles[int(sq_king), sq_checker]
        bb_piece_can_drop[int(Piece.Pawn)] <- System.UInt128()
        if ((bt.contents.Hand.[color] &&& Hand_Mask[int(Piece.Pawn)]) > 0) then
            for i = int(File.File1) to int(File.File9) do
                if ((BB_File[i] &&& bt.contents.BB_Piece[color, int(Piece.Pawn)]) <> System.UInt128()) then
                    let mutable bb = bb_occupied
                    bb <- bb_occupied ||| bb_empty ^^^ bt.contents.BB_Piece[color, int(Piece.Pawn)]
                    bb <- bb &&& BB_Pawn_Lance_Can_Drop[color] &&& bb_empty &&& BB_File[i]
                    bb_piece_can_drop[int(Piece.Pawn)] <- bb_piece_can_drop[int(Piece.Pawn)] ||| bb
            let sq = int(bt.contents.SQ_King[color]) + Delta_Table[color]
            if (sq >= 0 && sq < Square_NB) && bt.contents.Board.[sq] = int8(Piece.Empty) && (bb_piece_can_drop[int(Piece.Pawn)] &&& ABB_Mask.[sq]) = System.UInt128() then
                if IsMatePawnDrop(bt.contents, sq, color) then
                    bb_piece_can_drop[int(Piece.Pawn)]  <- bb_piece_can_drop[int(Piece.Pawn)] ^^^ ABB_Mask[sq]
        bb_piece_can_drop[int(Piece.Lance)] <- BB_Pawn_Lance_Can_Drop[color] &&& bb_empty
        bb_piece_can_drop[int(Piece.Knight)] <- BB_Knight_Can_Drop[color] &&& bb_empty
        bb_piece_can_drop[int(Piece.Silver)] <- BB_Others_Can_Drop &&& bb_empty
        bb_piece_can_drop[int(Piece.Gold)] <- bb_piece_can_drop[int(Piece.Silver)]
        bb_piece_can_drop[int(Piece.Bishop)] <- bb_piece_can_drop[int(Piece.Silver)]
        bb_piece_can_drop[int(Piece.Rook)] <- bb_piece_can_drop[int(Piece.Silver)]
        for i = int(Piece.Pawn) to int(Piece.Rook) do
            if ((bt.contents.Hand.[color] &&& Hand_Mask[i]) > 0) then
                let mutable bb_object = bb_piece_can_drop[i]
                while bb_object > System.UInt128() do
                    ifrom <- uint8(Square_NB + i - 1)
                    ito <- Square(bb_object)
                    bb_object <- bb_object ^^^ ABB_Mask.[ito]
                    move <- Move.Init()
                    move <- Move.Pack(uint32(ifrom), uint32(ito), uint32(i), 0u, 0u)
                    moves.contents.[move_index] <- move
                    move_index <- move_index + 1   
    move_index

let BehindAttacks(idirec:int, ik:int) :System.UInt128 = 
    if idirec = int(Direction.Direc_Diag1_U2d) then
        ABB_Diag1_Attacks[ik][System.UInt128()]
    elif idirec = int(Direction.Direc_Diag2_U2d) then
        ABB_Diag2_Attacks[ik][System.UInt128()]
    elif idirec = int(Direction.Direc_File_U2d) then
        ABB_File_Attacks[ik][System.UInt128()]
    elif idirec = int(Direction.Direc_Rank_L2r) then
        ABB_Rank_Attacks[ik][System.UInt128()]
    else
        System.UInt128()

let Xor (a: System.UInt128, b: System.UInt128) : System.UInt128 =
    a ^^^ b

let AddBehindAttacks(bb: System.UInt128, idirec:int, ik:int) :System.UInt128 = 
    let bb_tmp = BehindAttacks(idirec, ik)
    let bb_tmp2 = BB_Full &&& Xor(bb_tmp, bb_tmp)
    bb_tmp2 ||| bb

let GenCheck(bt: BoardTree, color:int, moves:uint32[] ref) = 
    //let mutable move_index = moves.contents.Length - 1
    let mutable move_index = 0
    let opponent_color = color ^^^ 1
    let sq_opponent_king = bt.SQ_King.[opponent_color]
    let sq_object = sq_opponent_king + uint8(Delta_Table.[opponent_color])
    let sq_pawn = sq_opponent_king + (uint8(2) * uint8(Delta_Table.[opponent_color]))
    let bb_occupied = bt.BB_Occupied.[0] ||| bt.BB_Occupied.[1]
    let bb_empty = bb_occupied ^^^ System.UInt128.MaxValue &&& BB_Full
    let bb_move_to = (bt.BB_Occupied[opponent_color] ||| bb_empty) &&& BB_Full
    if (sq_pawn >= uint8(0)) && (sq_pawn < uint8(Square_NB)) && (bt.Board.[int(sq_pawn)] = int8(Sign_Table[int(opponent_color)]) * int8(Piece.Pawn) && (ABB_Mask.[int(sq_pawn)] &&& BB_Pawn_Mask.[color]) > System.UInt128()) then
        let move = Move.Pack(uint32(sq_pawn), uint32(sq_object), uint32(Piece.Pawn), uint32(bt.Board[int(sq_object)]), 0ul)
        moves.contents.[move_index] <- move
        move_index <- move_index + 1
    let mutable bb_from = bt.BB_Piece[color, int(Piece.Pawn)]
    while bb_from > System.UInt128() do
        let ifrom = Square(bb_from)
        bb_from <- bb_from ^^^ ABB_Mask.[ifrom]
        let mutable bb_to = BB_Rev_Color_Position[color] &&& ABB_Piece_Attacks[color, int(Piece.Pawn), ifrom] &&& ABB_Piece_Attacks[opponent_color, int(Piece.King), int(sq_opponent_king)] &&& bb_move_to
        while bb_to > System.UInt128() do
            let ito = Square(bb_to)
            bb_to <- bb_to ^^^ ABB_Mask.[ito]
            let move = Move.Pack(uint32(ifrom), uint32(ito), uint32(Piece.Pawn), uint32(bt.Board[int(ito)]), 1ul)
            moves.contents.[move_index] <- move
            move_index <- move_index + 1
    bb_from <- ABB_Rank_Attacks[int(sq_opponent_king)][System.UInt128()] &&& bt.BB_Piece[color, int(Piece.Pawn)]
    while bb_from > System.UInt128() do
        let ifrom = Square(bb_from)
        bb_from <- bb_from ^^^ ABB_Mask.[ifrom]
        let bb_rd = bt.BB_Piece[color, int(Piece.Rook)] ||| bt.BB_Piece[color, int(Piece.Dragon)]
        let bb_temp = ABB_Rank_Attacks[ifrom][System.UInt128()] &&& bb_rd
        let bb_temp2 = ABB_Piece_Attacks[color, int(Piece.Pawn), ifrom] &&& bb_move_to
        if (bt.BB_Piece[color, int(Piece.Rook)] ||| bt.BB_Piece[color, int(Piece.Dragon)] &&& ABB_Rank_Attacks[ifrom][System.UInt128()]) > System.UInt128() && (ABB_Piece_Attacks[color, int(Piece.Pawn), ifrom] &&& bb_move_to) > System.UInt128() then
            let mutable flag_promo = uint32(0)
            if (BB_Rev_Color_Position[color] &&& ABB_Piece_Attacks[color, int(Piece.Pawn), ifrom]) <> System.UInt128() then
                flag_promo <- uint32(1)
            let ito = ifrom + Delta_Table[color]
            let move = Move.Pack(uint32(ifrom), uint32(ito), uint32(Piece.Pawn), uint32(bt.Board[int(ito)]), flag_promo)
            moves.contents.[move_index] <- move
            move_index <- move_index + 1
    bb_from <- ABB_Diag1_Attacks[int(sq_opponent_king)][System.UInt128()] &&& bt.BB_Piece[color, int(Piece.Pawn)]
    while bb_from > System.UInt128() do
        let ifrom = Square(bb_from)
        bb_from <- bb_from ^^^ ABB_Mask.[ifrom]
        let bb_bh = bt.BB_Piece[color, int(Piece.Bishop)] ||| bt.BB_Piece[color, int(Piece.Horse)]
        let bb_temp = ABB_Rank_Attacks[ifrom][System.UInt128()] &&& bb_bh
        let bb_temp2 = ABB_Piece_Attacks[color, int(Piece.Pawn), ifrom] &&& bb_move_to
        if (ABB_Diag1_Attacks[ifrom][System.UInt128()] &&& (bt.BB_Piece[color, int(Piece.Bishop)] ||| bt.BB_Piece[color, int(Piece.Horse)])) > System.UInt128() && ((ABB_Piece_Attacks[color, (int)Piece.Pawn, ifrom] &&& bb_move_to) > System.UInt128()) then
            let mutable flag_promo = uint32(0)
            if (BB_Rev_Color_Position[color] &&& ABB_Piece_Attacks[color, int(Piece.Pawn), ifrom]) <> System.UInt128() then
                flag_promo <- uint32(1)
            let ito = ifrom + Delta_Table[color]
            let move = Move.Pack(uint32(ifrom), uint32(ito), uint32(Piece.Pawn), uint32(bt.Board[int(ito)]), flag_promo)
            moves.contents.[move_index] <- move
            move_index <- move_index + 1
    bb_from <- ABB_Diag2_Attacks[int(sq_opponent_king)][System.UInt128()] &&& bt.BB_Piece[color, int(Piece.Pawn)]
    while bb_from > System.UInt128() do
        let ifrom = Square(bb_from)
        bb_from <- bb_from ^^^ ABB_Mask.[ifrom]
        let bb_bh = bt.BB_Piece[color, int(Piece.Bishop)] ||| bt.BB_Piece[color, int(Piece.Horse)]
        let bb_temp = ABB_Rank_Attacks[ifrom][System.UInt128()] &&& bb_bh
        let bb_temp2 = ABB_Piece_Attacks[color, int(Piece.Pawn), ifrom] &&& bb_move_to
        if (ABB_Diag2_Attacks[ifrom][System.UInt128()] &&& (bt.BB_Piece[color, int(Piece.Bishop)] ||| bt.BB_Piece[color, int(Piece.Horse)])) > System.UInt128() && ((ABB_Piece_Attacks[color, (int)Piece.Pawn, ifrom] &&& bb_move_to) > System.UInt128()) then
            let mutable flag_promo = uint32(0)
            if (BB_Rev_Color_Position[color] &&& ABB_Piece_Attacks[color, int(Piece.Pawn), ifrom]) <> System.UInt128() then
                flag_promo <- uint32(1)
            let ito = ifrom + Delta_Table[color]
            let move = Move.Pack(uint32(ifrom), uint32(ito), uint32(Piece.Pawn), uint32(bt.Board[int(ito)]), flag_promo)
            moves.contents.[move_index] <- move
            move_index <- move_index + 1
    let mutable bb_temp = System.UInt128()
    if sq_object > uint8(0) && sq_object < uint8(Square_NB) then
        bb_temp <- BB_File[int(FileTable[int(sq_object)])] &&& bt.BB_Piece[color, int(Piece.Pawn)]
    if bb_temp > System.UInt128() && sq_object > uint8(0) && sq_object < uint8(Square_NB) && (bt.Hand[color] &&& Hand_Mask[int(Piece.Pawn)]) > 0 && bt.Board[int(sq_object)] = int8(Piece.Empty) && IsMatePawnDrop(bt, int(sq_object), opponent_color) = false then
        let move = Move.Pack(uint32(Square_NB + int(Piece.Pawn) - 1), uint32(sq_object), uint32(Piece.Pawn), 0u, 0u)
        moves.contents.[move_index] <- move
        move_index <- move_index + 1
    bb_from <- bt.BB_Piece[color, int(Piece.Silver)]
    while bb_from > System.UInt128() do
        let ifrom = Square(bb_from)
        bb_from <- bb_from ^^^ ABB_Mask.[ifrom]
        let idirec = Adirec[int(sq_opponent_king), ifrom]
        let mutable bb_to = ABB_Piece_Attacks[color, int(Piece.Silver), ifrom] &&& ABB_Piece_Attacks[opponent_color, int(Piece.Silver), int(sq_opponent_king)] &&& bb_move_to
        if idirec <> int(Direction.Direc_Misc) && IsPinnedOnKing(bt, ifrom, idirec, opponent_color) > System.UInt128() then
            bb_temp <- System.UInt128()
            bb_to <- bb_to ||| AddBehindAttacks(bb_temp, idirec, int(sq_opponent_king)) &&& ABB_Piece_Attacks[color, (int)Piece.Silver, ifrom] &&& bb_move_to
        while bb_to > System.UInt128() do
            let ito = Square(bb_to)
            bb_to <- bb_to ^^^ ABB_Mask.[ito]
            let move = Move.Pack(uint32(ifrom), uint32(ito), uint32(Piece.Silver), uint32(abs(bt.Board[ito])), 0u)
            moves.contents.[move_index] <- move
            move_index <- move_index + 1
    bb_from <- bt.BB_Piece[color, int(Piece.Silver)]
    while bb_from > System.UInt128() do
        let ifrom = Square(bb_from)
        bb_from <- bb_from ^^^ ABB_Mask.[ifrom]
        let idirec = Adirec[int(sq_opponent_king), ifrom]
        let mutable bb_to = ABB_Piece_Attacks[color, int(Piece.Silver), ifrom] &&& ABB_Piece_Attacks[opponent_color, int(Piece.Gold), int(sq_opponent_king)] &&& bb_move_to
        if idirec <> int(Direction.Direc_Misc) && IsPinnedOnKing(bt, ifrom, idirec, opponent_color) > System.UInt128() then
            bb_temp <- System.UInt128()
            bb_to <- bb_to ||| AddBehindAttacks(bb_temp, idirec, int(sq_opponent_king)) &&& ABB_Piece_Attacks[color, (int)Piece.Silver, ifrom] &&& bb_move_to
        while bb_to > System.UInt128() do
            let ito = Square(bb_to)
            bb_to <- bb_to ^^^ ABB_Mask.[ito]
            let move = Move.Pack(uint32(ifrom), uint32(ito), uint32(Piece.Silver), uint32(abs(bt.Board[ito])), 1u)
            moves.contents.[move_index] <- move
            move_index <- move_index + 1
    if (bt.Hand[color] &&& Hand_Mask[int(Piece.Silver)]) > 0 then
        let mutable bb_to = ABB_Piece_Attacks[opponent_color, int(Piece.Silver), int(sq_opponent_king)] &&& bb_empty
        while bb_to > System.UInt128() do
            let ito = Square(bb_to)
            bb_to <- bb_to ^^^ ABB_Mask.[ito]
            let move = Move.Pack(uint32(Square_NB + int(Piece.Silver) - 1), uint32(ito), uint32(Piece.Silver), 0u, 0u)
            moves.contents.[move_index] <- move
            move_index <- move_index + 1
    bb_from <- bt.BB_Piece[color, int(Piece.Gold)] ||| bt.BB_Piece[color, int(Piece.Pro_Pawn)] ||| bt.BB_Piece[color, int(Piece.Pro_Lance)] ||| bt.BB_Piece[color, int(Piece.Pro_Knight)] ||| bt.BB_Piece[color, int(Piece.Pro_Silver)]
    while bb_from > System.UInt128() do
        let ifrom = Square(bb_from)
        bb_from <- bb_from ^^^ ABB_Mask.[ifrom]
        let idirec = Adirec[int(sq_opponent_king), ifrom]
        let mutable bb_to = ABB_Piece_Attacks[color, int(Piece.Gold), ifrom] &&& ABB_Piece_Attacks[opponent_color, int(Piece.Gold), int(sq_opponent_king)] &&& bb_move_to
        while bb_to > System.UInt128() do
            let ito = Square(bb_to)
            bb_to <- bb_to ^^^ ABB_Mask.[ito]
            let move = Move.Pack(uint32(ifrom), uint32(ito), uint32(abs(bt.Board[ifrom])), uint32(abs(bt.Board[ito])), 0u)
            moves.contents.[move_index] <- move
            move_index <- move_index + 1
    if (bt.Hand[color] &&& Hand_Mask[int(Piece.Gold)]) > 0 then
        let mutable bb_to = ABB_Piece_Attacks[opponent_color, int(Piece.Gold), int(sq_opponent_king)] &&& bb_empty
        while bb_to > System.UInt128() do
            let ito = Square(bb_to)
            bb_to <- bb_to ^^^ ABB_Mask.[ito]
            let move = Move.Pack(uint32(Square_NB + int(Piece.Gold) - 1), uint32(ito), uint32(Piece.Gold), 0u, 0u)
            moves.contents.[move_index] <- move
            move_index <- move_index + 1
    bb_from <- bt.BB_Piece[color, int(Piece.Knight)]
    while bb_from > System.UInt128() do
        let ifrom = Square(bb_from)
        bb_from <- bb_from ^^^ ABB_Mask.[ifrom]
        let idirec = Adirec[int(sq_opponent_king), ifrom]
        let mutable bb_to = ABB_Piece_Attacks[color, int(Piece.Knight), ifrom] &&& ABB_Piece_Attacks[opponent_color, int(Piece.Knight), int(sq_opponent_king)] &&& bb_move_to
        if idirec <> int(Direction.Direc_Misc) && IsPinnedOnKing(bt, ifrom, idirec, opponent_color) > System.UInt128() then
            bb_temp <- System.UInt128()
            bb_to <- bb_to ||| AddBehindAttacks(bb_temp, idirec, int(sq_opponent_king)) &&& ABB_Piece_Attacks[color, (int)Piece.Knight, ifrom] &&& bb_move_to
        while bb_to > System.UInt128() do
            let ito = Square(bb_to)
            bb_to <- bb_to ^^^ ABB_Mask.[ito]
            let move = Move.Pack(uint32(ifrom), uint32(ito), uint32(Piece.Knight), uint32(abs(bt.Board[ito])), 0u)
            moves.contents.[move_index] <- move
            move_index <- move_index + 1
    bb_from <- bt.BB_Piece[color, int(Piece.Knight)]
    while bb_from > System.UInt128() do
        let ifrom = Square(bb_from)
        bb_from <- bb_from ^^^ ABB_Mask.[ifrom]
        let idirec = Adirec[int(sq_opponent_king), ifrom]
        let mutable bb_to = ABB_Piece_Attacks[color, int(Piece.Knight), ifrom] &&& ABB_Piece_Attacks[opponent_color, int(Piece.Gold), int(sq_opponent_king)] &&& bb_move_to
        if idirec <> int(Direction.Direc_Misc) && IsPinnedOnKing(bt, ifrom, idirec, opponent_color) > System.UInt128() then
            bb_temp <- System.UInt128()
            bb_to <- bb_to ||| AddBehindAttacks(bb_temp, idirec, int(sq_opponent_king)) &&& ABB_Piece_Attacks[color, (int)Piece.Knight, ifrom] &&& bb_move_to
        while bb_to > System.UInt128() do
            let ito = Square(bb_to)
            bb_to <- bb_to ^^^ ABB_Mask.[ito]
            let move = Move.Pack(uint32(ifrom), uint32(ito), uint32(Piece.Knight), uint32(abs(bt.Board[ito])), 1u)
            moves.contents.[move_index] <- move
            move_index <- move_index + 1
    if (bt.Hand[color] &&& Hand_Mask[int(Piece.Knight)]) > 0 then
        let mutable bb_to = ABB_Piece_Attacks[opponent_color, int(Piece.Knight), int(sq_opponent_king)] &&& bb_empty
        while bb_to > System.UInt128() do
            let ito = Square(bb_to)
            bb_to <- bb_to ^^^ ABB_Mask.[ito]
            let move = Move.Pack(uint32(Square_NB + int(Piece.Knight) - 1), uint32(ito), uint32(Piece.Knight), 0u, 0u)
            moves.contents.[move_index] <- move
            move_index <- move_index + 1
    let ifrom = int(bt.SQ_King[color])
    let idirec = Adirec[int(sq_opponent_king), ifrom]
    if idirec <> int(Direction.Direc_Misc) && IsPinnedOnKing(bt, ifrom, idirec, opponent_color) > System.UInt128() then
        bb_temp <- System.UInt128()
        let mutable bb_to = AddBehindAttacks(bb_temp, idirec, int(sq_opponent_king)) &&& ABB_Piece_Attacks[color, (int)Piece.King, ifrom] &&& bb_move_to
        while bb_to > System.UInt128() do
            let ito = Square(bb_to)
            bb_to <- bb_to ^^^ ABB_Mask.[ito]
            let move = Move.Pack(uint32(ifrom), uint32(ito), uint32(Piece.King), uint32(abs(bt.Board[ito])), 0u)
            moves.contents.[move_index] <- move
            move_index <- move_index + 1
    bb_from <- bt.BB_Piece[color, int(Piece.Lance)]
    while bb_from > System.UInt128() do
        let ifrom = Square(bb_from)
        bb_from <- bb_from ^^^ ABB_Mask.[ifrom]
        let mutable bb_to = ABB_Lance_Attacks[color, ifrom][ABB_Lance_Mask_Ex[color, ifrom] &&& bb_occupied] &&& (BB_Not_Knight_Must_Promote[color] &&& BB_Full &&& bt.BB_Occupied[opponent_color] &&& bb_move_to)
        let mutable bb_attacks = bb_to
        bb_to <- bb_to &&& ABB_Lance_Attacks[opponent_color, int(sq_opponent_king)][ABB_Lance_Mask_Ex[opponent_color, int(sq_opponent_king)] &&& bb_occupied]
        let idirec = Adirec[int(sq_opponent_king), ifrom]
        if idirec <> int(Direction.Direc_Misc) && IsPinnedOnKing(bt, ifrom, idirec, opponent_color) > System.UInt128() then
            bb_temp <- bb_attacks &&& AddBehindAttacks(bb_temp, idirec, int(sq_opponent_king))
            bb_to <- bb_to ||| bb_temp
            if color = int(Color.Black) then
                bb_to <- bb_to &&& (BB_File[int(FileTable[ifrom])] &&& (BB_Rank[2] ||| BB_Rank[3]))
            else
                bb_to <- bb_to &&& (BB_File[int(FileTable[ifrom])] &&& (BB_Rank[6] ||| BB_Rank[5]))
            while bb_to > System.UInt128() do
                let ito = Square(bb_to)
                bb_to <- bb_to ^^^ ABB_Mask.[ito]
                let move = Move.Pack(uint32(ifrom), uint32(ito), uint32(Piece.Lance), uint32(abs(bt.Board[ito])), 0u)
                moves.contents.[move_index] <- move
                move_index <- move_index + 1
    bb_from <- bt.BB_Piece[color, int(Piece.Lance)]
    while bb_from > System.UInt128() do
        let ifrom = Square(bb_from)
        bb_from <- bb_from ^^^ ABB_Mask.[ifrom]
        let mutable bb_to = ABB_Lance_Attacks[color, ifrom][ABB_Lance_Mask_Ex[color, ifrom] &&& bb_occupied] &&& (BB_Not_Knight_Must_Promote[color] &&& BB_Full &&& bt.BB_Occupied[opponent_color] &&& bb_move_to)
        let mutable bb_attacks = bb_to
        bb_to <- bb_to &&& BB_Rev_Color_Position[color] &&& BB_Full &&& ABB_Piece_Attacks[opponent_color, int(Piece.Gold), int(sq_opponent_king)] &&& bb_move_to
        bb_to <- bb_to &&& ABB_Lance_Attacks[opponent_color, int(sq_opponent_king)][ABB_Lance_Mask_Ex[opponent_color, int(sq_opponent_king)] &&& bb_occupied]
        let idirec = Adirec[int(sq_opponent_king), ifrom]
        if idirec <> int(Direction.Direc_Misc) && IsPinnedOnKing(bt, ifrom, idirec, opponent_color) > System.UInt128() then
            bb_temp <- bb_attacks &&& AddBehindAttacks(bb_temp, idirec, int(sq_opponent_king))
            bb_to <- bb_to ||| bb_temp
            bb_to <- bb_to &&& BB_Color_Position[int(Common.Color.Black)] ||| BB_Color_Position[int(Common.Color.White)]
        while bb_to > System.UInt128() do
            let ito = Square(bb_to)
            bb_to <- bb_to ^^^ ABB_Mask.[ito]
            let move = Move.Pack(uint32(ifrom), uint32(ito), uint32(Piece.Lance), uint32(abs(bt.Board[ito])), 1u)
            moves.contents.[move_index] <- move
            move_index <- move_index + 1
    if (bt.Hand[color] &&& Hand_Mask[int(Piece.Lance)]) > 0 then
        let mutable bb_to = ABB_Lance_Attacks[opponent_color, int(sq_opponent_king)][ABB_Lance_Mask_Ex[opponent_color, int(sq_opponent_king)] &&& bb_occupied] &&& bb_empty
        while bb_to > System.UInt128() do
            let ito = Square(bb_to)
            bb_to <- bb_to ^^^ ABB_Mask.[ito]
            let move = Move.Pack(uint32(Square_NB + int(Piece.Lance) - 1), uint32(ito), uint32(Piece.Lance), 0u, 0u)
            moves.contents.[move_index] <- move
            move_index <- move_index + 1
    bb_from <- bt.BB_Piece[color, int(Piece.Rook)] &&& (BB_Color_Position[color] ||| BB_DMZ)
    while bb_from > System.UInt128() do
        let ifrom = Square(bb_from)
        bb_from <- bb_from ^^^ ABB_Mask.[ifrom]
        let mutable bb_to = ABB_Cross_Attacks[ifrom][ABB_Cross_Mask_Ex[ifrom] &&& bb_occupied]
        let mutable bb_attacks = bb_to
        bb_to <- bb_to &&& bb_move_to
        let idirec = Adirec[int(sq_opponent_king), ifrom]
        bb_to <- bb_to &&& ABB_Cross_Attacks[int(sq_opponent_king)][ABB_Cross_Mask_Ex[int(sq_opponent_king)] &&& bb_occupied]
        bb_to <- bb_to &&& (BB_Color_Position[color] ||| BB_DMZ)
        if idirec <> int(Direction.Direc_Misc) && IsPinnedOnKing(bt, ifrom, idirec, opponent_color) > System.UInt128() then
            bb_temp <- bb_attacks &&& AddBehindAttacks(bb_temp, idirec, int(sq_opponent_king))
            bb_to <- bb_to ||| bb_temp
        while bb_to > System.UInt128() do
            let ito = Square(bb_to)
            bb_to <- bb_to ^^^ ABB_Mask.[ito]
            let move = Move.Pack(uint32(ifrom), uint32(ito), uint32(Piece.Rook), uint32(abs(bt.Board[ito])), 0u)
            moves.contents.[move_index] <- move
            move_index <- move_index + 1
    bb_from <- bt.BB_Piece[color, int(Piece.Rook)]
    while bb_from > System.UInt128() do
        let ifrom = Square(bb_from)
        bb_from <- bb_from ^^^ ABB_Mask.[ifrom]
        let mutable bb_to = ABB_Cross_Attacks[ifrom][ABB_Cross_Mask_Ex[ifrom] &&& bb_occupied]
        let mutable bb_attacks = bb_to
        bb_to <- bb_to &&& bb_move_to
        let idirec = Adirec[int(sq_opponent_king), ifrom]
        bb_to <- bb_to &&& (ABB_Cross_Attacks[int(sq_opponent_king)][ABB_Cross_Mask_Ex[int(sq_opponent_king)] &&& bb_occupied] ||| ABB_Piece_Attacks[opponent_color, int(Piece.King), int(sq_opponent_king)])
        if idirec <> int(Direction.Direc_Misc) && IsPinnedOnKing(bt, ifrom, idirec, opponent_color) > System.UInt128() then
            bb_temp <- bb_attacks &&& AddBehindAttacks(bb_temp, idirec, int(sq_opponent_king))
            bb_to <- bb_to ||| bb_temp
        while bb_to > System.UInt128() do
            let ito = Square(bb_to)
            bb_to <- bb_to ^^^ ABB_Mask.[ito]
            let move = Move.Pack(uint32(ifrom), uint32(ito), uint32(Piece.Rook), uint32(abs(bt.Board[ito])), 1u)
            moves.contents.[move_index] <- move
            move_index <- move_index + 1
    if (bt.Hand[color] &&& Hand_Mask[int(Piece.Rook)]) > 0 then
        let mutable bb_to = ABB_Cross_Attacks[int(sq_opponent_king)][ABB_Cross_Mask_Ex[int(sq_opponent_king)] &&& bb_occupied] &&& bb_empty
        while bb_to > System.UInt128() do
            let ito = Square(bb_to)
            bb_to <- bb_to ^^^ ABB_Mask.[ito]
            let move = Move.Pack(uint32(Square_NB + int(Piece.Rook) - 1), uint32(ito), uint32(Piece.Rook), 0u, 0u)
            moves.contents.[move_index] <- move
            move_index <- move_index + 1
    bb_from <- bt.BB_Piece[color, int(Piece.Bishop)] &&& (BB_Color_Position[color] ||| BB_DMZ)
    while bb_from > System.UInt128() do
        let ifrom = Square(bb_from)
        bb_from <- bb_from ^^^ ABB_Mask.[ifrom]
        let mutable bb_to = ABB_Diagonal_Attacks[ifrom][ABB_Diagonal_Mask_Ex[ifrom] &&& bb_occupied]
        let mutable bb_attacks = bb_to
        bb_to <- bb_to &&& bb_move_to
        let idirec = Adirec[int(sq_opponent_king), ifrom]
        bb_to <- bb_to &&& ABB_Diagonal_Attacks[int(sq_opponent_king)][ABB_Diagonal_Mask_Ex[int(sq_opponent_king)] &&& bb_occupied]
        bb_to <- bb_to &&& (BB_Color_Position[color] ||| BB_DMZ)
        if idirec <> int(Direction.Direc_Misc) && IsPinnedOnKing(bt, ifrom, idirec, opponent_color) > System.UInt128() then
            bb_temp <- bb_attacks &&& AddBehindAttacks(bb_temp, idirec, int(sq_opponent_king))
            bb_to <- bb_to ||| bb_temp
        while bb_to > System.UInt128() do
            let ito = Square(bb_to)
            bb_to <- bb_to ^^^ ABB_Mask.[ito]
            let move = Move.Pack(uint32(ifrom), uint32(ito), uint32(Piece.Bishop), uint32(abs(bt.Board[ito])), 0u)
            moves.contents.[move_index] <- move
            move_index <- move_index + 1
    bb_from <- bt.BB_Piece[color, int(Piece.Bishop)]
    while bb_from > System.UInt128() do
        let ifrom = Square(bb_from)
        bb_from <- bb_from ^^^ ABB_Mask.[ifrom]
        let mutable bb_to = ABB_Diagonal_Attacks[ifrom][ABB_Diagonal_Mask_Ex[ifrom] &&& bb_occupied]
        let mutable bb_attacks = bb_to
        bb_to <- bb_to &&& bb_move_to
        let idirec = Adirec[int(sq_opponent_king), ifrom]
        bb_to <- bb_to &&& (ABB_Diagonal_Attacks[int(sq_opponent_king)][ABB_Diagonal_Mask_Ex[int(sq_opponent_king)] &&& bb_occupied] ||| ABB_Piece_Attacks[opponent_color, int(Piece.King), int(sq_opponent_king)])
        if idirec <> int(Direction.Direc_Misc) && IsPinnedOnKing(bt, ifrom, idirec, opponent_color) > System.UInt128() then
            bb_temp <- bb_attacks &&& AddBehindAttacks(bb_temp, idirec, int(sq_opponent_king))
            bb_to <- bb_to ||| bb_temp
        while bb_to > System.UInt128() do
            let ito = Square(bb_to)
            bb_to <- bb_to ^^^ ABB_Mask.[ito]
            let move = Move.Pack(uint32(ifrom), uint32(ito), uint32(Piece.Bishop), uint32(abs(bt.Board[ito])), 1u)
            moves.contents.[move_index] <- move
            move_index <- move_index + 1
    if (bt.Hand[color] &&& Hand_Mask[int(Piece.Bishop)]) > 0 then
        let mutable bb_to = ABB_Diagonal_Attacks[int(sq_opponent_king)][ABB_Diagonal_Mask_Ex[int(sq_opponent_king)] &&& bb_occupied] &&& bb_empty
        while bb_to > System.UInt128() do
            let ito = Square(bb_to)
            bb_to <- bb_to ^^^ ABB_Mask.[ito]
            let move = Move.Pack(uint32(Square_NB + int(Piece.Bishop) - 1), uint32(ito), uint32(Piece.Bishop), 0u, 0u)
            moves.contents.[move_index] <- move
            move_index <- move_index + 1
    bb_from <- bt.BB_Piece[color, int(Piece.Dragon)]
    while bb_from > System.UInt128() do
        let ifrom = Square(bb_from)
        bb_from <- bb_from ^^^ ABB_Mask.[ifrom]
        let mutable bb_to = ABB_Cross_Attacks[ifrom][ABB_Cross_Mask_Ex[ifrom] &&& bb_occupied] ||| ABB_Piece_Attacks[color, int(Piece.King), ifrom]
        let mutable bb_attacks = bb_to
        bb_to <- bb_to &&& bb_move_to
        let idirec = Adirec[int(sq_opponent_king), ifrom]
        bb_to <- bb_to &&& ABB_Cross_Attacks[int(sq_opponent_king)][ABB_Cross_Mask_Ex[int(sq_opponent_king)] &&& bb_occupied] ||| ABB_Piece_Attacks[color, int(Piece.King), ifrom]
        if idirec <> int(Direction.Direc_Misc) && IsPinnedOnKing(bt, ifrom, idirec, opponent_color) > System.UInt128() then
            bb_temp <- bb_attacks &&& AddBehindAttacks(bb_temp, idirec, int(sq_opponent_king))
            bb_to <- bb_to ||| bb_temp
        while bb_to > System.UInt128() do
            let ito = Square(bb_to)
            bb_to <- bb_to ^^^ ABB_Mask.[ito]
            let move = Move.Pack(uint32(ifrom), uint32(ito), uint32(Piece.Dragon), uint32(abs(bt.Board[ito])), 0u)
            moves.contents.[move_index] <- move
            move_index <- move_index + 1
    bb_from <- bt.BB_Piece[color, int(Piece.Horse)]
    while bb_from > System.UInt128() do
        let ifrom = Square(bb_from)
        bb_from <- bb_from ^^^ ABB_Mask.[ifrom]
        let mutable bb_to = ABB_Diagonal_Attacks[ifrom][ABB_Diagonal_Mask_Ex[ifrom] &&& bb_occupied] ||| ABB_Piece_Attacks[color, int(Piece.King), ifrom]
        let mutable bb_attacks = bb_to
        bb_to <- bb_to &&& bb_move_to
        let idirec = Adirec[int(sq_opponent_king), ifrom]
        bb_to <- bb_to &&& ABB_Diagonal_Attacks[int(sq_opponent_king)][ABB_Diagonal_Mask_Ex[int(sq_opponent_king)] &&& bb_occupied] ||| ABB_Piece_Attacks[color, int(Piece.King), ifrom]
        if idirec <> int(Direction.Direc_Misc) && IsPinnedOnKing(bt, ifrom, idirec, opponent_color) > System.UInt128() then
            bb_temp <- bb_attacks &&& AddBehindAttacks(bb_temp, idirec, int(sq_opponent_king))
            bb_to <- bb_to ||| bb_temp
        while bb_to > System.UInt128() do
            let ito = Square(bb_to)
            bb_to <- bb_to ^^^ ABB_Mask.[ito]
            let move = Move.Pack(uint32(ifrom), uint32(ito), uint32(Piece.Horse), uint32(abs(bt.Board[ito])), 0u)
            moves.contents.[move_index] <- move
            move_index <- move_index + 1
    move_index
