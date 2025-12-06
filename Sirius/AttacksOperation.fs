[<AutoOpen>]
module AttacksOperation

let IsPinnedOnKing(bt:BoardTree, sq:int, idirec:int, color:int):System.UInt128 =
    let bb_occupied = bt.BB_Occupied.[0] ||| bt.BB_Occupied.[1]
    let direc_abs = abs(idirec)
    let mutable bb_attacks = System.UInt128()
    let mutable bb_ret = System.UInt128()
    if direc_abs = int(Direction.Direc_File_U2d) then
        bb_attacks <- ABB_File_Attacks[sq][ABB_File_Mask_Ex[sq] &&& bb_occupied]
        if bb_attacks &&& ABB_Mask.[int(bt.SQ_King[color])] > System.UInt128() then
            bb_ret <- bb_attacks &&& (bt.BB_Piece[color ^^^ 1, (int)Piece.Rook] ||| bt.BB_Piece[color ^^^ 1, (int)Piece.Dragon] ||| bt.BB_Piece[color ^^^ 1, (int)Piece.Lance])
    else if direc_abs = int(Direction.Direc_Rank_L2r) then
        bb_attacks <- ABB_Rank_Attacks[sq][ABB_Rank_Mask_Ex[sq] &&& bb_occupied]
        if bb_attacks &&& ABB_Mask.[int(bt.SQ_King[color])] > System.UInt128() then
            bb_ret <- bb_attacks &&& (bt.BB_Piece[color ^^^ 1, (int)Piece.Rook] ||| bt.BB_Piece[color ^^^ 1, (int)Piece.Dragon])
    else if direc_abs = int(Direction.Direc_Diag1_U2d) then
        bb_attacks <- ABB_Diag1_Attacks[sq][ABB_Diag1_Mask_Ex[sq] &&& bb_occupied]
        if bb_attacks &&& ABB_Mask.[int(bt.SQ_King[color])] > System.UInt128() then
            bb_ret <- bb_attacks &&& (bt.BB_Piece[color ^^^ 1, (int)Piece.Bishop] ||| bt.BB_Piece[color ^^^ 1, (int)Piece.Horse])
     else if direc_abs = int(Direction.Direc_Diag2_U2d) then
        bb_attacks <- ABB_Diag2_Attacks[sq][ABB_Diag2_Mask_Ex[sq] &&& bb_occupied]
        if bb_attacks &&& ABB_Mask.[int(bt.SQ_King[color])] > System.UInt128() then
            bb_ret <- bb_attacks &&& (bt.BB_Piece[color ^^^ 1, (int)Piece.Bishop] ||| bt.BB_Piece[color ^^^ 1, (int)Piece.Horse])      
    bb_ret

let AttacksToPiece(bt:BoardTree, sq:int, color:int):System.UInt128 = 
    let bb_occupied = bt.BB_Occupied[0] ||| bt.BB_Occupied[1]
    let mutable bb_ret = bt.BB_Piece[color, int(Piece.Pawn)] &&& ABB_Piece_Attacks[color ^^^ 1, int(Piece.Pawn), sq]
    bb_ret <- bb_ret ||| (bt.BB_Piece[color, int(Piece.Knight)] &&& ABB_Piece_Attacks[color ^^^ 1, int(Piece.Knight), sq])
    bb_ret <- bb_ret ||| (bt.BB_Piece[color, int(Piece.Silver)] &&& ABB_Piece_Attacks[color ^^^ 1, int(Piece.Silver), sq])
    let bb_total_gold = bt.BB_Piece[color, int(Piece.Gold)] ||| bt.BB_Piece[color, int(Piece.Pro_Pawn)] ||| bt.BB_Piece[color, int(Piece.Pro_Lance)] ||| bt.BB_Piece[color, int(Piece.Pro_Knight)] ||| bt.BB_Piece[color, int(Piece.Pro_Silver)]
    bb_ret <- bb_ret ||| (bb_total_gold &&& ABB_Piece_Attacks[color ^^^ 1, int(Piece.Gold), sq])
    let bb_hdk = bt.BB_Piece[color, int(Piece.Horse)] ||| bt.BB_Piece[color, int(Piece.Dragon)] ||| bt.BB_Piece[color, int(Piece.King)]
    bb_ret <- bb_ret ||| (bb_hdk &&& ABB_Piece_Attacks[color ^^^ 1, int(Piece.King), sq])
    let bb_bh = bt.BB_Piece[color, int(Piece.Bishop)] ||| bt.BB_Piece[color, int(Piece.Horse)]
    bb_ret <- bb_ret ||| ( bb_bh &&& ABB_Diagonal_Attacks[sq][ABB_Diagonal_Mask_Ex[sq] &&& bb_occupied])
    let bb_rd = bt.BB_Piece[color, int(Piece.Rook)] ||| bt.BB_Piece[color, int(Piece.Dragon)]
    bb_ret <- bb_ret ||| (bb_rd &&& ABB_Cross_Attacks[sq][ABB_Cross_Mask_Ex[sq] &&& bb_occupied])
    let bb_lance_attacks = ABB_Lance_Attacks[color ^^^ 1, sq][ABB_Lance_Mask_Ex[color ^^^ 1, sq] &&& bb_occupied]
    bb_ret <- bb_ret ||| ( bt.BB_Piece[color, int(Piece.Lance)] &&& bb_lance_attacks)
    bb_ret
        
let AttacksToLongPiece(bt:BoardTree, sq:int, color:int):System.UInt128 = 
    let bb_occupied = bt.BB_Occupied[0] ||| bt.BB_Occupied[1]
    let bb_bh = bt.BB_Piece[color, int(Piece.Bishop)] ||| bt.BB_Piece[color, int(Piece.Horse)]
    let mutable bb_ret = ( bb_bh &&& ABB_Diagonal_Attacks[sq][ABB_Diagonal_Mask_Ex[sq] &&& bb_occupied])
    let bb_rd = bt.BB_Piece[color, int(Piece.Rook)] ||| bt.BB_Piece[color, int(Piece.Dragon)]
    bb_ret <- bb_ret ||| (bb_rd &&& ABB_Cross_Attacks[sq][ABB_Cross_Mask_Ex[sq] &&& bb_occupied])
    let bb_lance_attacks = ABB_Lance_Attacks[color ^^^ 1, sq][ABB_Lance_Mask_Ex[color ^^^ 1, sq] &&& bb_occupied]
    bb_ret <- bb_ret ||| ( bt.BB_Piece[color, int(Piece.Lance)] &&& bb_lance_attacks)
    bb_ret

let IsDiscoverKing(bt: BoardTree, ifrom:int, ito:int, color:int):bool = 
    let idirec = Adirec[ifrom, ito]
    let mutable bret:bool = false
    if idirec <> int(Direction.Direc_Misc) && idirec <> Adirec[int(bt.SQ_King[color]), ito] && IsPinnedOnKing(bt, ifrom, idirec, color) <> System.UInt128() then
        bret <- true
    else
        bret <- false
    bret

let IsDiscoverKing2(bt: BoardTree, ifrom:int, ito:int, color:int, ipiece:int):bool = 
    let idirec = Adirec[ifrom, ito]
    bt.BB_Piece[color, ipiece] <- bt.BB_Piece[color, ipiece] ^^^ ABB_Mask.[ifrom]
    bt.BB_Occupied[color] <- bt.BB_Occupied[color] ^^^ ABB_Mask.[ifrom]
    let mutable bret:bool = false
    if idirec <> int(Direction.Direc_Misc) && idirec <> Adirec[int(bt.SQ_King[color]), ito] && IsPinnedOnKing(bt, ifrom, idirec, color) <> System.UInt128() then
        bt.BB_Piece[color, ipiece] <- bt.BB_Piece[color, ipiece] ^^^ ABB_Mask.[ifrom]
        bt.BB_Occupied[color] <- bt.BB_Occupied[color] ^^^ ABB_Mask.[ifrom]
        bret <- true
    else
        bt.BB_Piece[color, ipiece] <- bt.BB_Piece[color, ipiece] ^^^ ABB_Mask.[ifrom]
        bt.BB_Occupied[color] <- bt.BB_Occupied[color] ^^^ ABB_Mask.[ifrom]
        bret <- false
    bret

let IsAttacked(bt:BoardTree, sq:int, color:int):System.UInt128 = 
    let mutable bb_ret = System.UInt128()
    let bb_occupied = bt.BB_Occupied[0] ||| bt.BB_Occupied[1]
    if (sq + Delta_Table[color]) >= 0 && (sq + Delta_Table[color]) < Square_NB then
        if bt.Board[sq + Delta_Table[color]] = int8(Sign_Table[color] * int(Piece.Pawn)) then
            bb_ret <- ABB_Mask[sq + Delta_Table[color]]
    bb_ret <- bb_ret ||| (bt.BB_Piece[color ^^^ 1, int(Piece.Knight)] &&& ABB_Piece_Attacks[color, int(Piece.Knight), sq])
    bb_ret <- bb_ret ||| (bt.BB_Piece[color ^^^ 1, int(Piece.Silver)] &&& ABB_Piece_Attacks[color, int(Piece.Silver), sq])
    let bb_total_gold = bt.BB_Piece[color ^^^ 1, int(Piece.Gold)] ||| bt.BB_Piece[color ^^^ 1, int(Piece.Pro_Pawn)] ||| bt.BB_Piece[color ^^^ 1, int(Piece.Pro_Lance)] ||| bt.BB_Piece[color ^^^ 1, int(Piece.Pro_Knight)] ||| bt.BB_Piece[color ^^^ 1, int(Piece.Pro_Silver)]
    bb_ret <- bb_ret ||| (bb_total_gold &&& ABB_Piece_Attacks[color, int(Piece.Gold), sq])
    let bb_hdk = bt.BB_Piece[color ^^^ 1, int(Piece.Horse)] ||| bt.BB_Piece[color ^^^ 1, int(Piece.Dragon)] ||| bt.BB_Piece[color ^^^ 1, int(Piece.King)]
    bb_ret <- bb_ret ||| (bb_hdk &&& ABB_Piece_Attacks[color, int(Piece.King), sq])
    let bb_bh = bt.BB_Piece[color ^^^ 1, int(Piece.Bishop)] ||| bt.BB_Piece[color ^^^ 1, int(Piece.Horse)]
    bb_ret <- bb_ret ||| (bb_bh &&& ABB_Diagonal_Attacks[sq][ABB_Diagonal_Mask_Ex[sq] &&& bb_occupied])
    let bb_rd = bt.BB_Piece[color ^^^ 1, int(Piece.Rook)] ||| bt.BB_Piece[color ^^^ 1, int(Piece.Dragon)]
    bb_ret <- bb_ret ||| (bb_rd &&& ABB_Cross_Attacks[sq][ABB_Cross_Mask_Ex[sq] &&& bb_occupied])
    let bb_lance_attacks = ABB_Lance_Attacks[color, sq][ABB_Lance_Mask_Ex[color, sq] &&& bb_occupied]
    bb_ret <- bb_ret ||| (bt.BB_Piece[color ^^^ 1, int(Piece.Lance)] &&& bb_lance_attacks)
    bb_ret

let IsAttackedByLongPieces(bt:BoardTree, sq:int, color:int):System.UInt128 = 
    let mutable bb_ret = System.UInt128()
    let bb_occupied = bt.BB_Occupied[0] ||| bt.BB_Occupied[1]
    let bb_bh = bt.BB_Piece[color ^^^ 1, int(Piece.Bishop)] ||| bt.BB_Piece[color ^^^ 1, int(Piece.Horse)]
    bb_ret <- bb_ret ||| bb_bh &&& ABB_Diagonal_Attacks[sq][ABB_Diagonal_Mask_Ex[sq] &&& bb_occupied]
    let bb_rd = bt.BB_Piece[color ^^^ 1, int(Piece.Rook)] ||| bt.BB_Piece[color ^^^ 1, int(Piece.Dragon)]
    bb_ret <- bb_ret ||| bb_rd &&& ABB_Cross_Attacks[sq][ABB_Cross_Mask_Ex[sq] &&& bb_occupied]
    let bb_lance_attacks = ABB_Lance_Attacks[color, sq][ABB_Lance_Mask_Ex[color, sq] &&& bb_occupied]
    bb_ret <- bt.BB_Piece[color ^^^ 1, int(Piece.Lance)] &&& bb_lance_attacks;
    bb_ret

let IsMatePawnDrop(bt: BoardTree, sq_drop:int, color:int):bool = 
    let mutable bret:bool = false
    if color = int(Common.Color.White) then
        if (sq_drop - 9) >= 1 && bt.Board.[sq_drop - 9] <> -int8(Piece.King) then
            bret <- false
    else
        if (sq_drop + 9) <= Square_NB && bt.Board.[sq_drop - 9] <> -int8(Piece.King) then
            bret <-false
    let mutable bb_sum:System.UInt128 = bt.BB_Piece[color, int(Piece.Knight)] &&& ABB_Piece_Attacks[int(color ^^^ 1), int(Piece.Knight), sq_drop]
    bb_sum <- bb_sum ||| (bt.BB_Piece[color, int(Piece.Silver)] &&& ABB_Piece_Attacks[int(color ^^^ 1), int(Piece.Silver), sq_drop])
    let bb_total_gold = bt.BB_Piece[color, int(Piece.Gold)] ||| bt.BB_Piece[color, int(Piece.Pro_Pawn)] ||| bt.BB_Piece[color, int(Piece.Pro_Lance)] ||| bt.BB_Piece[color, int(Piece.Pro_Knight)] ||| bt.BB_Piece[color, int(Piece.Pro_Silver)]
    bb_sum <- bb_sum ||| (bb_total_gold &&& ABB_Piece_Attacks[color ^^^ 1, (int)Piece.Gold, sq_drop])
    let bb_occupied = bt.BB_Occupied[0] ||| bt.BB_Occupied[1]
    let bb_bh = bt.BB_Piece[color, int(Piece.Bishop)] ||| bt.BB_Piece[color, int(Piece.Horse)]
    bb_sum <- bb_sum ||| (bb_bh &&& ABB_Diagonal_Attacks[sq_drop][ABB_Diagonal_Mask_Ex[sq_drop] &&& bb_occupied])
    let bb_rd = bt.BB_Piece[color, int(Piece.Rook)] ||| bt.BB_Piece[color, int(Piece.Dragon)]
    bb_sum <- bb_sum ||| (bb_rd &&& ABB_Cross_Attacks[sq_drop][ABB_Cross_Mask_Ex[sq_drop] &&& bb_occupied])
    let bb_hd = bt.BB_Piece[color, (int)Piece.Horse] ||| bt.BB_Piece[color, (int)Piece.Dragon]
    bb_sum <- bb_sum ||| (bb_hd &&& ABB_Piece_Attacks[color, (int)Piece.King, sq_drop])
    while bb_sum <> System.UInt128(uint64(0), uint64(0)) do
        let ifrom = Square(bb_sum)
        bb_sum <- bb_sum ^^^ ABB_Mask[ifrom]
        if IsDiscoverKing(bt, ifrom, sq_drop, color) then
            bret <- true
            bb_sum <-System.UInt128()
    if bret = false then
        let iking = int(bt.SQ_King[color])
        bret <- true
        bt.BB_Occupied[color ^^^ 1] <- bt.BB_Occupied[color ^^^ 1] ^^^ ABB_Mask[sq_drop]
        let mutable bb_move = ABB_Piece_Attacks[color, int(Piece.King), iking] ^^^ (bt.BB_Occupied[color] &&& BB_Full)
        while bb_move <> System.UInt128() do
            let ito = Square(bb_move)
            bb_move <- bb_move ^^^ ABB_Mask[ito]
            if IsAttacked(bt, ito, color) = System.UInt128() then
                bret <- false
                bb_move <- System.UInt128()
            //bb_move <- bb_move ^^^ ABB_Mask[ito]
        bt.BB_Occupied[color ^^^ 1] <- bt.BB_Occupied[color ^^^ 1] ^^^ ABB_Mask[sq_drop]
    bret

