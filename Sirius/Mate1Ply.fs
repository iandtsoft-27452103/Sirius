[<AutoOpen>]
module Mate1Ply

// メモ, 関数型言語での詰み判定は猛烈に面倒なので、使わない方が良いか？

let IsCanEscape(bt: BoardTree, color:int, sq_checker:int, pc_checker:int, sq_opponent_king:int, sq_object:int, is_promo:bool):bool = 
    let mutable bb_occupied = (bt.BB_Occupied[0] ||| bt.BB_Occupied[1]) ^^^ (ABB_Mask[sq_opponent_king] ||| ABB_Mask[sq_object])
    let mutable bb_attacks = System.UInt128()
    if is_promo = true then
        if pc_checker = int(Piece.Rook) then
            bb_attacks <- ABB_Cross_Attacks[sq_checker][ABB_Cross_Mask_Ex[sq_checker] &&& bb_occupied] ||| ABB_Piece_Attacks[color, int(Piece.King), sq_checker]
        elif pc_checker = int(Piece.Bishop) then
            bb_attacks <- ABB_Diagonal_Attacks[sq_checker][ABB_Diagonal_Mask_Ex[sq_checker] &&& bb_occupied] ||| ABB_Piece_Attacks[color, int(Piece.King), sq_checker]
        else
            bb_attacks <- ABB_Piece_Attacks[color, int(Piece.Gold), sq_checker]
    else
        if pc_checker = int(Piece.Rook) then
            bb_attacks <- ABB_Cross_Attacks[sq_checker][ABB_Cross_Mask_Ex[sq_checker] &&& bb_occupied]
        elif pc_checker = int(Piece.Dragon) then
            bb_attacks <- ABB_Cross_Attacks[sq_checker][ABB_Cross_Mask_Ex[sq_checker] &&& bb_occupied] ||| ABB_Piece_Attacks[color, int(Piece.King), sq_checker]
        elif pc_checker = int(Piece.Bishop) then
            bb_attacks <- ABB_Diagonal_Attacks[sq_checker][ABB_Diagonal_Mask_Ex[sq_checker] &&& bb_occupied]
        elif pc_checker = int(Piece.Horse) then
            bb_attacks <- ABB_Diagonal_Attacks[sq_checker][ABB_Diagonal_Mask_Ex[sq_checker] &&& bb_occupied] ||| ABB_Piece_Attacks[color, int(Piece.King), sq_checker]
        elif pc_checker = int(Piece.Lance) then
            bb_attacks <- ABB_Lance_Attacks[color, sq_checker][ABB_Lance_Mask_Ex[color, sq_checker] &&& bb_occupied]
        else
            bb_attacks <- ABB_Piece_Attacks[color, pc_checker, sq_checker]
    bb_attacks <- bb_attacks &&& ABB_Mask[sq_object]
    if bb_attacks > System.UInt128.Zero then
        false
    else
        true

let IsCanCapture(bt:BoardTree ref, color:int, opponent_color:int, sq_object:int, is_drop:bool, ifrom:int, ipiece:int):bool = 
    let pt = [|int(Piece.Pawn); int(Piece.Lance); int(Piece.Rook); int(Piece.Dragon)|]
    let bb_myside_attacks = AttacksToPiece(bt.contents, sq_object, color)
    let myside_attacks_count = int(System.UInt128.PopCount(bb_myside_attacks))
    let bb_opp_attacks = AttacksToPiece(bt.contents, sq_object, opponent_color)
    let opp_attacks_count = int(System.UInt128.PopCount(bb_opp_attacks))
    let mutable bret = false
    if opp_attacks_count > 1 then
        bret <- true
    elif opp_attacks_count = 1 && myside_attacks_count = 0 then
        bret <- true
    else
        if opp_attacks_count >= myside_attacks_count then
            if opp_attacks_count = myside_attacks_count && is_drop = false then
                bret <- true
            else
                if is_drop then
                    bret <- false
            if is_drop = false then
                bt.contents.BB_Occupied[color] <- bt.contents.BB_Occupied[color] ^^^ ABB_Mask[ifrom]
                bt.contents.BB_Piece[color, ipiece] <- bt.contents.BB_Piece[color, ipiece] ^^^ ABB_Mask[ifrom]
                let bb = IsAttacked(bt.contents, int(bt.contents.SQ_King[opponent_color]), color)
                let bb2 = IsAttacked(bt.contents, int(bt.contents.SQ_King[color]), color ^^^ 1)
                let mutable bb3 = System.UInt128()
                if Array.contains ipiece pt then
                    let idirec = Adirec[ifrom, sq_object]
                    if int(abs(idirec)) = int(Direction.Direc_File_U2d) then
                        bb3 <- IsAttacked(bt.contents, sq_object, color)
                bt.contents.BB_Occupied[color] <- bt.contents.BB_Occupied[color] ^^^ ABB_Mask[ifrom]
                bt.contents.BB_Piece[color, ipiece] <- bt.contents.BB_Piece[color, ipiece] ^^^ ABB_Mask[ifrom]
                if bb > System.UInt128() then
                    bret <- false
                if bb2 > System.UInt128() || bb3 > System.UInt128() then
                    bret <- false
        else
            bret <-false
    bret

let IsMateIn1Ply(bt: BoardTree, color:int):uint32 =
    let mutable mate_move = Move.Init()
    let null_move = Move.Init()
    let mutable sq_can_check_by_drop:int[] = [| 0; 0; 0; 0; 0; 0; 0; 0 |]
    let mutable sq_can_check_by_move:int[] = [| 0; 0; 0; 0; 0; 0; 0; 0 |]
    let mutable pos_array:int[] = [| 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 |]
    let mutable pc_array:int[] = [| 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 |]
    let mutable sq_can_escape:int[] = [| 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 |]
    let mutable cnt_d = 0
    let mutable cnt_m = 0
    let mutable cnt_e = 0
    let opponent_color = color ^^^ 1
    let sq_opponent_king = bt.SQ_King.[opponent_color]
    let bb_occupied = bt.BB_Occupied.[0] ||| bt.BB_Occupied.[1]
    let bb_empty = bb_occupied ^^^ System.UInt128.MaxValue &&& BB_Full
    let bb_can_escape = (bt.BB_Occupied[color] ||| bb_empty) &&& BB_Full
    let hand = bt.Hand.[color]
    let mutable bb_opp_king_attacks = ABB_Piece_Attacks[opponent_color, int(Piece.King), int(sq_opponent_king)]
    let mutable flag = false
    let mutable is_mate = false
    while bb_opp_king_attacks > System.UInt128() do
        let sq = Square(bb_opp_king_attacks)
        bb_opp_king_attacks <- bb_opp_king_attacks ^^^ ABB_Mask.[sq]
        let bb_myside_attacks = AttacksToPiece(bt, sq, opponent_color)
        let myside_attacks_count = int(System.UInt128.PopCount(bb_myside_attacks))
        flag <- false
        if myside_attacks_count >= 2 && bt.Board.[sq] = int8(Piece.Empty) then
            flag <- true
        if bb_can_escape &&& ABB_Mask[sq] > System.UInt128() then
            if IsAttacked(bt, sq, opponent_color) = System.UInt128() then
                sq_can_escape[cnt_e] <- sq
                cnt_e <- cnt_e + 1
        if bt.Board.[sq] = int8(Piece.Empty) && flag = false then
            sq_can_check_by_drop[cnt_d] <- sq
            cnt_d <- cnt_d + 1
        let bb_enemy_attacks = IsAttacked(bt, sq, color ^^^ 1)
        if bt.Board.[sq] <> int8(Piece.Empty) && (bt.BB_Occupied[opponent_color] &&& ABB_Mask.[sq]) > System.UInt128() && bb_enemy_attacks > System.UInt128() then
            sq_can_check_by_move[cnt_m] <- sq
            cnt_m <- cnt_m + 1
        if myside_attacks_count < 2 && bt.Board.[sq] = int8(Piece.Empty) && bb_enemy_attacks > System.UInt128() then
            sq_can_check_by_move[cnt_m] <- sq
            cnt_m <- cnt_m + 1
    let mutable i = 0
    //for i in 0.. cnt_d - 1 do
    if hand > 0 then
        while i < cnt_d do
            let sq = sq_can_check_by_drop[i]
            let idirec = Adirec[sq, int(sq_opponent_king)]
            let pt = Piece_Table[opponent_color]
            let mutable bb = AttacksToPiece(bt, sq, opponent_color)
            let mutable cnt_pos = 0
            let mutable cnt_pc = 0
            while bb > System.UInt128() do
                let pos = Square(bb)
                bb <- bb ^^^ ABB_Mask.[pos]
                pos_array[cnt_pos] <- pos
                cnt_pos <- cnt_pos + 1
                pc_array[cnt_pc] <- int(bt.Board[pos])
                cnt_pc <- cnt_pc + 1
            let pcs = pt[idirec]
            let max_pcs = List.ofSeq pcs |> List.max
            if max_pcs > int(Piece.Rook)then
                let mutable j = 0
                while j < pt.Count && j < pcs.Count do
                    let pc = pcs[j]
                    if pc <> int(Piece.Pawn) && pc < 8 && (hand &&& Hand_Mask[pc]) > 0 then
                        if cnt_e = 0 then
                            is_mate <- true
                            mate_move <- Move.Pack(uint32(Square_NB + pc - 1), uint32(sq), uint32(pc), 0u, 0u)
                        else
                            let mutable counter = 0
                            let mutable mate_flag = false
                            //let mutable k = 0
                            //for k in 0.. cnt_e - 1 do
                            while counter < cnt_e do
                                let sq_object = sq_can_escape[counter]
                                if sq = sq_object then
                                    counter <- counter + 1
                                if IsCanEscape(bt, color, sq, pc, int(sq_opponent_king), sq_object, false) = false && IsCanCapture(ref bt, color, opponent_color, sq, true, int(Square_NB + pc - 1), pc) = false then
                                    if counter = cnt_e - 1 then
                                        mate_flag <- true
                                        counter <- cnt_e
                                    else
                                        counter <- counter + 1
                                else
                                    mate_flag <- false
                                    //counter <- counter + 1
                                    counter <- cnt_e
                            if counter = cnt_e && mate_flag = true then
                                is_mate <- true
                                mate_move <- Move.Pack(uint32(Square_NB + pc - 1), uint32(sq), uint32(pc), 0u, 0u)
                    j <- j + 1
                    if is_mate = true then
                        j <- pt.Count
                        i <- cnt_d
                i <- i + 1
    else
        let mutable ii = 0
        while ii < cnt_m do
            let sq = sq_can_check_by_move[ii]
            let mutable idirec = Adirec[sq, int(sq_opponent_king)]
            let pt = Piece_Table[opponent_color]
            let mutable bb = AttacksToPiece(bt, sq, color)
            let attacks_count = System.UInt128.PopCount(bb)
            if int(attacks_count) < 2 && bb > System.UInt128() then
                let pos = Square(bb);
                let mutable bb2 = AttacksToLongPiece(bt, pos, color)
                while bb2 > System.UInt128() do
                    let sq2 = Square(bb2);
                    bb2 <- bb2 ^^^  ABB_Mask.[sq2]
                    let idirec2 = Adirec[sq2, int(sq_opponent_king)]
                    if idirec = idirec2 then
                        if cnt_e = 0 then
                            if IsCanCapture(ref bt, color, opponent_color, sq, false, pos, abs(int(bt.Board[pos]))) = false  && IsPinnedOnKing(bt, pos, Adirec[int(bt.SQ_King[color]), pos], color) = System.UInt128()then
                                is_mate <- true
                                mate_move <- Move.Pack(uint32(pos), uint32(sq), uint32(abs(bt.Board[pos])), uint32(abs(bt.Board[sq])), 0u)
                        elif cnt_e = 1 then
                            let sq3 = sq_can_escape[0]
                            let idirec3 = Adirec[sq3, int(sq_opponent_king)]
                            if IsCanCapture(ref bt, color, opponent_color, sq, false, pos, abs(int(bt.Board[pos]))) = false  && IsPinnedOnKing(bt, pos, Adirec[int(bt.SQ_King[color]), pos], color) = System.UInt128()then
                                if abs(idirec) = abs(idirec3) then
                                    if idirec = int(Direction.Direc_File_U2d) || idirec = int(Direction.Direc_File_D2u)then
                                        if abs(bt.Board[pos]) = int8(Piece.Lance) || abs(bt.Board[pos]) = int8(Piece.Rook) || abs(bt.Board[pos]) = int8(Piece.Dragon) then
                                             is_mate <- true
                                             mate_move <- Move.Pack(uint32(pos), uint32(sq), uint32(abs(bt.Board[pos])), uint32(abs(bt.Board[sq])), 0u)
                                    elif idirec = int(Direction.Direc_Rank_L2r) then
                                        if abs(bt.Board[pos]) = int8(Piece.Rook) || abs(bt.Board[pos]) = int8(Piece.Dragon) then
                                             is_mate <- true
                                             mate_move <- Move.Pack(uint32(pos), uint32(sq), uint32(abs(bt.Board[pos])), uint32(abs(bt.Board[sq])), 0u)
                                    elif idirec = int(Direction.Direc_Diag1_U2d) || idirec = int(Direction.Direc_Diag2_U2d) then
                                        if abs(bt.Board[pos]) = int8(Piece.Bishop) || abs(bt.Board[pos]) = int8(Piece.Horse) then
                                             is_mate <- true
                                             mate_move <- Move.Pack(uint32(pos), uint32(sq), uint32(abs(bt.Board[pos])), uint32(abs(bt.Board[sq])), 0u)
                    else
                        if cnt_e = 0 && (ABB_Piece_Attacks[color, int(Piece.Gold), sq] &&& ABB_Piece_Attacks[opponent_color, int(Piece.King), int(sq_opponent_king)]) > System.UInt128() && (ABB_Mask.[sq] &&& BB_Color_Position[int(opponent_color)]) > System.UInt128() then
                            let bb_myside_attacks = AttacksToPiece(bt, sq, opponent_color)
                            let myside_attacks_count = System.UInt128.PopCount(bb_myside_attacks)
                            let idirec3 = Adirec[pos, int(bt.SQ_King[color])]
                            bt.BB_Occupied[color] <- bt.BB_Occupied[color] ^^^ ABB_Mask.[pos]
                            let bb = IsPinnedOnKing(bt, pos, idirec3, color)
                            bt.BB_Occupied[color] <- bt.BB_Occupied[color] ^^^ ABB_Mask.[pos]
                            if int(myside_attacks_count) < 2 && bb = System.UInt128() then
                                is_mate <- true
                                mate_move <- Move.Pack(uint32(pos), uint32(sq), uint32(abs(bt.Board.[pos])), uint32(abs(bt.Board[sq])), 1u)                                    
            else
                let mutable cnt_pos = 0
                let mutable cnt_pc = 0
                while bb > System.UInt128() do
                    let pos = Square(bb)
                    bb <- bb ^^^ ABB_Mask.[pos]
                    pos_array[cnt_pos] <- pos
                    cnt_pos <- cnt_pos + 1
                    pc_array[cnt_pc] <- int(bt.Board.[pos])
                    cnt_pc <- cnt_pc + 1
                let pcs = pt[idirec]
                if cnt_pos <> 0 then
                    let mutable index = 0
                    let mutable temp_index = 0
                    while index < cnt_pos do
                        let pos = pos_array[index]
                        let pc = abs(pc_array[index])
                        temp_index <- index
                        if pc <> int(Piece.King) then
                            idirec <- Adirec[pos, int(sq_opponent_king)]
                            let is_contain =  pcs.Contains(pc)
                            let is_rev = BB_Rev_Color_Position[color] &&& ABB_Mask[pos] > System.UInt128()
                            if IsDiscoverKing2(bt, pos, sq, color, pc) = false || is_rev then
                                if is_contain = true || is_rev = true then
                                    if LongPieces2.Contains(pc) then
                                        if cnt_e = 0 then
                                            if LongPieces.Contains(pc) && IsCanCapture(ref bt, color, opponent_color, sq, false, pos, pc) = false && IsPinnedOnKing(bt, pos, Adirec[int(bt.SQ_King[color]), pos], color) = System.UInt128() then
                                                let mutable flag_promo = 0
                                                if ABB_Mask.[sq] &&& BB_Color_Position[opponent_color] > System.UInt128() then
                                                    flag_promo <- 1
                                                mate_move <- Move.Pack(uint32(pos), uint32(sq), uint32(pc), uint32(abs(bt.Board.[sq])), uint32(flag_promo))
                                                index <- cnt_pos
                                            else
                                                index <- index + 1
                                        else
                                            flag <- false
                                            let mutable j = 0
                                            while j < cnt_e do
                                                let sq_object = sq_can_escape.[j]
                                                j <- j + 1
                                                if sq <> sq_object then
                                                    if IsCanEscape(bt, color, sq, pc, int(sq_opponent_king), sq_object, false) = false && IsCanCapture(ref bt, color, opponent_color, sq, false, pos, pc) = false && IsPinnedOnKing(bt, pos, Adirec[int(bt.SQ_King[color]), pos], color) = System.UInt128() then
                                                        let mutable flag_promo = 0
                                                        if (ABB_Mask.[pos] &&& BB_Color_Position[opponent_color]) > System.UInt128() || (ABB_Mask[sq] &&& BB_Color_Position[opponent_color]) > System.UInt128() then
                                                            flag_promo <- 1
                                                        mate_move <- Move.Pack(uint32(pos), uint32(sq), uint32(pc), uint32(abs(bt.Board.[sq])), uint32(flag_promo))
                                                        if j = cnt_e then
                                                            index <- cnt_pos
                                                            j <- cnt_e
                                                    else
                                                        mate_move <- Move.Init()
                                                        j <- cnt_e
                                                else
                                                    flag <- true
                                                    mate_move <- Move.Init()
                                                    j <- cnt_e
                                            if (flag = true && mate_move <> 0u) then
                                                index <- cnt_pos
                                            else
                                                index <- index + 1
                                    elif pc = int(Piece.Dragon) || pc = int(Piece.Horse) then
                                        if cnt_e = 0 then
                                            if IsCanCapture(ref bt, color, opponent_color, sq, false, pos, pc) = false  && IsPinnedOnKing(bt, pos, Adirec[int(bt.SQ_King[color]), pos], color) = System.UInt128() then
                                                mate_move <- Move.Pack(uint32(pos), uint32(sq), uint32(pc), uint32(abs(bt.Board.[sq])), 0u)
                                                index <- cnt_pos
                                            else
                                                index <- index + 1
                                        else
                                            flag <- false
                                            let mutable j = 0
                                            while j < cnt_e do
                                                let sq_object = sq_can_escape.[j]
                                                j <- j + 1
                                                if sq <> sq_object then
                                                    if IsCanEscape(bt, color, sq, pc, int(sq_opponent_king), sq_object, false) = false && IsCanCapture(ref bt, color, opponent_color, sq, false, pos, pc) = false && IsPinnedOnKing(bt, pos, Adirec[int(bt.SQ_King[color]), pos], color) = System.UInt128() then
                                                        mate_move <- Move.Pack(uint32(pos), uint32(sq), uint32(pc), uint32(abs(bt.Board.[sq])), 0u)
                                                        flag <- true
                                                        //index <- cnt_pos
                                                        //index <- index + 1
                                                        //j <- cnt_e
                                                        //j <- j + 1
                                                    else
                                                        flag <- false
                                                        mate_move <- Move.Init()
                                                        j <- cnt_e
                                            if (flag = true && mate_move <> 0u) then
                                                index <- cnt_pos
                                            else
                                                index <- index + 1
                                    elif is_contain = true && GoldSilver.Contains(pc) then
                                        if cnt_e = 0 then
                                            if IsCanCapture(ref bt, color, opponent_color, sq, false, pos, pc) = false  && IsPinnedOnKing(bt, pos, Adirec[int(bt.SQ_King[color]), pos], color) = System.UInt128() then
                                                mate_move <- Move.Pack(uint32(pos), uint32(sq), uint32(pc), uint32(abs(bt.Board.[sq])), 0u)
                                                index <- cnt_pos
                                            else
                                                index <- index + 1
                                        else
                                            flag <- false
                                            let mutable j = 0
                                            while j < cnt_e do
                                                let sq_object = sq_can_escape.[j]
                                                j <- j + 1
                                                if sq <> sq_object then
                                                    if IsCanEscape(bt, color, sq, pc, int(sq_opponent_king), sq_object, true) = false && IsCanCapture(ref bt, color, opponent_color, sq, false, pos, pc) = false  && IsPinnedOnKing(bt, pos, Adirec[int(bt.SQ_King[color]), pos], color) = System.UInt128() then
                                                        mate_move <- Move.Pack(uint32(pos), uint32(sq), uint32(pc), uint32(abs(bt.Board.[sq])), 1u)
                                                        flag <- true
                                                        //index <- cnt_pos あああ
                                                        //j <- cnt_e
                                                    else
                                                        flag <- false
                                                        mate_move <- Move.Init()
                                                        j <- cnt_e
                                            if (flag = true && mate_move <> 0u) then
                                                index <- cnt_pos
                                            else
                                                index <- index + 1
                                    elif is_contain = false && pc = int(Piece.Silver) then
                                        if cnt_e = 0 then
                                            if IsCanCapture(ref bt, color, opponent_color, sq, false, pos, pc) = false  && IsPinnedOnKing(bt, pos, Adirec[int(bt.SQ_King[color]), pos], color) = System.UInt128() then
                                                mate_move <- Move.Pack(uint32(pos), uint32(sq), uint32(pc), uint32(abs(bt.Board.[sq])), 1u)
                                                index <- cnt_pos
                                            else
                                                index <- index + 1
                                        else
                                            flag <- false
                                            let mutable j = 0
                                            while j < cnt_e do
                                                let sq_object = sq_can_escape.[j]
                                                j <- j + 1
                                                if sq <> sq_object then
                                                    if IsCanEscape(bt, color, sq, pc, int(sq_opponent_king), sq_object, true) = false && IsCanCapture(ref bt, color, opponent_color, sq, false, pos, pc) = false  && IsPinnedOnKing(bt, pos, Adirec[int(bt.SQ_King[color]), pos], color) = System.UInt128() then
                                                        mate_move <- Move.Pack(uint32(pos), uint32(sq), uint32(pc), uint32(abs(bt.Board.[sq])), 1u)
                                                        flag <- true
                                                        //index <- cnt_pos あああ
                                                        //j <- cnt_e
                                                    else
                                                        flag <- false
                                                        mate_move <- Move.Init()
                                                        j <- cnt_e
                                            if (flag = true && mate_move <> 0u) then
                                                index <- cnt_pos
                                            else
                                                index <- index + 1
                                    elif pc = int(Piece.Knight) then
                                        //桂馬が成る手
                                        if cnt_e = 0 then
                                            if IsCanCapture(ref bt, color, opponent_color, sq, false, pos, pc) = false && IsPinnedOnKing(bt, pos, Adirec[int(bt.SQ_King[color]), pos], color) = System.UInt128() && (ABB_Piece_Attacks.[color, int(Piece.Gold), sq] &&& ABB_Mask[int(sq_opponent_king)]) > System.UInt128() then
                                                mate_move <- Move.Pack(uint32(pos), uint32(sq), uint32(pc), uint32(abs(bt.Board.[sq])), 1u)
                                                index <- cnt_pos
                                            else
                                                index <- index + 1
                                        else
                                            flag <- false
                                            let mutable j = 0
                                            while j < cnt_e do
                                                let sq_object = sq_can_escape.[j]
                                                j <- j + 1
                                                if sq <> sq_object then
                                                    if IsCanEscape(bt, color, sq, pc, int(sq_opponent_king), sq_object, true) = false && IsCanCapture(ref bt, color, opponent_color, sq, false, pos, pc) = false && IsPinnedOnKing(bt, pos, Adirec[int(bt.SQ_King[color]), pos], color) = System.UInt128() then
                                                        mate_move <- Move.Pack(uint32(pos), uint32(sq), uint32(pc), uint32(abs(bt.Board.[sq])), 1u)
                                                        flag <- true
                                                        //index <- cnt_pos あああ
                                                        //j <- cnt_e
                                                    else
                                                        flag <- false
                                                        mate_move <- Move.Init()
                                                        j <- cnt_e
                                            if (flag = true && mate_move <> 0u) then
                                                index <- cnt_pos
                                            else
                                                index <- index + 1


                                    // 香車または歩が成る手
                                    elif ShortPieces.Contains(pc) && (BB_Rev_Color_Position[color] &&& ABB_Mask.[sq] > System.UInt128()) then
                                        if cnt_e = 0 then
                                            if IsCanCapture(ref bt, color, opponent_color, sq, false, pos, pc) = false && IsPinnedOnKing(bt, pos, Adirec[int(bt.SQ_King[color]), pos], color) = System.UInt128() && (ABB_Piece_Attacks.[color, int(Piece.Gold), sq] &&& ABB_Mask.[int(sq_opponent_king)]) > System.UInt128() then
                                                mate_move <- Move.Pack(uint32(pos), uint32(sq), uint32(pc), uint32(abs(bt.Board.[sq])), 1u)
                                                index <- cnt_pos
                                            else
                                                index <- index + 1
                                        else
                                            flag <- false
                                            let mutable j = 0
                                            while j < cnt_e do
                                                let sq_object = sq_can_escape.[j]
                                                j <- j + 1
                                                if sq <> sq_object then
                                                    if IsCanEscape(bt, color, sq, pc, int(sq_opponent_king), sq_object, true) = false && IsCanCapture(ref bt, color, opponent_color, sq, false, pos, pc) = false && IsPinnedOnKing(bt, pos, Adirec[int(bt.SQ_King[color]), pos], color) = System.UInt128() then
                                                        mate_move <- Move.Pack(uint32(pos), uint32(sq), uint32(pc), uint32(abs(bt.Board.[sq])), 1u)
                                                        flag <- true
                                                        //index <- cnt_pos あああ
                                                        //j <- cnt_e
                                                    else
                                                        flag <- false
                                                        mate_move <- Move.Init()
                                                        j <- cnt_e
                                            if (flag = true && mate_move <> 0u) then
                                                index <- cnt_pos
                                            else
                                                index <- index + 1
                                    else
                                        index <- index + 1
                                else
                                    let pc_promote = pc + Promote
                                    if pcs.Contains(pc) && pc = int(Piece.Knight) && (BB_Rev_Color_Position[color] &&& ABB_Mask.[sq] > System.UInt128()) then
                                        // 桂馬が成る手
                                        if cnt_e = 0 then
                                            if IsCanCapture(ref bt, color, opponent_color, sq, false, pos, pc) = false && IsPinnedOnKing(bt, pos, Adirec[int(bt.SQ_King[color]), pos], color) = System.UInt128() && (ABB_Piece_Attacks.[color, int(Piece.Gold), sq] &&& ABB_Mask[int(sq_opponent_king)]) > System.UInt128() then
                                                mate_move <- Move.Pack(uint32(pos), uint32(sq), uint32(pc), uint32(abs(bt.Board.[sq])), 1u)
                                                index <- cnt_pos
                                            else
                                                index <- index + 1
                                        else
                                            flag <- false
                                            let mutable j = 0
                                            while j < cnt_e do
                                                let sq_object = sq_can_escape.[j]
                                                j <- j + 1
                                                if sq <> sq_object then
                                                    if IsCanEscape(bt, color, sq, pc, int(sq_opponent_king), sq_object, true) = false && IsCanCapture(ref bt, color, opponent_color, sq, false, pos, pc) = false && IsPinnedOnKing(bt, pos, Adirec[int(bt.SQ_King[color]), pos], color) = System.UInt128() then
                                                        mate_move <- Move.Pack(uint32(pos), uint32(sq), uint32(pc), uint32(abs(bt.Board.[sq])), 1u)
                                                        flag <- true
                                                        //index <- cnt_pos あああ
                                                        //j <- cnt_e
                                                    else
                                                        flag <- false
                                                        mate_move <- Move.Init()
                                                        j <- cnt_e
                                            if (flag = true && mate_move <> 0u) then
                                                index <- cnt_pos
                                            else
                                                index <- index + 1
                                    elif  pcs.Contains(pc_promote) && pc = int(Piece.Knight) && (BB_Rev_Color_Position[color] &&& ABB_Mask.[sq] > System.UInt128()) then
                                        // 桂馬が成る手
                                        if cnt_e = 0 then
                                            if IsCanCapture(ref bt, color, opponent_color, sq, false, pos, pc) = false && IsPinnedOnKing(bt, pos, Adirec[int(bt.SQ_King[color]), pos], color) = System.UInt128() && (ABB_Piece_Attacks.[color, int(Piece.Gold), sq] &&& ABB_Mask[int(sq_opponent_king)]) > System.UInt128() then
                                                mate_move <- Move.Pack(uint32(pos), uint32(sq), uint32(pc), uint32(abs(bt.Board.[sq])), 1u)
                                                index <- cnt_pos
                                            else
                                                index <- index + 1
                                        else
                                            flag <- false
                                            let mutable j = 0
                                            while j < cnt_e do
                                                let sq_object = sq_can_escape.[j]
                                                j <- j + 1
                                                if sq <> sq_object then
                                                    if IsCanEscape(bt, color, sq, pc, int(sq_opponent_king), sq_object, true) = false && IsCanCapture(ref bt, color, opponent_color, sq, false, pos, pc) = false && IsPinnedOnKing(bt, pos, Adirec[int(bt.SQ_King[color]), pos], color) = System.UInt128() then
                                                        mate_move <- Move.Pack(uint32(pos), uint32(sq), uint32(pc), uint32(abs(bt.Board.[sq])), 1u)
                                                        flag <- true
                                                        //index <- cnt_pos
                                                        //j <- cnt_e
                                                    else
                                                        flag <- false
                                                        mate_move <- Move.Init()
                                                        j <- cnt_e
                                            if (flag = true && mate_move <> 0u) then
                                                index <- cnt_pos
                                            else
                                                index <- index + 1
                                    else
                                        // 香車または歩が成る手
                                        if pcs.Contains(pc) && ShortPieces.Contains(pc) && (BB_Rev_Color_Position[color] &&& ABB_Mask.[sq] > System.UInt128()) then
                                            if cnt_e = 0 then
                                                if IsCanCapture(ref bt, color, opponent_color, sq, false, pos, pc) = false && IsPinnedOnKing(bt, pos, Adirec[int(bt.SQ_King[color]), pos], color) = System.UInt128() && (ABB_Piece_Attacks.[color, int(Piece.Gold), sq] &&& ABB_Mask.[int(sq_opponent_king)]) > System.UInt128() then
                                                    mate_move <- Move.Pack(uint32(pos), uint32(sq), uint32(pc), uint32(abs(bt.Board.[sq])), 1u)
                                                    index <- cnt_pos
                                                else
                                                    index <- index + 1
                                            else
                                                flag <- false
                                                let mutable j = 0
                                                while j < cnt_e do
                                                    let sq_object = sq_can_escape.[j]
                                                    j <- j + 1
                                                    if sq <> sq_object then
                                                        if IsCanEscape(bt, color, sq, pc, int(sq_opponent_king), sq_object, true) = false && IsCanCapture(ref bt, color, opponent_color, sq, false, pos, pc) = false && IsPinnedOnKing(bt, pos, Adirec[int(bt.SQ_King[color]), pos], color) = System.UInt128() then
                                                            mate_move <- Move.Pack(uint32(pos), uint32(sq), uint32(pc), uint32(abs(bt.Board.[sq])), 1u)
                                                            flag <- true
                                                            //index <- cnt_pos あああ
                                                            //j <- cnt_e
                                                        else
                                                            flag <- false
                                                            mate_move <- Move.Init()
                                                            j <- cnt_e
                                                if (flag = true && mate_move <> 0u) then
                                                    index <- cnt_pos
                                                else
                                                    index <- index + 1
                                        elif pc = int(Piece.Silver) then
                                            // 銀が成る手
                                            if cnt_e = 0 then
                                                if IsCanCapture(ref bt, color, opponent_color, sq, false, pos, pc) = false && IsPinnedOnKing(bt, pos, Adirec[int(bt.SQ_King[color]), pos], color) = System.UInt128() && (ABB_Piece_Attacks.[color, int(Piece.Gold), sq] &&& ABB_Mask.[int(sq_opponent_king)]) > System.UInt128() then
                                                    mate_move <- Move.Pack(uint32(pos), uint32(sq), uint32(pc), uint32(abs(bt.Board.[sq])), 1u)
                                                    index <- cnt_pos
                                                else
                                                    index <- index + 1
                                            else
                                                flag <- false
                                                let mutable j = 0
                                                while j < cnt_e do
                                                    let sq_object = sq_can_escape.[j]
                                                    j <- j + 1
                                                    if sq <> sq_object then
                                                        if IsCanEscape(bt, color, sq, pc, int(sq_opponent_king), sq_object, true) = false && IsCanCapture(ref bt, color, opponent_color, sq, false, pos, pc) = false && IsPinnedOnKing(bt, pos, Adirec[int(bt.SQ_King[color]), pos], color) = System.UInt128() then
                                                            mate_move <- Move.Pack(uint32(pos), uint32(sq), uint32(pc), uint32(abs(bt.Board.[sq])), 1u)
                                                            flag <- true
                                                            //index <- cnt_pos
                                                            //j <- cnt_e
                                                        else
                                                            flag <- false
                                                            mate_move <- Move.Init()
                                                            j <- cnt_e
                                                if (flag = true && mate_move <> 0u) then
                                                    index <- cnt_pos
                                                else
                                                    index <- index + 1
                                        else
                                            if pc < int(Piece.Bishop) then
                                                index <- index + 1
                                            else
                                                // 角行または飛車が成る手
                                                if pcs.Contains(pc_promote) && LongPieces.Contains(pc) && (BB_Rev_Color_Position.[color] &&& ABB_Mask.[sq]) > System.UInt128() || (BB_Rev_Color_Position.[color] &&& ABB_Mask.[pos]) > System.UInt128() then
                                                    if cnt_e = 0 then
                                                        if IsCanCapture(ref bt, color, opponent_color, sq, false, pos, pc) = false && IsPinnedOnKing(bt, pos, Adirec[int(bt.SQ_King[color]), pos], color) = System.UInt128() && (ABB_Piece_Attacks.[color, int(Piece.King), sq] &&& ABB_Mask.[int(sq_opponent_king)]) > System.UInt128() then
                                                            let mutable flag_promo = 0 // この場合は成らないか？
                                                            if (ABB_Mask.[pos] &&& BB_Color_Position[opponent_color]) > System.UInt128() || (ABB_Mask.[sq] &&& BB_Color_Position[opponent_color]) > System.UInt128() then
                                                                flag_promo <- 1
                                                            mate_move <- Move.Pack(uint32(pos), uint32(sq), uint32(pc_promote), uint32(abs(bt.Board.[sq])), uint32(flag_promo))
                                                            index <- cnt_pos
                                                        else
                                                            index <- index + 1
                                                    else
                                                        flag <- false
                                                        let mutable j = 0
                                                        while j < cnt_e do
                                                            let sq_object = sq_can_escape.[j]
                                                            j <- j + 1
                                                            if sq <> sq_object then
                                                                if IsCanEscape(bt, color, sq, pc_promote, int(sq_opponent_king), sq_object, true) = false && IsCanCapture(ref bt, color, opponent_color, sq, false, pos, pc) = false  && IsPinnedOnKing(bt, pos, Adirec[int(bt.SQ_King[color]), pos], color) = System.UInt128() then
                                                                    let mutable flag_promo = 0 // この場合は成らないか？
                                                                    if (ABB_Mask.[pos] &&& BB_Color_Position[opponent_color]) > System.UInt128() || (ABB_Mask.[sq] &&& BB_Color_Position[opponent_color]) > System.UInt128() then
                                                                        flag_promo <- 1
                                                                    mate_move <- Move.Pack(uint32(pos), uint32(sq), uint32(pc_promote), uint32(abs(bt.Board.[sq])), uint32(flag_promo))
                                                                    flag <- true
                                                                    //index <- cnt_pos
                                                                    //j <- cnt_e
                                                                else
                                                                    flag <- false
                                                                    mate_move <- Move.Init()
                                                                    j <- cnt_e
                                                        if (flag = true && mate_move <> 0u) then
                                                            index <- cnt_pos
                                                        else
                                                            index <- index + 1
                                                else
                                                    index <- index + 1


                                        //index <- index + 1
                                //else
                                //index <- index + 1
                            else
                                index <- index + 1
                        elif pc > int(Piece.Rook) then
                            index <- index + 1
                    if index = temp_index then
                        index <- index + 1
                if mate_move > 0u then
                    ii <- cnt_m
            ii <- ii + 1
        //i <- cnt_d


    // 桂馬を打って詰みになる場合と、動かして詰みになる場合。
    // 相手玉8近傍からの王手ではないので、玉で取ることはできない。
    if mate_move = 0u then
        let pc = int(Piece.Knight)
        let bb_occupied = bt.BB_Occupied.[0] ||| bt.BB_Occupied.[1]
        let mutable bb_empty = bb_occupied ^^^ System.UInt128.MaxValue &&& BB_Full
        let mutable bb = ABB_Piece_Attacks.[opponent_color, pc, int(sq_opponent_king)] &&& (bb_empty ||| bt.BB_Occupied.[opponent_color])
        while bb > System.UInt128() do
            let sq = Square(bb)
            bb <- bb ^^^ ABB_Mask.[sq]
            let bb_opponent_attacks_to_sq = AttacksToPiece(bt, sq, opponent_color)
            if ((hand &&& Hand_Mask.[pc]) > 0 && bt.Board.[sq] = int8(Piece.Empty) && cnt_e = 0 && bb_opponent_attacks_to_sq = System.UInt128()) then
                // drop knight
                mate_move <- Move.Pack(uint32(Square_NB + pc - 1), uint32(sq), uint32(pc), 0u, 0u)
                bb <- System.UInt128()
            else
                let mutable bb_my_knight_attacks = ABB_Piece_Attacks.[opponent_color, pc, sq] &&& bt.BB_Piece.[color, int(Piece.Knight)]
                if bb_my_knight_attacks > System.UInt128() && cnt_e = 0 && bb_opponent_attacks_to_sq = System.UInt128() then
                    // move knight
                    let pos = Square(bb_my_knight_attacks)
                    bb_my_knight_attacks <- bb_my_knight_attacks ^^^ ABB_Mask.[sq]
                    //if IsDiscoverKing2(bt, pos, sq, color, pc) = true then
                    if IsCanCapture(ref bt, color, opponent_color, sq, false, pos, pc) = false && IsPinnedOnKing(bt, pos, Adirec[int(bt.SQ_King[color]), pos], color) = System.UInt128() then
                        mate_move <- Move.Pack(uint32(pos), uint32(sq), uint32(pc), uint32(abs(bt.Board.[sq])), 0u)
    mate_move