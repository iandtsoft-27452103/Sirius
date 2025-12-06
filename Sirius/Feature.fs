[<AutoOpen>]
module Feature

let Rev_Sq = [| 80; 79; 78; 77; 76; 75; 74; 73; 72;
                71; 70; 69; 68; 67; 66; 65; 64; 63;
                62; 61; 60; 59; 58; 57; 56; 55; 54;
                53; 52; 51; 50; 49; 48; 47; 46; 45;
                44; 43; 42; 41; 40; 39; 38; 37; 36;
                35; 34; 33; 32; 31; 30; 29; 28; 27;
                26; 25; 24; 23; 22; 21; 20; 19; 18;
                17; 16; 15; 14; 13; 12; 11; 10; 9;
                8;  7;  6;  5;  4;  3;  2;  1; 0 |]

let kkp_hand_pawn = 0
let kkp_hand_lance = 18
let kkp_hand_knight = 22
let kkp_hand_silver = 26
let kkp_hand_gold = 30
let kkp_hand_bishop = 34
let kkp_hand_rook = 36
let kkp_pawn = 38
let kkp_lance = 119
let kkp_knight = 200
let kkp_silver = 281
let kkp_gold = 362
let kkp_bishop = 443
let kkp_rook = 524
let kkp_pro_pawn = 605
let kkp_pro_lance = 686
let kkp_pro_knight = 767
let kkp_pro_silver = 848
let kkp_horse = 929
let kkp_dragon = 1010
let kkp_end = 1091

let b_hand_pawn = 0
let b_hand_lance = 18
let b_hand_knight = 22
let b_hand_silver = 26
let b_hand_gold = 30
let b_hand_bishop = 34
let b_hand_rook = 36
let b_pawn = 38
let b_lance = 119
let b_knight = 200
let b_silver = 281
let b_gold = 362
let b_bishop = 443
let b_rook = 524
let b_pro_pawn = 605
let b_pro_lance = 686
let b_pro_knight = 767
let b_pro_silver = 848
let b_horse = 929
let b_dragon = 1010
let w_hand_pawn = 1091
let w_hand_lance = 1109
let w_hand_knight = 1113
let w_hand_silver = 1117
let w_hand_gold = 1121
let w_hand_bishop = 1125
let w_hand_rook = 1127
let w_pawn = 1129
let w_lance = 1210
let w_knight = 1291
let w_silver = 1372
let w_gold = 1453
let w_bishop = 1534
let w_rook = 1615
let w_pro_pawn = 1696
let w_pro_lance = 1777
let w_pro_knight = 1858
let w_pro_silver = 1939
let w_horse = 2020
let w_dragon = 2101
let pp_end = 2182

let kkp_start_index = [| -1; kkp_pawn; kkp_lance; kkp_knight; kkp_silver; kkp_gold; kkp_bishop; kkp_rook; -1; kkp_pro_pawn; kkp_pro_lance; kkp_pro_knight; kkp_pro_silver; -1; kkp_horse; kkp_dragon |]
let kkp_hand_start_index = [| -1; kkp_hand_pawn; kkp_hand_lance; kkp_hand_knight; kkp_hand_silver; kkp_hand_gold; kkp_hand_bishop; kkp_hand_rook |]
let mutable pp_start_index:int[,] = Array2D.zeroCreate<int> 2 16
pp_start_index.[0, 0] <- -1
pp_start_index.[0, 1] <- b_pawn
pp_start_index.[0, 2] <- b_lance
pp_start_index.[0, 3] <- b_knight
pp_start_index.[0, 4] <- b_silver
pp_start_index.[0, 5] <- b_gold
pp_start_index.[0, 6] <- b_bishop
pp_start_index.[0, 7] <- b_rook
pp_start_index.[0, 8] <- -1
pp_start_index.[0, 9] <- b_pro_pawn
pp_start_index.[0, 10] <- b_pro_lance
pp_start_index.[0, 11] <- b_pro_knight
pp_start_index.[0, 12] <- b_pro_silver
pp_start_index.[0, 13] <- -1
pp_start_index.[0, 14] <- b_horse
pp_start_index.[0, 15] <- b_dragon
pp_start_index.[1, 0] <- -1
pp_start_index.[1, 1] <- w_pawn
pp_start_index.[1, 2] <- w_lance
pp_start_index.[1, 3] <- w_knight
pp_start_index.[1, 4] <- w_silver
pp_start_index.[1, 5] <- w_gold
pp_start_index.[1, 6] <- w_bishop
pp_start_index.[1, 7] <- w_rook
pp_start_index.[1, 8] <- -1
pp_start_index.[1, 9] <- w_pro_pawn
pp_start_index.[1, 10] <- w_pro_lance
pp_start_index.[1, 11] <- w_pro_knight
pp_start_index.[1, 12] <- w_pro_silver
pp_start_index.[1, 13] <- -1
pp_start_index.[1, 14] <- w_horse
pp_start_index.[1, 15] <- w_dragon
let mutable pp_hand_start_index:int[,] = Array2D.zeroCreate<int> Color_NB Piece_NB
pp_hand_start_index.[0, 0] <- 0
pp_hand_start_index.[0, 1] <- b_hand_pawn
pp_hand_start_index.[0, 2] <- b_hand_lance
pp_hand_start_index.[0, 3] <- b_hand_knight
pp_hand_start_index.[0, 4] <- b_hand_silver
pp_hand_start_index.[0, 5] <- b_hand_gold
pp_hand_start_index.[0, 6] <- b_hand_bishop
pp_hand_start_index.[0, 7] <- b_hand_rook
pp_hand_start_index.[1, 0] <- 0
pp_hand_start_index.[1, 1] <- w_hand_pawn
pp_hand_start_index.[1, 2] <- w_hand_lance
pp_hand_start_index.[1, 3] <- w_hand_knight
pp_hand_start_index.[1, 4] <- w_hand_silver
pp_hand_start_index.[1, 5] <- w_hand_gold
pp_hand_start_index.[1, 6] <- w_hand_bishop
pp_hand_start_index.[1, 7] <- w_hand_rook
let mutable kkp_index_table:int[,,] = Array3D.zeroCreate<int> Color_NB Piece_NB Square_NB
let mutable pp_index_table:int[,,] = Array3D.zeroCreate<int> Color_NB Piece_NB Square_NB
let mutable fv_kkp:int16[,,] = Array3D.zeroCreate<int16> Square_NB Square_NB kkp_end
let mutable fv_kpp:int16[,,] = Array3D.zeroCreate<int16> Square_NB pp_end pp_end
let file_name_kkp = "fv_kkp.bin"
let file_name_kpp = "fv_kpp.bin"

let InitKKPIndex() =
    for c in 0 .. Color_NB - 1 do
        for p in 1 .. Piece_NB - 1 do
            if p <> int(Piece.None) && p <> int(Piece.King) then
                for s in 0 .. Square_NB - 1 do
                    let mutable pos = s
                    if c = int(Color.White) then
                        pos <- Rev_Sq.[pos]
                    kkp_index_table.[c, p, s] <- kkp_start_index[p] + pos
                    pp_index_table.[c, p, s] <- pp_start_index[c, p] + pos

// ランダムに初期化する。差分計算のテスト用。
let RandomInit() = 
    for i in 0 .. Square_NB - 1 do
        for j in 0 .. Square_NB - 1 do
            for k in 0 .. kkp_end - 1 do
                fv_kkp.[i, j, k] <- int16(System.Random().Next(0, 16))

    for i in 0 .. Square_NB - 1 do
        for j in 0 .. pp_end - 1 do
            for k in 0 .. pp_end - 1 do
                fv_kpp.[i, j, k] <- int16(System.Random().Next(0, 16))

let Init() = 
    fv_kkp <- Array3D.zeroCreate<int16> Square_NB Square_NB kkp_end
    fv_kpp <- Array3D.zeroCreate<int16> Square_NB pp_end pp_end

let Load() = 
    //InitKKPIndex()
    if System.IO.File.Exists(file_name_kkp) then
        use fs = new System.IO.FileStream(file_name_kkp, System.IO.FileMode.Open)
        use br = new System.IO.BinaryReader(fs)
        for i in 0 .. Square_NB - 1 do
            for j in 0 .. Square_NB - 1 do
                for k in 0 .. kkp_end - 1 do
                    fv_kkp.[i, j, k] <- br.ReadInt16()
    if System.IO.File.Exists(file_name_kpp) then
        use fs = new System.IO.FileStream(file_name_kpp, System.IO.FileMode.Open)
        use br = new System.IO.BinaryReader(fs)
        for i in 0 .. Square_NB - 1 do
            for j in 0 .. pp_end - 1 do
                for k in 0 .. pp_end - 1 do
                    fv_kpp.[i, j, k] <- br.ReadInt16()

// 最大値を表示する処理を入れた方が良いかもしれない。
let Save() =
    use fs = new System.IO.FileStream(file_name_kkp, System.IO.FileMode.Create)
    use bw = new System.IO.BinaryWriter(fs)
    for i in 0 .. Square_NB - 1 do
        for j in 0 .. Square_NB - 1 do
            for k in 0 .. kkp_end - 1 do
                bw.Write(fv_kkp.[i, j, k])
    use fs2 = new System.IO.FileStream(file_name_kpp, System.IO.FileMode.Create)
    use bw2 = new System.IO.BinaryWriter(fs2)
    for i in 0 .. Square_NB - 1 do
        for j in 0 .. pp_end - 1 do
            for k in 0 .. pp_end - 1 do
                bw2.Write(fv_kpp.[i, j, k])