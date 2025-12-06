[<AutoOpen>]

module Common

//open System.Numerics
open System.Collections.Generic

open System.IO

let Square_NB: int = 81
let Color_NB: int = 2
let Piece_NB: int = 16
let No_Pro_Piece_NB: int = 8
let NFile: int = 9
let NRank: int = 9
let Label_NB: int = 32
let Moves_Max: int = 700
let Piece_Can_Drop_NB: int = 7
let Ply_Max: int = 1024
let Ply_Inc: int = 8
let Max_Ply: int = 1024
let Value_Max: int = 32768
let Value_Min: int = -Value_Max + 1;
let Value_Draw: int = 0
let Promote: int = 8
let KKP_END: int = 1091
let PP_END: int = 2182
let FV_SCALE: int = 32
let PV_LIMIT: int = 128
let Hand_Pawn :int = 1
let Hand_Lance :int = 32
let Hand_Knight :int = 256
let Hand_Silver :int = 2048
let Hand_Gold :int = 16384
let Hand_Bishop :int = 131072
let Hand_Rook :int = 524288
let Hand_Hash = [| 0; Hand_Pawn; Hand_Lance; Hand_Knight; Hand_Silver; Hand_Gold; Hand_Bishop; Hand_Rook |]
let Hand_Rev_Bit = [| 0; 0; 5; 8; 11; 14; 17; 19 |] 
let Hand_Mask = [| 0; 31; 224; 1792; 14336; 114688; 393216; 1572864 |]
let Square_Edge = [| true; true; true; true; true; true; true; true; true;
                     true; false; false; false; false; false; false; false; true;
                     true; false; false; false; false; false; false; false; true;
                     true; false; false; false; false; false; false; false; true;
                     true; false; false; false; false; false; false; false; true;
                     true; false; false; false; false; false; false; false; true;
                     true; false; false; false; false; false; false; false; true;
                     true; false; false; false; false; false; false; false; true;
                     true; true; true; true; true; true; true; true; true |]
let Str_CSA = [|"91"; "81"; "71"; "61"; "51"; "41"; "31"; "21"; "11";
                "92"; "82"; "72"; "62"; "52"; "42"; "32"; "22"; "12";
                "93"; "83"; "73"; "63"; "53"; "43"; "33"; "23"; "13";
                "94"; "84"; "74"; "64"; "54"; "44"; "34"; "24"; "14";
                "95"; "85"; "75"; "65"; "55"; "45"; "35"; "25"; "15";
                "96"; "86"; "76"; "66"; "56"; "46"; "36"; "26"; "16";
                "97"; "87"; "77"; "67"; "57"; "47"; "37"; "27"; "17";
                "98"; "88"; "78"; "68"; "58"; "48"; "38"; "28"; "18";
                "99"; "89"; "79"; "69"; "59"; "49"; "39"; "29"; "19";
                "00"; "00"; "00"; "00"; "00"; "00"; "00"|] 

let Str_USI = [|"9a"; "8a"; "7a"; "6a"; "5a"; "4a"; "3a"; "2a"; "1a";
                "9b"; "8b"; "7b"; "6b"; "5b"; "4b"; "3b"; "2b"; "1b";
                "9c"; "8c"; "7c"; "6c"; "5c"; "4c"; "3c"; "2c"; "1c";
                "9d"; "8d"; "7d"; "6d"; "5d"; "4d"; "3d"; "2d"; "1d";
                "9e"; "8e"; "7e"; "6e"; "5e"; "4e"; "3e"; "2e"; "1e";
                "9f"; "8f"; "7f"; "6f"; "5f"; "4f"; "3f"; "2f"; "1f";
                "9g"; "8g"; "7g"; "6g"; "5g"; "4g"; "3g"; "2g"; "1g";
                "9h"; "8h"; "7h"; "6h"; "5h"; "4h"; "3h"; "2h"; "1h";
                "9i"; "8i"; "7i"; "6i"; "5i"; "4i"; "3i"; "2i"; "1i"|]

let USI_TO_SQ = Dictionary<string, int>(dict [ "9a", 0;  "8a", 1;  "7a", 2;  "6a", 3;  "5a", 4;  "4a", 5;  "3a", 6;  "2a", 7;  "1a", 8;
                                     "9b", 9;  "8b", 10; "7b", 11; "6b", 12; "5b", 13; "4b", 14; "3b", 15; "2b", 16; "1b", 17;
                                     "9c", 18; "8c", 19; "7c", 20; "6c", 21; "5c", 22; "4c", 23; "3c", 24; "2c", 25; "1c", 26;
                                     "9d", 27; "8d", 28; "7d", 29; "6d", 30; "5d", 31; "4d", 32; "3d", 33; "2d", 34; "1d", 35;
                                     "9e", 36; "8e", 37; "7e", 38; "6e", 39; "5e", 40; "4e", 41; "3e", 42; "2e", 43; "1e", 44;
                                     "9f", 45; "8f", 46; "7f", 47; "6f", 48; "5f", 49; "4f", 50; "3f", 51; "2f", 52; "1f", 53;
                                     "9g", 54; "8g", 55; "7g", 56; "6g", 57; "5g", 58; "4g", 59; "3g", 60; "2g", 61; "1g", 62;
                                     "9h", 63; "8h", 64; "7h", 65; "6h", 66; "5h", 67; "4h", 68; "3h", 69; "2h", 70; "1h", 71;
                                     "9i", 72; "8i", 73; "7i", 74; "6i", 75; "5i", 76; "4i", 77; "3i", 78; "2i", 79; "1i", 80 ])

let Str_Piece = [| "None"; "FU"; "KY"; "KE"; "GI"; "KI"; "KA"; "HI"; "OU"; "TO"; "NY"; "NK"; "NG"; "None"; "UM"; "RY"|]

let Str_Piece_JP = [| "None"; "歩"; "香"; "桂"; "銀"; "金"; "角"; "飛"; "玉"; "と"; "杏"; "圭"; "全"; "None"; "馬"; "龍"|]

let Set_Empty_Num = [|""; "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"|]

let Set_Hand_Num = [|"0"; "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"|]

type Color =  | Black = 0 | White = 1

type Result = | BlackWin = 0 | WhiteWin = 1 | Draw = 2

type Piece = | Empty = 0 | Pawn = 1 | Lance = 2 | Knight = 3 | Silver = 4 | Gold = 5 | Bishop = 6 | Rook = 7 | King = 8 | Pro_Pawn = 9 | Pro_Lance = 10 | Pro_Knight = 11 | Pro_Silver = 12 | None = 13 | Horse = 14 | Dragon = 15

let CSA_TO_SQ = Dictionary<string, int>(dict [ "91", 0;  "81", 1;  "71", 2;  "61", 3;  "51", 4;  "41", 5;  "31", 6;  "21", 7;  "11", 8;
                                     "92", 9;  "82", 10; "72", 11; "62", 12; "52", 13; "42", 14; "32", 15; "22", 16; "12", 17;
                                     "93", 18; "83", 19; "73", 20; "63", 21; "53", 22; "43", 23; "33", 24; "23", 25; "13", 26;
                                     "94", 27; "84", 28; "74", 29; "64", 30; "54", 31; "44", 32; "34", 33; "24", 34; "14", 35;
                                     "95", 36; "85", 37; "75", 38; "65", 39; "55", 40; "45", 41; "35", 42; "25", 43; "15", 44;
                                     "96", 45; "86", 46; "76", 47; "66", 48; "56", 49; "46", 50; "36", 51; "26", 52; "16", 53;
                                     "97", 54; "87", 55; "77", 56; "67", 57; "57", 58; "47", 59; "37", 60; "27", 61; "17", 62;
                                     "98", 63; "88", 64; "78", 65; "68", 66; "58", 67; "48", 68; "38", 69; "28", 70; "18", 71;
                                     "99", 72; "89", 73; "79", 74; "69", 75; "59", 76; "49", 77; "39", 78; "29", 79; "19", 80; "00", 81 ])

let CSA_TO_PC = Dictionary<string, Piece>(dict [ "FU", Piece.Pawn; "KY", Piece.Lance; "KE", Piece.Knight; "GI", Piece.Silver; "KI", Piece.Gold; "KA", Piece.Bishop; "HI", Piece.Rook; "OU", Piece.King; "TO", Piece.Pro_Pawn; "NY", Piece.Pro_Lance; "NK", Piece.Pro_Knight; "NG", Piece.Pro_Silver; "None", Piece.None; "UM", Piece.Horse; "RY", Piece.Dragon ])

let Str_SFEN_Pc = Dictionary<int, string>(dict [ 1, "P"; 2, "L"; 3, "N"; 4, "S"; 5, "G"; 6, "B"; 7, "R"; 8, "K"; 9, "+P"; 10, "+L"; 11, "+N"; 12, "+S"; 13, ""; 14, "+B"; 15, "+R"; -1, "p"; -2, "l"; -3, "n"; -4, "s"; -5, "g"; -6, "b"; -7, "r"; -8, "k"; -9, "+p"; -10, "+l"; -11, "+n"; -12, "+s"; -13, ""; -14, "+b"; -15, "+r"; ])

let Str_Kanji_Pc = Dictionary<int, string>(dict [ 1, " 歩"; 2, " 香"; 3, " 桂"; 4, " 銀"; 5, " 金"; 6, " 角"; 7, " 飛"; 8, " 玉"; 9, " と"; 10, " 杏"; 11, " 圭"; 12, " 全"; 13, ""; 14, " 馬"; 15, " 龍"; -1, "v歩"; -2, "v香"; -3, "v桂"; -4, "v銀"; -5, "v金"; -6, "v角"; -7, "v飛"; -8, "v玉"; -9, "vと"; -10, "v杏"; -11, "v圭"; -12, "v全"; -13, ""; -14, "v馬"; -15, "v龍"; ])

let Int_Pc = Dictionary<string, int>(dict [ "P", 1; "L", 2; "N", 3; "S", 4; "G", 5; "B", 6; "R", 7; "K", 8; "+P", 9; "+L", 10; "+N", 11; "+S", 12; "", 13; "+B", 14; "+R", 15; "p", -1; "l", -2; "n", -3 ; "s", -4; "g", -5 ; "b", -6 ; "r", -7; "k", -8; "+p", -9; "+l", -10; "+n", -11; "+s", -12; "", -13; "+b", -14; "+r", -15 ])

let Int_Empty_Num = new Dictionary<string, int>(dict[ "1", 1; "2", 2; "3", 3; "4", 4; "5", 5; "6", 6; "7", 7; "8", 8; "9", 9 ])

let Int_Hand_Num = new Dictionary<string, int>(dict[ "0", 0; "1", 1; "2", 2; "3", 3; "4", 4; "5", 5; "6", 6; "7", 7; "8", 8; "9", 9 ])

let Str_Color = new Dictionary<Color, string>(dict[ Color.Black, "b"; Color.White, "w"])

let Num_Color = new Dictionary<string, Color>(dict[ "b", Color.Black; "w", Color.White ])

type Direction = | Direc_Misc = 0 | Direc_Diag1_U2d = 8 | Direc_Diag1_D2u = -8 | Direc_Diag2_U2d = 10 | Direc_Diag2_D2u = -10 | Direc_File_U2d = 9 | Direc_File_D2u = -9 | Direc_Rank_L2r = 1 | Direc_Rank_R2l = -1 | Direc_Knight_L_U2d = 19 | Direc_Knight_R_U2d = 17 | Direc_Knight_L_D2u = -17 | Direc_Knight_R_D2u = -19

type File = | File1 = 0 | File2 = 1 | File3 = 2 | File4 = 3 | File5 = 4 | File6 = 5 | File7 = 6 | File8 = 7 | File9 = 8

type Rank = | Rank1 = 0 | Rank2 = 1 | Rank3 = 2 | Rank4 = 3 | Rank5 = 4 | Rank6 = 5 | Rank7 = 6 | Rank8 = 7 | Rank9 = 8

let FileTable = [| File.File1; File.File2; File.File3; File.File4; File.File5; File.File6; File.File7; File.File8; File.File9;
                   File.File1; File.File2; File.File3; File.File4; File.File5; File.File6; File.File7; File.File8; File.File9;
                   File.File1; File.File2; File.File3; File.File4; File.File5; File.File6; File.File7; File.File8; File.File9;
                   File.File1; File.File2; File.File3; File.File4; File.File5; File.File6; File.File7; File.File8; File.File9;
                   File.File1; File.File2; File.File3; File.File4; File.File5; File.File6; File.File7; File.File8; File.File9;
                   File.File1; File.File2; File.File3; File.File4; File.File5; File.File6; File.File7; File.File8; File.File9;
                   File.File1; File.File2; File.File3; File.File4; File.File5; File.File6; File.File7; File.File8; File.File9;
                   File.File1; File.File2; File.File3; File.File4; File.File5; File.File6; File.File7; File.File8; File.File9;
                   File.File1; File.File2; File.File3; File.File4; File.File5; File.File6; File.File7; File.File8; File.File9; |]

let RankTable = [| Rank.Rank1; Rank.Rank1; Rank.Rank1; Rank.Rank1; Rank.Rank1; Rank.Rank1; Rank.Rank1; Rank.Rank1; Rank.Rank1;
                   Rank.Rank2; Rank.Rank2; Rank.Rank2; Rank.Rank2; Rank.Rank2; Rank.Rank2; Rank.Rank2; Rank.Rank2; Rank.Rank2;
                   Rank.Rank3; Rank.Rank3; Rank.Rank3; Rank.Rank3; Rank.Rank3; Rank.Rank3; Rank.Rank3; Rank.Rank3; Rank.Rank3;
                   Rank.Rank4; Rank.Rank4; Rank.Rank4; Rank.Rank4; Rank.Rank4; Rank.Rank4; Rank.Rank4; Rank.Rank4; Rank.Rank4;
                   Rank.Rank5; Rank.Rank5; Rank.Rank5; Rank.Rank5; Rank.Rank5; Rank.Rank5; Rank.Rank5; Rank.Rank5; Rank.Rank5;
                   Rank.Rank6; Rank.Rank6; Rank.Rank6; Rank.Rank6; Rank.Rank6; Rank.Rank6; Rank.Rank6; Rank.Rank6; Rank.Rank6;
                   Rank.Rank7; Rank.Rank7; Rank.Rank7; Rank.Rank7; Rank.Rank7; Rank.Rank7; Rank.Rank7; Rank.Rank7; Rank.Rank7;
                   Rank.Rank8; Rank.Rank8; Rank.Rank8; Rank.Rank8; Rank.Rank8; Rank.Rank8; Rank.Rank8; Rank.Rank8; Rank.Rank8;
                   Rank.Rank9; Rank.Rank9; Rank.Rank9; Rank.Rank9; Rank.Rank9; Rank.Rank9; Rank.Rank9; Rank.Rank9; Rank.Rank9 |]


let ABB_Mask:System.UInt128[] = [|System.UInt128.Parse("1208925819614629174706176");System.UInt128.Parse("604462909807314587353088");System.UInt128.Parse("302231454903657293676544");System.UInt128.Parse("151115727451828646838272");System.UInt128.Parse("75557863725914323419136");System.UInt128.Parse("37778931862957161709568");System.UInt128.Parse("18889465931478580854784");System.UInt128.Parse("9444732965739290427392");System.UInt128.Parse("4722366482869645213696");
                            System.UInt128.Parse("2361183241434822606848");System.UInt128.Parse("1180591620717411303424");System.UInt128.Parse("590295810358705651712");System.UInt128.Parse("295147905179352825856");System.UInt128.Parse("147573952589676412928");System.UInt128.Parse("73786976294838206464");System.UInt128.Parse("36893488147419103232");System.UInt128.Parse("18446744073709551616");System.UInt128.Parse("9223372036854775808");
                            System.UInt128.Parse("4611686018427387904");System.UInt128.Parse("2305843009213693952");System.UInt128.Parse("1152921504606846976");System.UInt128.Parse("576460752303423488");System.UInt128.Parse("288230376151711744");System.UInt128.Parse("144115188075855872");System.UInt128.Parse("72057594037927936");System.UInt128.Parse("36028797018963968");System.UInt128.Parse("18014398509481984");
                            System.UInt128.Parse("9007199254740992");System.UInt128.Parse("4503599627370496");System.UInt128.Parse("2251799813685248");System.UInt128.Parse("1125899906842624");System.UInt128.Parse("562949953421312");System.UInt128.Parse("281474976710656");System.UInt128.Parse("140737488355328");System.UInt128.Parse("70368744177664");System.UInt128.Parse("35184372088832");
                            System.UInt128(0UL, 17592186044416UL);System.UInt128(0UL, 8796093022208UL);System.UInt128(0UL, 4398046511104UL);System.UInt128(0UL, 2199023255552UL);System.UInt128(0UL, 1099511627776UL);System.UInt128(0UL, 549755813888UL);System.UInt128(0UL, 274877906944UL);System.UInt128(0UL, 137438953472UL);System.UInt128(0UL, 68719476736UL);
                            System.UInt128(0UL, 34359738368UL);System.UInt128(0UL, 17179869184UL);System.UInt128(0UL, 8589934592UL);System.UInt128(0UL, 4294967296UL);System.UInt128(0UL, 2147483648UL);System.UInt128(0UL, 1073741824UL);System.UInt128(0UL, 536870912UL);System.UInt128(0UL, 268435456UL);System.UInt128(0UL, 134217728UL);
                            System.UInt128(0UL, 67108864UL);System.UInt128(0UL, 33554432UL);System.UInt128(0UL, 16777216UL);System.UInt128(0UL, 8388608UL);System.UInt128(0UL, 4194304UL);System.UInt128(0UL, 2097152UL);System.UInt128(0UL, 1048576UL);System.UInt128(0UL, 524288UL);System.UInt128(0UL, 262144UL);
                            System.UInt128(0UL, 131072UL);System.UInt128(0UL, 65536UL);System.UInt128(0UL, 32768UL);System.UInt128(0UL, 16384UL);System.UInt128(0UL, 8192UL);System.UInt128(0UL, 4096UL);System.UInt128(0UL, 2048UL);System.UInt128(0UL, 1024UL);System.UInt128(0UL, 512UL);
                            System.UInt128(0UL, 256UL);System.UInt128(0UL, 128UL);System.UInt128(0UL, 64UL);System.UInt128(0UL, 32UL);System.UInt128(0UL, 16UL);System.UInt128(0UL, 8UL);System.UInt128(0UL, 4UL);System.UInt128(0UL, 2UL);System.UInt128(0UL, 1UL)|]

let mutable ABB_File_Mask_Ex:System.UInt128[] = [|System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();
                            System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();
                            System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();
                            System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();
                            System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();
                            System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();
                            System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();
                            System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();
                            System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128()|]

let mutable ABB_Rank_Mask_Ex:System.UInt128[] = [|System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();
                            System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();
                            System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();
                            System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();
                            System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();
                            System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();
                            System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();
                            System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();
                            System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128()|]

let mutable ABB_Diag1_Mask_Ex:System.UInt128[] = [|System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();
                            System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();
                            System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();
                            System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();
                            System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();
                            System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();
                            System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();
                            System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();
                            System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128()|]

let mutable ABB_Diag2_Mask_Ex:System.UInt128[] = [|System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();
                            System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();
                            System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();
                            System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();
                            System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();
                            System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();
                            System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();
                            System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();
                            System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128()|]

let mutable ABB_Lance_Mask_Ex:System.UInt128[,] = Array2D.init Color_NB Square_NB (fun i j -> System.UInt128())

let mutable ABB_Obstacles:System.UInt128[,] = Array2D.init Square_NB Square_NB (fun i j -> System.UInt128())

let mutable ABB_Cross_Mask_Ex:System.UInt128[] = [|System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();
                            System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();
                            System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();
                            System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();
                            System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();
                            System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();
                            System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();
                            System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();
                            System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128()|]

let mutable ABB_Diagonal_Mask_Ex:System.UInt128[] = [|System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();
                            System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();
                            System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();
                            System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();
                            System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();
                            System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();
                            System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();
                            System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();
                            System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128()|]

let mutable ABB_Rank_Attacks:Dictionary<System.UInt128, System.UInt128>[] = Array.init Square_NB (fun _ -> Dictionary<System.UInt128, System.UInt128>())

let mutable ABB_File_Attacks:Dictionary<System.UInt128, System.UInt128>[] = Array.init Square_NB (fun _ -> Dictionary<System.UInt128, System.UInt128>())

let mutable ABB_Diag1_Attacks:Dictionary<System.UInt128, System.UInt128>[] = Array.init Square_NB (fun _ -> Dictionary<System.UInt128, System.UInt128>())

let mutable ABB_Diag2_Attacks:Dictionary<System.UInt128, System.UInt128>[] = Array.init Square_NB (fun _ -> Dictionary<System.UInt128, System.UInt128>())

let mutable ABB_Cross_Attacks:Dictionary<System.UInt128, System.UInt128>[] = Array.init Square_NB (fun _ -> Dictionary<System.UInt128, System.UInt128>())

let mutable ABB_Diagonal_Attacks:Dictionary<System.UInt128, System.UInt128>[] = Array.init Square_NB (fun _ -> Dictionary<System.UInt128, System.UInt128>())

let mutable ABB_Lance_Attacks:Dictionary<System.UInt128, System.UInt128>[,] = Array2D.init Color_NB Square_NB  (fun i j -> Dictionary<System.UInt128, System.UInt128>())

let mutable Piece_Table:Dictionary<int, List<int>>[] = Array.init Color_NB (fun _ -> Dictionary<int, List<int>>())

// int8にしたい
let mutable Adirec:int[,] = Array2D.init Square_NB Square_NB (fun i j -> 0)

let mutable ABB_Stomach_Attacks:System.UInt128[] = [|System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();
                            System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();
                            System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();
                            System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();
                            System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();
                            System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();
                            System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();
                            System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();
                            System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128();System.UInt128()|]

let mutable ABB_2Up_Attacks:System.UInt128[,] = Array2D.init Color_NB Square_NB (fun i j -> System.UInt128())

let mutable ABB_3Up_Attacks:System.UInt128[,] = Array2D.init Color_NB Square_NB (fun i j -> System.UInt128())

let mutable ABB_2Up_3Sq_Attacks:System.UInt128[,] = Array2D.init Color_NB Square_NB (fun i j -> System.UInt128())

let mutable ABB_Diag_Back_Attacks:System.UInt128[,] = Array2D.init Color_NB Square_NB (fun i j -> System.UInt128())

let mutable ABB_Piece_Attacks:System.UInt128[,,] = Array3D.init Color_NB Piece_NB Square_NB (fun i j k -> System.UInt128())

let BB_Black_Position:System.UInt128 = System.UInt128(uint64(0), uint64(0x7FFFFFF));
let BB_White_Position:System.UInt128 = System.UInt128.Parse("2417851621214859839930368")
let BB_Color_Position:System.UInt128[] = [|BB_Black_Position; BB_White_Position|]
let BB_Rev_Color_Position:System.UInt128[] = [|BB_White_Position; BB_Black_Position|]
let BB_DMZ:System.UInt128 = System.UInt128.Parse("70368609959936")
let BB_Full:System.UInt128 = System.UInt128.Parse("1208925819614629174706175")
let BB_File:System.UInt128[] = [|System.UInt128.Parse("1211291623566908292464896");System.UInt128.Parse("605645811783454146232448");System.UInt128.Parse("302822905891727073116224");System.UInt128.Parse("151411452945863536558112");System.UInt128.Parse("75705726472931768279056");System.UInt128.Parse("37852863236465884139528");System.UInt128.Parse("18926431618232942069764"); System.UInt128.Parse("9463215809116471034882"); System.UInt128.Parse("4731607904558235517441")|]
let BB_Rank:System.UInt128[] = [|System.UInt128.Parse("2413129272746388704198656");System.UInt128.Parse("4713143110832790437888");System.UInt128.Parse("9205357638345293824");System.UInt128.Parse("17979214137393152");System.UInt128.Parse("35115652612096");System.UInt128.Parse("68585259008");System.UInt128.Parse("133955584");System.UInt128.Parse("261632");System.UInt128.Parse("512")|]
let BB_Knight_Must_Promote:System.UInt128[] = [|System.UInt128.Parse("2417842415857221494636544"); System.UInt128.Parse("262143")|]
let BB_Not_Knight_Must_Promote:System.UInt128[] = [|System.UInt128.Parse("9223372036854775808"); System.UInt128.Parse("2417851639229258349150208")|]
let mutable BB_Lance_Mask:System.UInt128[,] = Array2D.init Square_NB Square_NB (fun i j -> System.UInt128())
BB_Lance_Mask[0,1] <- System.UInt128.Parse("2413129272746388704198656")
BB_Lance_Mask[0,2] <- System.UInt128.Parse("2417842415857221494636544")
BB_Lance_Mask[0,3] <- System.UInt128.Parse("2417851621214859839930368")
BB_Lance_Mask[0,4] <- System.UInt128.Parse("2417851639194073977323520")
BB_Lance_Mask[0,5] <- System.UInt128.Parse("2417851639229189629935616")
BB_Lance_Mask[0,6] <- System.UInt128.Parse("2417851639229258215194624")
BB_Lance_Mask[0,7] <- System.UInt128.Parse("2417851639229258349150208")
BB_Lance_Mask[0,8] <- System.UInt128.Parse("2417851639229258349411840")
BB_Lance_Mask[1,0] <- System.UInt128.Parse("4722366482869645213695")
BB_Lance_Mask[1,1] <- System.UInt128.Parse("9223372036854775807")
BB_Lance_Mask[1,2] <- System.UInt128.Parse("18014398509481983")
BB_Lance_Mask[1,3] <- System.UInt128.Parse("35184372088831")
BB_Lance_Mask[1,4] <- System.UInt128.Parse("68719476735")
BB_Lance_Mask[1,5] <- System.UInt128.Parse("134217727")
BB_Lance_Mask[1,6] <- System.UInt128.Parse("262143")
BB_Lance_Mask[1,7] <- System.UInt128.Parse("511")
let RandM:uint = uint(397)
let RandN:uint = uint(624)
let MaskU:uint = uint(0x80000000)
let MaskL:uint = uint(0x7fffffff)
let Mask32:uint = uint(0xffffffff)
let BB_Pawn_Lance_Can_Drop:System.UInt128[] = [|System.UInt128.Parse("4722366482869645213695"); System.UInt128.Parse("2417851639229258349411840")|]
let BB_Knight_Can_Drop:System.UInt128[] = [|System.UInt128.Parse("9223372036854775807"); System.UInt128.Parse("2417851639229258349150208")|]
let BB_Others_Can_Drop:System.UInt128 = System.UInt128.Parse("2417851639229258349412351")
let Delta_Table:int[] = [|-9; 9|]
let Sign_Table:int[] = [|-1; 1|]
let Set_Long_Attack_Pieces:int[] = [| int(Piece.Lance); int(Piece.Bishop); int(Piece.Rook); int(Piece.Horse); int(Piece.Dragon); -int(Piece.Lance); -int(Piece.Bishop); -int(Piece.Rook); -int(Piece.Horse); -int(Piece.Dragon) |]
let Set_Piece_Can_Promote0:int[] = [| int(Piece.Pawn); int(Piece.Lance); int(Piece.Knight) |]
let Set_Piece_Can_Promote1:int[] = [| int(Piece.Silver); int(Piece.Bishop); int(Piece.Rook) |]
let LongPieces:List<int> = List<int>([int(Piece.Bishop); int(Piece.Rook)])
let LongPieces2:List<int> = List<int>([int(Piece.Bishop); int(Piece.Rook); int(Piece.Lance)])
let ShortPieces:List<int> = List<int>([int(Piece.Pawn); int(Piece.Lance)])
let GoldSilver:List<int> = List<int>([int(Piece.Gold); int(Piece.Pro_Pawn); int(Piece.Pro_Lance); int(Piece.Pro_Knight); int(Piece.Pro_Silver); int(Piece.Silver)])
let BB_Pawn_Mask:System.UInt128[] = [| System.UInt128.Parse("35184372088831"); System.UInt128.Parse("2417851639229189629935616") |]
//let b = BigInteger.Parse("340282366920938463463374607431768211456")

let mutable filePath = "abb_file_mask_ex.txt"
let mutable lines = File.ReadAllLines(filePath)
for line in lines do
    let l = line.Split([|','|])
    let sq = int l.[0]
    let v = System.UInt128.Parse(l.[1])
    ABB_File_Mask_Ex.[sq - 1]<- v

filePath <- "abb_rank_mask_ex.txt"
lines<-File.ReadAllLines(filePath)
for line in lines do
    let l = line.Split([|','|])
    let sq = int l.[0]
    let v = System.UInt128.Parse(l.[1])
    ABB_Rank_Mask_Ex.[sq - 1]<- v

filePath <- "abb_diag1_mask_ex.txt"
lines<-File.ReadAllLines(filePath)
for line in lines do
    let l = line.Split([|','|])
    let sq = int l.[0]
    let v = System.UInt128.Parse(l.[1])
    ABB_Diag1_Mask_Ex.[sq - 1]<- v

filePath <- "abb_diag2_mask_ex.txt"
lines<-File.ReadAllLines(filePath)
for line in lines do
    let l = line.Split([|','|])
    let sq = int l.[0]
    let v = System.UInt128.Parse(l.[1])
    ABB_Diag2_Mask_Ex.[sq - 1]<- v

filePath <- "abb_lance_mask_ex.txt"
lines<-File.ReadAllLines(filePath)
for line in lines do
    let l = line.Split([|','|])
    let c = int l.[0]
    let sq = int l.[1]
    let v = System.UInt128.Parse(l.[2])
    ABB_Lance_Mask_Ex.[c - 1, sq - 1]<- v

filePath <- "abb_obstacles.txt"
lines<-File.ReadAllLines(filePath)
for line in lines do
    let l = line.Split([|','|])
    let c = int l.[0]
    let sq = int l.[1]
    let v = System.UInt128.Parse(l.[2])
    ABB_Obstacles.[c - 1, sq - 1]<- v

filePath <- "abb_cross_mask_ex.txt"
lines<-File.ReadAllLines(filePath)
for line in lines do
    let l = line.Split([|','|])
    let sq = int l.[0]
    let v = System.UInt128.Parse(l.[1])
    ABB_Cross_Mask_Ex.[sq - 1]<- v

filePath <- "abb_diagonal_mask_ex.txt"
lines<-File.ReadAllLines(filePath)
for line in lines do
    let l = line.Split([|','|])
    let sq = int l.[0]
    let v = System.UInt128.Parse(l.[1])
    ABB_Diagonal_Mask_Ex.[sq - 1]<- v

filePath <- "abb_rank_attacks.txt"
lines<-File.ReadAllLines(filePath)
for line in lines do
    let l = line.Split([|','|])
    let sq = int l.[0]
    let h = System.UInt128.Parse(l.[1])
    let v = System.UInt128.Parse(l.[2])
    ABB_Rank_Attacks.[sq - 1].Add(h, v)

filePath <- "abb_file_attacks.txt"
lines<-File.ReadAllLines(filePath)
for line in lines do
    let l = line.Split([|','|])
    let sq = int l.[0]
    let h = System.UInt128.Parse(l.[1])
    let v = System.UInt128.Parse(l.[2])
    ABB_File_Attacks.[sq - 1].Add(h, v)

filePath <- "abb_diag1_attacks.txt"
lines<-File.ReadAllLines(filePath)
for line in lines do
    let l = line.Split([|','|])
    let sq = int l.[0]
    let h = System.UInt128.Parse(l.[1])
    let v = System.UInt128.Parse(l.[2])
    ABB_Diag1_Attacks.[sq - 1].Add(h, v)

filePath <- "abb_diag2_attacks.txt"
lines<-File.ReadAllLines(filePath)
for line in lines do
    let l = line.Split([|','|])
    let sq = int l.[0]
    let h = System.UInt128.Parse(l.[1])
    let v = System.UInt128.Parse(l.[2])
    ABB_Diag2_Attacks.[sq - 1].Add(h, v)

filePath <- "abb_cross_attacks.txt"
lines<-File.ReadAllLines(filePath)
for line in lines do
    let l = line.Split([|','|])
    let sq = int l.[0]
    let h = System.UInt128.Parse(l.[1])
    let v = System.UInt128.Parse(l.[2])
    ABB_Cross_Attacks.[sq - 1].Add(h, v)

filePath <- "abb_diagonal_attacks.txt"
lines<-File.ReadAllLines(filePath)
for line in lines do
    let l = line.Split([|','|])
    let sq = int l.[0]
    let h = System.UInt128.Parse(l.[1])
    let v = System.UInt128.Parse(l.[2])
    ABB_Diagonal_Attacks.[sq - 1].Add(h, v)

filePath <- "abb_lance_attacks.txt"
lines<-File.ReadAllLines(filePath)
for line in lines do
    let l = line.Split([|','|])
    let c = int l.[0]
    let sq = int l.[1]
    let h = System.UInt128.Parse(l.[2])
    let v = System.UInt128.Parse(l.[3])
    ABB_Lance_Attacks.[c - 1, sq - 1].Add(h, v)

filePath <- "adirec.txt"
lines<-File.ReadAllLines(filePath)
for line in lines do
    let l = line.Split([|','|])
    let ifrom = int l.[0]
    let ito = int l.[1]
    let idirec = int l.[2]
    Adirec.[ifrom, ito]<- idirec

filePath <- "abb_piece_attacks.txt"
lines<-File.ReadAllLines(filePath)
for line in lines do
    let l = line.Split([|','|])
    let c = int l.[0]
    let pc = int l.[1]
    let sq = int l.[2]
    let v = System.UInt128.Parse(l.[3])
    ABB_Piece_Attacks.[c, pc, sq]<- v

let list:List<int>[] = Array.init 16 (fun _ -> List<int>())
list[0].AddRange([ (int)Piece.Silver; (int)Piece.Gold; (int)Piece.Bishop; (int)Piece.Pro_Pawn; (int)Piece.Pro_Lance; (int)Piece.Pro_Knight; (int)Piece.Pro_Silver; (int)Piece.Horse; (int)Piece.Dragon ])
Piece_Table.[int(Color.Black)].Add((int)Direction.Direc_Diag2_U2d, list[0])
list[1].AddRange([ (int)Piece.Pawn; (int)Piece.Lance; (int)Piece.Silver; (int)Piece.Gold; (int)Piece.Rook; (int)Piece.Pro_Pawn; (int)Piece.Pro_Lance; (int)Piece.Pro_Knight; (int)Piece.Pro_Silver; (int)Piece.Horse; (int)Piece.Dragon ])
Piece_Table.[int(Color.Black)].Add((int)Direction.Direc_File_U2d, list[1])
list[2].AddRange([ (int)Piece.Silver; (int)Piece.Gold; (int)Piece.Bishop; (int)Piece.Pro_Pawn; (int)Piece.Pro_Lance; (int)Piece.Pro_Knight; (int)Piece.Pro_Silver; (int)Piece.Horse; (int)Piece.Dragon ])
Piece_Table.[int(Color.Black)].Add((int)Direction.Direc_Diag1_U2d, list[2])
list[3].AddRange([ (int)Piece.Gold; (int)Piece.Rook; (int)Piece.Pro_Pawn; (int)Piece.Pro_Lance; (int)Piece.Pro_Knight; (int)Piece.Pro_Silver; (int)Piece.Horse; (int)Piece.Dragon ])
Piece_Table.[int(Color.Black)].Add((int)Direction.Direc_Rank_L2r, list[3])
list[4].AddRange([ (int)Piece.Gold; (int)Piece.Rook; (int)Piece.Pro_Pawn; (int)Piece.Pro_Lance; (int)Piece.Pro_Knight; (int)Piece.Pro_Silver; (int)Piece.Horse; (int)Piece.Dragon ])
Piece_Table.[int(Color.Black)].Add((int)Direction.Direc_Rank_R2l, list[4])
list[5].AddRange([ (int)Piece.Silver; (int)Piece.Bishop; (int)Piece.Horse; (int)Piece.Dragon; ])
Piece_Table.[int(Color.Black)].Add((int)Direction.Direc_Diag1_D2u, list[5])
list[6].AddRange([ (int)Piece.Gold; (int)Piece.Rook; (int)Piece.Pro_Pawn; (int)Piece.Pro_Lance; (int)Piece.Pro_Knight; (int)Piece.Pro_Silver; (int)Piece.Horse; (int)Piece.Dragon ])
Piece_Table.[int(Color.Black)].Add((int)Direction.Direc_File_D2u, list[6])
list[7].AddRange([ (int)Piece.Silver; (int)Piece.Bishop; (int)Piece.Horse; (int)Piece.Dragon; ])
Piece_Table.[int(Color.Black)].Add((int)Direction.Direc_Diag2_D2u, list[7])
list[8].AddRange([ (int)Piece.Silver; (int)Piece.Bishop; (int)Piece.Horse; (int)Piece.Dragon; ])
Piece_Table.[int(Color.White)].Add((int)Direction.Direc_Diag2_U2d, list[8])
list[9].AddRange([ (int)Piece.Gold; (int)Piece.Rook; (int)Piece.Pro_Pawn; (int)Piece.Pro_Lance; (int)Piece.Pro_Knight; (int)Piece.Pro_Silver; (int)Piece.Horse; (int)Piece.Dragon ])
Piece_Table.[int(Color.White)].Add((int)Direction.Direc_File_U2d, list[9])
list[10].AddRange([ (int)Piece.Silver; (int)Piece.Bishop; (int)Piece.Horse; (int)Piece.Dragon; ])
Piece_Table.[int(Color.White)].Add((int)Direction.Direc_Diag1_U2d, list[10])
list[11].AddRange([ (int)Piece.Gold; (int)Piece.Rook; (int)Piece.Pro_Pawn; (int)Piece.Pro_Lance; (int)Piece.Pro_Knight; (int)Piece.Pro_Silver; (int)Piece.Horse; (int)Piece.Dragon ])
Piece_Table.[int(Color.White)].Add((int)Direction.Direc_Rank_L2r, list[11])
list[12].AddRange([ (int)Piece.Gold; (int)Piece.Rook; (int)Piece.Pro_Pawn; (int)Piece.Pro_Lance; (int)Piece.Pro_Knight; (int)Piece.Pro_Silver; (int)Piece.Horse; (int)Piece.Dragon ])
Piece_Table.[int(Color.White)].Add((int)Direction.Direc_Rank_R2l, list[12])
list[13].AddRange([ (int)Piece.Silver; (int)Piece.Gold; (int)Piece.Bishop; (int)Piece.Pro_Pawn; (int)Piece.Pro_Lance; (int)Piece.Pro_Knight; (int)Piece.Pro_Silver; (int)Piece.Horse; (int)Piece.Dragon ])
Piece_Table.[int(Color.White)].Add((int)Direction.Direc_Diag1_D2u, list[13])
list[14].AddRange([ (int)Piece.Pawn; (int)Piece.Lance; (int)Piece.Silver; (int)Piece.Gold; (int)Piece.Rook; (int)Piece.Pro_Pawn; (int)Piece.Pro_Lance; (int)Piece.Pro_Knight; (int)Piece.Pro_Silver; (int)Piece.Horse; (int)Piece.Dragon ])
Piece_Table.[int(Color.White)].Add((int)Direction.Direc_File_D2u, list[14])
list[15].AddRange([ (int)Piece.Silver; (int)Piece.Gold; (int)Piece.Bishop; (int)Piece.Pro_Pawn; (int)Piece.Pro_Lance; (int)Piece.Pro_Knight; (int)Piece.Pro_Silver; (int)Piece.Horse; (int)Piece.Dragon ])
Piece_Table.[int(Color.White)].Add((int)Direction.Direc_Diag2_D2u, list[15])

for i in 0.. Square_NB - 1 do
    if FileTable.[i] = File.File1 then
        ABB_Stomach_Attacks.[i] <- ABB_Mask.[i + 1]
    elif FileTable.[i] = File.File9 then
        ABB_Stomach_Attacks.[i] <- ABB_Mask.[i - 1]
    else
        ABB_Stomach_Attacks.[i] <- ABB_Mask.[i - 1] ||| ABB_Mask.[i + 1]

for i in 0.. Color_NB - 1 do
    for j in 0.. Square_NB - 1 do
        if i = int(Color.Black) then
            if j >= 18 then
                ABB_2Up_Attacks.[i, j] <- ABB_Mask.[j - 18]
        else
            if j < 63 then
                ABB_2Up_Attacks.[i, j] <- ABB_Mask.[j + 18]

for i in 0.. Color_NB - 1 do
    for j in 0.. Square_NB - 1 do
        if i = int(Color.Black) then
            if j >= 27 then
                ABB_3Up_Attacks.[i, j] <- ABB_Mask.[j - 27]
        else
            if j < 54 then
                ABB_3Up_Attacks.[i, j] <- ABB_Mask.[j + 27]

for i in 0.. Color_NB - 1 do
    for j in 0.. Square_NB - 1 do
        if i = int(Color.Black) then
            if j >= 18 then
                if FileTable.[j] = File.File1 then
                    ABB_2Up_3Sq_Attacks.[i, j] <- ABB_Mask.[j - 18] ||| ABB_Mask.[j - 18 + 1]
                elif FileTable.[j] = File.File9 then
                    ABB_2Up_3Sq_Attacks.[i, j] <- ABB_Mask.[j - 18 - 1] ||| ABB_Mask.[j - 18]
                else
                    ABB_2Up_3Sq_Attacks.[i, j] <- ABB_Mask.[j - 18 - 1] ||| ABB_Mask.[j - 18] ||| ABB_Mask.[j - 18 + 1]
        else
            if j < 63 then
                if FileTable.[j] = File.File1 then
                    ABB_2Up_3Sq_Attacks.[i, j] <- ABB_Mask.[j + 18] ||| ABB_Mask.[j + 18 + 1]
                elif FileTable.[j] = File.File9 then
                    ABB_2Up_3Sq_Attacks.[i, j] <- ABB_Mask.[j + 18 - 1] ||| ABB_Mask.[j + 18]
                else
                    ABB_2Up_3Sq_Attacks.[i, j] <- ABB_Mask.[j + 18 - 1] ||| ABB_Mask.[j + 18] ||| ABB_Mask.[j + 18 + 1]

for i in 0.. Color_NB - 1 do
    for j in 0.. Square_NB - 1 do
        if i = int(Color.Black) then
            if j <= 71 then
                ABB_Diag_Back_Attacks.[i, j] <- ABB_Mask.[j + 9]
        else
            if j >= 10 then
                ABB_Diag_Back_Attacks.[i, j] <- ABB_Mask.[j - 9]

type BoardTree =
    struct
        val mutable Board: int8[]
        val mutable BB_Piece: System.UInt128[,]
        val mutable BB_Occupied: System.UInt128[]
        val mutable SQ_King: uint8[]
        val mutable Hand: int[]
        val mutable RootColor: Color
        val mutable Hash: uint64[]// 128ビットにするかもしれない。
        val mutable CurrentHash: uint64
        val mutable PrevHash: uint64
        val mutable Ply: uint16
        val mutable EvalArray: int[]
    end

let mutable Rand:uint64[,,] = Array3D.init Color_NB Piece_NB Square_NB (fun i j k -> uint64(0))
filePath <- "hash_table.txt"
lines<-File.ReadAllLines(filePath)
for line in lines do
    let l = line.Split([|','|])
    let c = int l.[0]
    let p = int l.[1]
    let sq = int l.[2]
    let v = uint64 l.[3]
    Rand.[c - 1, p, sq - 1]<- v

type Record = 
    struct
        val mutable str_moves: string[]
        val mutable winner: uint8
        val mutable ply: uint16
    end

type TT = 
    struct
        val mutable value:Dictionary<uint64, int>
        val mutable color:Dictionary<uint64, int>
        val mutable is_check:Dictionary<uint64, bool>
        val mutable move:Dictionary<uint64, uint32>
        val mutable ply:Dictionary<uint64, int16>
    end

type MoveAndScore = 
    struct
        val mutable move: uint32
        val mutable score: int
    end