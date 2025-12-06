module Sort

let Merge(A:MoveAndScore[,] ref, B:MoveAndScore[,] ref, left:int, mid:int, right:int, ply:int) =
    let mutable i = left
    let mutable j = mid
    let mutable k = 0
    while i < mid && j < right do
        if A.contents.[ply, i].score <= A.contents.[ply, j].score then
            B.contents.[ply, k] <- A.contents.[ply, i]
            k <- k + 1
            i <- i + 1
        else
            B.contents.[ply, k] <- A.contents.[ply, j]
            k <- k + 1
            j <- j + 1
    if i = mid then
        // i側のAをBに移動し尽くしたので、j側も順番にBに入れていく
        while j < right do
            B.contents.[ply, k] <- A.contents.[ply, j]
            k <- k + 1
            j <- j + 1
    else
        // j側のAをBに移動し尽くしたので、i側も順番にBに入れていく
        while i < mid do
            B.contents.[ply, k] <- A.contents.[ply, i]
            k <- k + 1
            i <- i + 1
    for l = 0 to k - 1 do
        A.contents.[ply, left + l] <- B.contents.[ply, l]
    0

let rec MergeSort(A:MoveAndScore[,] ref, B:MoveAndScore[,] ref, left:int, right:int, ply:int) =
    let mutable flag = false
    if left = right || left = right - 1 then
        flag <- true
    if flag = false then
        let mid = (left + right) / 2
        MergeSort(A, B, left, mid, ply)
        MergeSort(A, B, mid, right, ply)
        Merge(A, B, left, mid, right, ply)|> ignore