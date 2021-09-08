namespace FSAI




module Minimax =

    let empty = 0
    let white = 1
    let black = 2
    let valid = 3
    let tie = 4

    let MinMaxAlphaBeta board depth a b tile isMaxPLayer =
        0


    let GetWinner board = 
        0

    let Evaluation board =
        0

    let GetScore (board : int[,]) (tile : int) =
        Seq.cast<int> board 
        |> Seq.filter (fun cell -> cell = tile)
        |> Seq.length

    let IsOnBoard x y =
        0 <= x && x <=7 && 0 <= 7 && y <= 7;

    let OtherTile tile =
        if tile = black then
            white
        else if tile = white then
            black
        else
            99 //error
               