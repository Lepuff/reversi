namespace FSAI




module Minimax =

    let MinMaxAlphaBeta board depth a b tile isMaxPLayer =
        0


    
module Evaluation =

    let GetWinner board = 
        0

    let Evaluation board =
        0

    let GetScore (board : int[,]) (tile : int) =
        Seq.cast<int> board 
        |> Seq.filter (fun cell -> cell = tile)
        |> Seq.length


module Moves = 
    
    let IsOnBoard x y =
        0 <= x && x <=7 && 0 <= 7 && y <= 7;



    let GetValidMoves board tile =
        0


        