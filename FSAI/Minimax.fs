namespace FSAI




module Minimax =

    let empty = byte 0
    let white = byte 1
    let black = byte 2
    let valid = byte 3
    let tie = byte 4

    let MinMaxAlphaBeta board depth a b tile isMaxPLayer =
        0

    let GetValidMoves board tile =
        0

    let Evaluation board =
        0

    let CountCorners (board : byte[,]) (tile : byte) = 
        let corners = [(board.[0,0]);(board.[0,7]);(board.[7,0]);(board.[7,7])]
        List.filter (fun corner -> corner = tile) corners
        |> List.length
            
    
    let GetScore (board : byte[,]) (tile : byte) =
        Seq.cast<byte> board 
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
            byte 99 //error
    
    let GetWinner (board : byte[,]) = 
        let blackScore = GetScore board black
        let whiteScore = GetScore board white
        if whiteScore = 0 || 
            blackScore = 0 || 
            blackScore+whiteScore = 64 || 
            GetValidMoves board white + GetValidMoves board black = 0 then
            if whiteScore > blackScore then white 
            elif blackScore > whiteScore then black 
            else tie
        else byte 99 //error
               