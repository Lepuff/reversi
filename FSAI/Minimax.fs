namespace FSAI




module Minimax =
    let boardSize = int 8
    let empty = byte  0
    let white = byte  1
    let black = byte  2
    let valid = byte  3
    let tie =  byte 4
    let dirs = [(-1,1) ;(0,1) ;(1,1)
                (-1,0) ;       (1,0)
                (-1,-1);(0,-1);(1,-1)]

    let MinMaxAlphaBeta board depth a b tile isMaxPLayer =
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


    let rec validDir (board:byte[,]) (x:int) (y:int) (dir:(int * int)) (tile:byte) =
        if IsOnBoard x y && board.[x,y] = OtherTile tile then
             let xDir, yDir = dir
             validDir board (x+xDir) (y+yDir) dir tile
        elif board.[x,y] = tile then
            true
        else
            false

    let rec loopDirs (board:byte[,]) (x:int) (y:int) (dirList: (int* int) list) (tile:byte) =
        match dirList with
        |[] -> false
        |head::tail ->
            if validDir board x y head tile then
                true
            else 
                loopDirs board x y tail tile  

     
    let ValidMove (board:byte[,]) (x:int) (y:int) (tile:byte) =
        if board.[x,y] = empty then
            loopDirs board x y dirs tile
        else
            false
            
       
    let GetValidMoves (board :byte[,]) (tile: byte) =
        let output = [
            for X in 0..7 do 
                for Y in 0..7 do if (ValidMove board X Y tile) = true then  yield (X,Y)]
        output



    let GetWinner (board : byte[,]) = 
        let blackScore = GetScore board black
        let whiteScore = GetScore board white
        if whiteScore = 0 || 
            blackScore = 0 || 
            blackScore+whiteScore = 64 || 
            List.length(GetValidMoves board white) + List.length(GetValidMoves board black) = 0 then
            if whiteScore > blackScore then white 
            elif blackScore > whiteScore then black 
            else tie
        else byte 99 //error
              
    let Evaluation (board:byte[,]) =
        let blackScore = GetScore board black
        let whiteScore = GetScore board white
        let blackMobility = (GetValidMoves board black).Length
        let whiteMobility = (GetValidMoves board white).Length

        if blackScore = 0 then
            -200000
        elif whiteScore = 0 then
            200000
        else

        if blackScore + whiteScore = (boardSize * boardSize) || blackMobility + whiteMobility = 0 then 
            if (blackScore < whiteScore) then
                -100000 - whiteScore + blackScore
            elif (blackScore > whiteScore) then
                100000 + blackScore - whiteScore
            else
                0
            
        else
        if blackScore + whiteScore > 55 then
            (blackScore - whiteScore)
        else
        let evaluation  = blackScore  -  whiteScore    
        let evaluation1 = evaluation  + (blackMobility - whiteMobility) * 10
        let evaluation2 = evaluation1 + ((CountCorners board black) - (CountCorners board white)) *100
        evaluation2

    let GetFlippedPeices (board:byte[,]) (move:(int*int)) (tile:byte) =
        [(1,1)]

      
      
    let MakeMove (board:byte[,]) (move:(int*int)) (tile:byte) =
        board
        
        

