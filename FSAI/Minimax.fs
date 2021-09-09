namespace FSAI




module Minimax =
    
    let boardSize = int 8
    let empty = byte  0
    let white = byte  1
    let black = byte  2
    let valid = byte  3
    let tie =  byte 4
    //directions
    let dirs = [(-1,1) ;(0,1) ;(1,1)
                (-1,0) ;       (1,0)
                (-1,-1);(0,-1);(1,-1)]


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

    //This function follows a direction on the board and checks if the direction is part of a valid move
    //returns true if that is the case
    let rec validDir (board:byte[,]) (x:int) (y:int) (dir:(int * int)) (tile:byte) =
        if IsOnBoard x y && board.[x,y] = OtherTile tile then
             let xDir, yDir = dir
             validDir board (x+xDir) (y+yDir) dir tile
        elif board.[x,y] = tile then
            true
        else
            false

    //This function loops trought all the directions and returns true if a valid direction is found
    let rec loopDirs (board:byte[,]) (x:int) (y:int) (dirList: (int* int) list) (tile:byte) =
        match dirList with
        |[] -> false
        |head::tail ->
            let dirX,dirY = head
            if validDir board (x+dirX) (y+dirY) head tile then
                true
            else 
                loopDirs board x y tail tile  

    //Checks if a move is valid, firstly by checking if the position is empty and secondly if the position will flip pieces 
    let ValidMove (board:byte[,]) (x:int) (y:int) (tile:byte) =
        if board.[x,y] = empty then
            loopDirs board x y dirs tile
        else
            false
            
    // Get all the valid moves by checking all the boards positions against the ValidMove boolean function
    let GetValidMoves (board :byte[,]) (tile: byte) =
        let output = [
            for X in 0..7 do 
                for Y in 0..7 do if (ValidMove board X Y tile) then yield (X,Y)]
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
            
            
    //Evaluation function
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
    
    //Loops a specific direction until a tile of the players color is found,
    //returns a list of positions of the opposite players tiles that should be turned
    let rec GetFlippedDirPieces (board:byte[,]) (pos:(int*int)) (dir:(int*int)) (tile:byte) =
        let posX,posY = pos
        let dirX,dirY = dir
        if board.[posX,posY] = tile then  
            []
        elif board.[posX,posY] = OtherTile tile then
            [(posX,posY)]@GetFlippedDirPieces board (posX+dirX,posY+dirY) dir tile
        else
            []

    //Loop trought the list if directions and check if they are valid directions, 
    //if they are return a list of tiles that should be flipped
    let rec LoopDirsForFlippedPieces (board:byte[,]) (x:int) (y:int) (dirList: (int* int) list) (tile:byte) =
        match dirList with
        |[] -> []
        |head::tail ->
            let dirX,dirY = head
            if validDir board (x+dirX) (y+dirY) head tile then //if direction is a valid dir, add it to the list
                let flippedPeicesList = GetFlippedDirPieces board (x+dirX,y+dirY) (dirX,dirY) tile
                flippedPeicesList@(LoopDirsForFlippedPieces board x y tail tile)
            else 
                []@LoopDirsForFlippedPieces board x y tail tile //skip adding this direction

    //Top level function to encapsulate the the recursive looping
    let GetFlippedPeices (board:byte[,]) (move:(int*int)) (tile:byte) =
        let moveX, moveY = move
        if board.[moveX,moveY] = empty then
            LoopDirsForFlippedPieces board moveX moveY dirs tile //get all flipped pieces in all directions
        else
            []
      
    let MakeMove (board:byte[,]) (move:(int*int)) (tile:byte) =
        board
        
        
    let MinMaxAlphaBeta board depth a b tile isMaxPLayer =
        0
