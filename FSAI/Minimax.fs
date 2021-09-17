namespace FSAI




module Minimax =
    open System
    // Constants
    let boardSize = int 8
    let empty = byte  0
    let white = byte  1
    let black = byte  2
    let valid = byte  3
    let tie =  byte 4
    // Directions
    let dirs = [(-1,1) ;(0,1) ;(1,1)
                (-1,0) ;       (1,0)
                (-1,-1);(0,-1);(1,-1)]

    // Count the number of tiles a player has in the corners
    let CountCorners (board : byte[,]) (tile : byte) = 
        let corners = [(board.[0,0]);(board.[0,7]);(board.[7,0]);(board.[7,7])]
        List.filter (fun corner -> corner = tile) corners
        |> List.length
            
    // Count the number of tiles a player has, one tile -> one point
    let GetScore (board : byte[,]) (tile : byte) =
        Seq.cast<byte> board 
        |> Seq.filter (fun cell -> cell = tile)
        |> Seq.length

    // Check that x and y are within the bounds of the board
    let IsOnBoard x y =
        0 <= x && x <=7 && 0 <= y && y <= 7;

    // Return the tile of the other player
    let OtherTile tile =
        if tile = black then
            white
        else if tile = white then
            black
        else
            byte 99 //error

    // This function follows a direction on the board and checks if the direction is part of a valid move
    // returns true if that is the case
    let rec ValidDir (board:byte[,]) (x:int) (y:int) (dir:(int * int)) (tile:byte) =
        if IsOnBoard x y then
            if board.[x,y] = OtherTile tile then
                 let xDir, yDir = dir
                 ValidDir board (x+xDir) (y+yDir) dir tile
            elif board.[x,y] = tile then
                true
            else
                false
        else
            false

    // This function loops through all the directions and returns true if a valid direction is found
    let rec LoopDirs (board:byte[,]) (x:int) (y:int) (dirList: (int* int) list) (tile:byte) =
        match dirList with
        |[] -> false
        |head::tail ->
            let dirX,dirY = head
            if ValidDir board (x+dirX) (y+dirY) head tile then
                true
            else 
                LoopDirs board x y tail tile  

    // Checks if a move is valid, firstly by checking if the position is empty and secondly if the position will flip pieces 
    let ValidMove (board:byte[,]) (x:int) (y:int) (tile:byte) =
        if board.[x,y] = empty then
            LoopDirs board x y dirs tile
        else
            false
            
    // Get all the valid moves by checking all the boards positions against the ValidMove boolean function
    let GetValidMoves (board :byte[,]) (tile: byte) =
        let output = [
            for X in 0..7 do 
                for Y in 0..7 do if (ValidMove board X Y tile) then yield (X,Y)]
        output


    // If a player has 0 tiles on the board, or if all tiles on the board are occupied, or if neither player
    // has any valid moves left -> return the winner, or tie
    let GetWinner (board : byte[,]) = 
        let blackScore = GetScore board black
        let whiteScore = GetScore board white
        if whiteScore = 0 || 
            blackScore = 0 || 
            blackScore+whiteScore = (boardSize * boardSize) || 
            List.length(GetValidMoves board white) + List.length(GetValidMoves board black) = 0 then
            if whiteScore > blackScore then white 
            elif blackScore > whiteScore then black 
            else tie
        else byte 99 //error
            
            
    // Evaluate how good a certain board is, relative to the black player
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
        let evaluation2 = evaluation1 + ((CountCorners board black) - (CountCorners board white)) * 100
        evaluation2
    
    // Loops a specific direction until a tile of the players color is found,
    // returns a list of positions of the opposite players tiles that should be turned
    let rec GetFlippedDirPieces (board:byte[,]) (pos:(int*int)) (dir:(int*int)) (tile:byte) =
        let posX,posY = pos
        let dirX,dirY = dir
        if board.[posX,posY] = tile then  
            []
        elif board.[posX,posY] = OtherTile tile then
            [(posX,posY)]@GetFlippedDirPieces board (posX+dirX,posY+dirY) dir tile
        else
            []

    // Loop through the list of directions and check if they are valid directions. 
    // if they are, return a list of tiles that should be flipped
    let rec LoopDirsForFlippedPieces (board:byte[,]) (x:int) (y:int) (dirList: (int* int) list) (tile:byte) =
        match dirList with
        |[] -> []
        |head::tail ->
            let dirX,dirY = head
            // if direction is a valid dir, add it to the list
            if ValidDir board (x+dirX) (y+dirY) head tile then 
                let flippedPeicesList = GetFlippedDirPieces board (x+dirX,y+dirY) (dirX,dirY) tile
                flippedPeicesList@(LoopDirsForFlippedPieces board x y tail tile)
            else 
                // skip adding this direction
                []@LoopDirsForFlippedPieces board x y tail tile

    // Top level function to encapsulate the the recursive looping
    let GetFlippedPieces (board:byte[,]) (move:(int*int)) (tile:byte) =
        let moveX, moveY = move
        if board.[moveX,moveY] = empty then
            // Get all flipped pieces in all directions
            LoopDirsForFlippedPieces board moveX moveY dirs tile 
        else
            []
    
    // Change the colors of the positions to own tile color, as well as
    // the clicked square
    let MakeMove (board:byte[,]) (move:(int*int)) (tile:byte) =
        let flippedPiecesList = GetFlippedPieces board move tile
        let newBoard = Array2D.copy board
        for flippedPiece in flippedPiecesList do
            newBoard.[fst flippedPiece, snd flippedPiece] <- tile
        if not flippedPiecesList.IsEmpty then
            newBoard.[fst move, snd move] <- tile
            newBoard
        else
            newBoard





    // The Minimax algorithm with alpha beta pruning. 
    let rec MinMaxAlphaBeta board depth alpha beta tile isMaxPLayer =
        
        // A nested function for handling the alpha beta pruning
        let rec LoopMoves (board:byte[,]) (validMoves:(int*int)list) (tile:byte) (isMaxPlayer:bool) (bestScore:int)  (alpha:int) (beta:int) =
            match validMoves with
            | [] -> bestScore
            | head::tail -> 
                
                let newBoard = MakeMove board head tile
                let nodeScore = MinMaxAlphaBeta newBoard (depth-1) alpha beta (OtherTile tile) (not isMaxPlayer)
                if isMaxPlayer then
                    let newBestScore = max bestScore nodeScore
                    let newAlpha = max bestScore alpha
                    if beta <= newAlpha then
                        newBestScore
                    else
                       (LoopMoves board tail tile isMaxPlayer newBestScore newAlpha beta)

                else
                    let newBestScore = min bestScore nodeScore
                    let newBeta = min bestScore beta
     
                    if newBeta <= alpha then
                        newBestScore
                    else
                       (LoopMoves board tail tile isMaxPlayer newBestScore alpha newBeta)
        // Base case, check if depth is 0 or if there has been a winner
        if depth = 0 || (GetWinner board  <> empty) then
            (Evaluation board)
        else
        let bestScore = match isMaxPLayer with
                        | true -> System.Int32.MinValue
                        | false -> System.Int32.MaxValue
        let validMoves = GetValidMoves board tile
        // If a player has no valid moves, continue MinMaxAlphaBeta-function 
        // with the next player
        if validMoves.IsEmpty then
            (MinMaxAlphaBeta board depth alpha beta (OtherTile tile) (not isMaxPLayer))
        // Else loop through the validMoves
        else
            (LoopMoves board validMoves tile isMaxPLayer bestScore alpha beta)


        
