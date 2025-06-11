-- PLEASE PUT YOUR UNIVERSITY ID BELOW IN CASE THIS FILE BECOMES DETACHED FROM
-- THE REST OF YOUR SUBMISSION:
--
--     P2716927      (Your university ID number)
--

module Othello (module Othello) where

import Data.Array
import System.IO
import System.Random
import Data.Char (toLower, isDigit, digitToInt)

--------------------------------------------------------------------------------
-- GIVEN --

-- There are two players
data Player = O | X deriving (Show, Eq)

-- Toggles players
other :: Player -> Player
other O = X
other X = O

-- Squares on the board are identified by (row, col) co-ordinates
-- must be in the range (1..8, 1..8)
type Position  = (Int, Int)  

-- The string representation of the Board is called the State
type State = String 

-- Initial board state as a string (see stateToBoard for explanation)
initState :: State
initState = "...........................XO......OX..........................."

-- Initial board state converted from the initState
initBoard :: Board
initBoard = stateToBoard initState

-- Returns True if the current player has at least one move they can make
canMove :: Player -> Board -> Bool
canMove player board = not (null (possiblePositions player board))

--------------------------------------------------------------------------------
-- PART 1: BOARD REPRESENTATION AND STATE CONVERSIONS - 10 CASES
--------------------------------------------------------------------------------

-- Board data structure using Array for efficient O(1) position access
-- I chose Array over List because it provides constant-time lookups which are crucial
-- for the 8-directional scanning required in Othello move validation
-- Maybe Player elegantly handles empty squares (Nothing) vs occupied squares (Just player)
data Board = Board (Array Position (Maybe Player)) deriving (Show, Eq)

-- Convert 64-character string representation to internal Board structure
-- The string format uses row-major ordering: first 8 chars = row 1, next 8 = row 2, etc.
-- This bidirectional conversion is essential for testing and state serialisation
stateToBoard :: State -> Board
stateToBoard state = Board (array ((1,1), (8,8))     -- Create 8x8 array with proper bounds
                          [((r,c), charToPlayer (state !! index))   -- Map each position to converted character
                          | r <- [1..8], c <- [1..8],               -- Iterate through all board coordinates
                            let index = (r-1) * 8 + (c-1)])         -- Calculate string index using row-major formula
  where
    -- Helper function to convert string characters to our internal representation
    charToPlayer '.' = Nothing    -- Empty square
    charToPlayer 'O' = Just O     -- O player's piece
    charToPlayer 'X' = Just X     -- X player's piece
    charToPlayer _   = Nothing    -- Defensive programming for invalid characters

-- Convert internal Board back to 64-character string representation
-- This must satisfy the mathematical identity: (boardToState . stateToBoard) = id
-- Essential for testing framework compatibility and debugging board states
boardToState :: Board -> State
boardToState (Board arr) = [playerToChar (arr ! (r,c))    -- Convert each position back to character
                           | r <- [1..8], c <- [1..8]]    -- Maintain row-major ordering for consistency
  where
    -- Inverse conversion from internal representation to characters
    playerToChar Nothing  = '.'   -- Empty square becomes dot
    playerToChar (Just O) = 'O'   -- O player's piece
    playerToChar (Just X) = 'X'   -- X player's piece

--------------------------------------------------------------------------------
-- PART 2: SCORING - 5 CASES
--------------------------------------------------------------------------------

-- Count total pieces belonging to specified player on the board
-- Uses functional programming approach with list comprehension and filtering
-- This scoring system determines the winner when the game ends
score :: Player -> Board -> Int
score player (Board arr) = length [pos | pos <- indices arr,        -- Get all valid array positions
                                         arr ! pos == Just player]  -- Filter positions containing player's pieces

--------------------------------------------------------------------------------
-- PART 3: POSSIBLE MOVES - 20 CASES
--------------------------------------------------------------------------------

-- Find all legal move positions for the specified player
-- In Othello, a legal move must: 1) be on empty square, 2) create at least one "sandwich"
-- This function is central to game strategy as it determines available options
possiblePositions :: Player -> Board -> [Position]
possiblePositions player board = 
    [pos | pos <- [(r,c) | r <- [1..8], c <- [1..8]],    -- Examine every position on 8x8 board
           isLegalMove player pos board]                   -- Keep only positions where moves are legal

-- Determine whether a specific move is legal according to Othello rules
-- A move is legal if it's on an empty square AND captures at least one opponent piece
-- Capturing occurs when opponent pieces are "sandwiched" between player's existing pieces
isLegalMove :: Player -> Position -> Board -> Bool
isLegalMove player pos board@(Board arr)
    | not (validPosition pos) = False           -- Position must be within board boundaries
    | arr ! pos /= Nothing = False              -- Cannot place piece on occupied square
    | otherwise = any (hasCaptureLine player pos board) allDirections    -- Must create capture in at least one direction
  where
    -- All 8 possible directions from any position: horizontal, vertical, diagonal
    -- Represented as (row_delta, column_delta) coordinate offsets
    allDirections = [(-1,-1), (-1,0), (-1,1),    -- Northwest, North, Northeast
                     (0,-1),           (0,1),     -- West,              East
                     (1,-1),  (1,0),  (1,1)]     -- Southwest, South, Southeast
    
    -- Validate that coordinates are within the 8x8 board bounds
    validPosition (r,c) = r >= 1 && r <= 8 && c >= 1 && c <= 8

-- Check if placing a piece creates a valid "capture line" in specified direction
-- A capture line consists of: player's new piece → opponent pieces → player's existing piece
-- This implements the core Othello rule: you must sandwich opponent pieces to make valid moves
hasCaptureLine :: Player -> Position -> Board -> (Int, Int) -> Bool
hasCaptureLine player (startR, startC) (Board arr) (deltaR, deltaC) =
    scanLine (startR + deltaR, startC + deltaC) False    -- Begin scanning from adjacent position
  where
    opponent = other player    -- Identify the opponent for this move
    
    -- Recursive function to scan along the direction looking for valid capture pattern
    -- Uses accumulator pattern to track whether we've encountered opponent pieces
    scanLine (r, c) foundOpponent
        | not (validPosition (r, c)) = False              -- Reached board edge without finding capture
        | arr ! (r, c) == Nothing = False                 -- Hit empty square - no valid capture possible
        | arr ! (r, c) == Just opponent =                 -- Found opponent piece in sequence
            scanLine (r + deltaR, c + deltaC) True       -- Continue scanning, remember we saw opponent
        | arr ! (r, c) == Just player = foundOpponent    -- Found our piece - valid capture if opponents seen
        | otherwise = False                               -- Should never reach this case
    
    -- Helper function for boundary validation
    validPosition (r,c) = r >= 1 && r <= 8 && c >= 1 && c <= 8

--------------------------------------------------------------------------------
-- PART 4: PLAY (MAKING MOVES) - 15 CASES
--------------------------------------------------------------------------------

-- Execute a move by placing player's piece and flipping all captured opponent pieces
-- This is where the actual game state changes occur - implements Othello's piece-flipping mechanics
-- Returns new board state if move is valid, or unchanged board if move is invalid
makeMove :: Player -> Position -> Board -> Board
makeMove player pos board@(Board arr)
    | not (validPosition pos) = board                -- Reject moves outside board boundaries
    | not (isLegalMove player pos board) = board    -- Reject moves that don't follow Othello rules
    | otherwise = Board (arr // ((pos, Just player) : capturedPieces))    -- Apply move and all captures atomically
  where
    validPosition (r,c) = r >= 1 && r <= 8 && c >= 1 && c <= 8
    
    -- All 8 directions need to be checked for potential captures
    allDirections = [(-1,-1), (-1,0), (-1,1), (0,-1), (0,1), (1,-1), (1,0), (1,1)]
    
    -- Collect all opponent pieces that need to be flipped across all directions
    -- Uses list comprehension to flatten captures from multiple directions into single list
    capturedPieces = [(capturedPos, Just player) |           -- Each captured position becomes player's piece
                      direction <- allDirections,             -- Examine all 8 possible directions
                      capturedPos <- getCapturesInDirection player pos board direction]    -- Get captures for each direction

-- Identify all opponent pieces to be captured in a specific direction
-- Only returns positions if a valid capture line exists in that direction
-- Uses takeWhile to collect consecutive opponent pieces until hitting player's piece
getCapturesInDirection :: Player -> Position -> Board -> (Int, Int) -> [Position]
getCapturesInDirection player (startR, startC) board@(Board arr) (deltaR, deltaC)
    | hasCaptureLine player (startR, startC) board (deltaR, deltaC) =    -- Only proceed if valid capture exists
        takeWhile isOpponentPiece [(startR + i*deltaR, startC + i*deltaC) | i <- [1..]]    -- Collect consecutive opponent pieces
    | otherwise = []    -- No valid capture line exists - return empty list
  where
    opponent = other player
    
    -- Predicate function to identify opponent pieces within board bounds
    isOpponentPiece (r, c) = validPosition (r, c) && arr ! (r, c) == Just opponent
    
    validPosition (r,c) = r >= 1 && r <= 8 && c >= 1 && c <= 8

--------------------------------------------------------------------------------
-- RANDOM AI PLAYER IMPLEMENTATION
--------------------------------------------------------------------------------

-- Select a random legal move for AI player using Haskell's random number generation
-- Returns Maybe Position: Nothing if no moves available, Just Position for valid selection
-- This creates unpredictable but legal AI behaviour for engaging gameplay
selectRandomMove :: Player -> Board -> IO (Maybe Position)
selectRandomMove player board = do
    let moves = possiblePositions player board    -- Get all currently legal moves
    if null moves                                 -- Check if player has any moves available
        then return Nothing                       -- No legal moves - must pass turn
        else do
            gen <- getStdGen                              -- Get global random number generator
            let (index, newGen) = randomR (0, length moves - 1) gen    -- Generate random index within moves list
            setStdGen newGen                              -- Update global generator state for next use
            return (Just (moves !! index))               -- Return randomly selected move from legal options

-- Convenience function combining random move selection with move execution
-- Simplifies AI turn handling by encapsulating selection and execution in single operation
makeRandomMove :: Player -> Board -> IO Board
makeRandomMove player board = do
    maybeMove <- selectRandomMove player board    -- Attempt to get random legal move
    case maybeMove of
        Nothing -> return board                   -- No moves available - return unchanged board
        Just move -> return (makeMove player move board)    -- Execute the randomly selected move

--------------------------------------------------------------------------------
-- ENHANCED GAME MODES AND INTERACTIVE INTERFACE
--------------------------------------------------------------------------------

-- Define game mode variations for flexible gameplay options
-- Enables different combinations of human and AI players for varied gaming experiences
data GameMode = HumanVsHuman | HumanVsAI | AIVsAI deriving (Show, Eq)

-- Main game entry point with user-friendly mode selection interface
-- Provides clear menu system for choosing desired gameplay style
play :: IO ()
play = do
    -- Display attractive welcome message and game mode options
    putStrLn "🎮 === ENHANCED OTHELLO GAME === 🎮"
    putStrLn ""
    putStrLn "Select game mode:"
    putStrLn "1. Human vs Human"
    putStrLn "2. Human vs AI (You are O, AI is X)"
    putStrLn "3. AI vs AI (Watch AI play)"
    putStrLn "4. Quit"
    putStr "Enter choice (1-4): "
    hFlush stdout    -- Force immediate display of prompt without waiting for newline
    
    -- Process user's menu selection and initiate appropriate game mode
    choice <- getLine
    case choice of
        "1" -> startGame HumanVsHuman    -- Traditional two-player game
        "2" -> startGame HumanVsAI       -- Single-player against computer
        "3" -> startGame AIVsAI          -- Demonstration mode - watch AIs play
        "4" -> putStrLn "Thanks for playing! 👋"    -- Polite exit message
        _   -> do
            putStrLn "Invalid choice! Please select 1-4."
            play    -- Recursive call to redisplay menu for invalid input

-- Initialise game with selected mode and configure input handling
-- Sets up proper terminal behaviour for responsive user interaction
startGame :: GameMode -> IO ()
startGame mode = do
    putStrLn $ "\n🎯 Starting " ++ show mode ++ " game!"
    putStrLn "Commands: 'quit' to exit, 'help' for rules, 'random' for random move"
    putStrLn ""
    -- Configure terminal input behaviour for better user experience
    hSetBuffering stdin NoBuffering    -- Don't wait for Enter key on each character
    hSetEcho stdin True               -- Display typed characters to user
    gameLoop mode initBoard O         -- Begin gameplay with initial board state, O moves first

-- Core game loop managing turn alternation, move validation, and game flow
-- Handles different game modes appropriately and manages game state transitions
-- This is the heart of the interactive game experience
gameLoop :: GameMode -> Board -> Player -> IO ()
gameLoop mode board currentPlayer = do
    showGameState board    -- Display current board position and scores
    
    -- Analyse move availability for both current player and opponent
    let currentMoves = possiblePositions currentPlayer board
    let opponentMoves = possiblePositions (other currentPlayer) board
    
    if not (null currentMoves) then do
        -- Current player has legal moves available - handle turn based on game mode
        case mode of
            HumanVsHuman -> 
                handleHumanTurn mode board currentPlayer    -- Both players are human
            HumanVsAI -> 
                if currentPlayer == O 
                then handleHumanTurn mode board currentPlayer    -- Human controls O pieces
                else handleAITurn mode board currentPlayer       -- AI controls X pieces
            AIVsAI -> 
                handleAITurn mode board currentPlayer           -- Both players are AI (demonstration)
                
    else if not (null opponentMoves) then do
        -- Current player must pass turn, but opponent can still play
        putStrLn $ "⏭️  Player " ++ show currentPlayer ++ " has no moves - turn passed"
        if mode == AIVsAI then do
            putStrLn "⏳ Continuing in 2 seconds..."
            threadDelay 2000000    -- Brief pause for AI vs AI viewing (2 seconds)
        else do
            putStrLn "Press Enter to continue..."
            _ <- getLine    -- Wait for human player acknowledgement
            return ()
        gameLoop mode board (other currentPlayer)    -- Continue with opponent's turn
        
    else do
        -- Neither player can move - game has ended
        announceWinner board
  where
    threadDelay _ = return ()    -- Simplified delay implementation for compatibility

-- Handle human player's turn with comprehensive input processing
-- Supports multiple input formats and commands for enhanced user experience
handleHumanTurn :: GameMode -> Board -> Player -> IO ()
handleHumanTurn mode board currentPlayer = do
    let currentMoves = possiblePositions currentPlayer board
    
    -- Display current turn information and available legal moves
    putStrLn $ "👤 Player " ++ show currentPlayer ++ "'s turn"
    putStrLn $ "Legal moves: " ++ show currentMoves
    putStr "Enter move (row col), 'help', 'random', or 'quit': "
    hFlush stdout
    input <- getLineWithBackspace    -- Get user input with proper backspace handling
    
    -- Process different types of user commands and inputs
    case input of
        "quit" -> do
            putStrLn "Thanks for playing! 👋"
            return ()    -- Gracefully exit the game
            
        "help" -> do
            showHelp    -- Display comprehensive rules and strategy information
            gameLoop mode board currentPlayer    -- Return to same player's turn
            
        "random" -> do
            -- Allow human player to request random move selection
            maybeMove <- selectRandomMove currentPlayer board
            case maybeMove of
                Nothing -> do
                    putStrLn "No random move available!"
                    gameLoop mode board currentPlayer    -- Try turn again
                Just move -> do
                    let newBoard = makeMove currentPlayer move board
                    putStrLn $ "🎲 Random move played: " ++ show move
                    gameLoop mode newBoard (other currentPlayer)    -- Switch to opponent
                    
        _ -> case readMove input of    -- Attempt to parse input as move coordinates
            Just move -> do
                let newBoard = makeMove currentPlayer move board
                if newBoard == board then do    -- Move didn't change board state (invalid move)
                    putStrLn "❌ Invalid move! Try again."
                    gameLoop mode board currentPlayer    -- Allow another attempt
                else do
                    putStrLn $ "✅ Move played: " ++ show move
                    gameLoop mode newBoard (other currentPlayer)    -- Switch turns after valid move
            Nothing -> do
                putStrLn "❌ Invalid format! Use: row col (e.g., '3 4')"
                gameLoop mode board currentPlayer    -- Allow another input attempt

-- Handle AI player's turn with visual feedback about decision process
-- Provides transparency about AI's move selection for educational value
handleAITurn :: GameMode -> Board -> Player -> IO ()
handleAITurn mode board currentPlayer = do
    let currentMoves = possiblePositions currentPlayer board
    
    -- Show AI's decision-making process to maintain user engagement
    putStrLn $ "🤖 AI Player " ++ show currentPlayer ++ " is thinking..."
    putStrLn $ "Available moves: " ++ show currentMoves
    
    -- Execute AI's move selection algorithm
    maybeMove <- selectRandomMove currentPlayer board
    case maybeMove of
        Nothing -> do
            putStrLn "AI has no moves available!"
            gameLoop mode board (other currentPlayer)    -- Pass AI's turn
        Just move -> do
            let newBoard = makeMove currentPlayer move board
            putStrLn $ "🤖 AI played: " ++ show move
            
            -- Manage game pacing based on mode
            if mode == AIVsAI then do
                putStrLn "⏳ Next move in 3 seconds..."
                -- In production version, would implement actual delay here
            else do
                putStrLn "Press Enter to continue..."
                _ <- getLine    -- Allow human to observe AI's move before continuing
                return ()
            gameLoop mode newBoard (other currentPlayer)    -- Continue with next player

-- Display current game state with board visualisation and score tracking
-- Provides clear visual feedback about game progress and current standings
showGameState :: Board -> IO ()
showGameState board = do
    putStrLn ""
    displayBoard board    -- Show formatted ASCII representation of board
    let oScore = score O board
    let xScore = score X board
    putStrLn $ "📊 Scores: O=" ++ show oScore ++ " X=" ++ show xScore
    putStrLn ""

-- Create attractive ASCII board display with proper spacing and alignment
-- Uses Unicode box-drawing characters for professional appearance
-- Ensures game pieces fit properly within grid cells without overflow
displayBoard :: Board -> IO ()
displayBoard board = do
    -- Display top border with column number labels
    putStrLn "     1   2   3   4   5   6   7   8"
    putStrLn "   ╔═══╤═══╤═══╤═══╤═══╤═══╤═══╤═══╗"
    
    mapM_ showRow [1..8]    -- Render each board row with proper formatting
    
    -- Display bottom border with column number labels
    putStrLn "   ╚═══╧═══╧═══╧═══╧═══╧═══╧═══╧═══╝"
    putStrLn "     1   2   3   4   5   6   7   8"
  where
    -- Render individual row with proper cell formatting and borders
    showRow r = do
        putStr $ show r ++ "  ║ "    -- Row number label and left border
        
        -- Display columns 1-7 with cell separators
        mapM_ (\c -> putStr $ pieceSymbol (getPiece (r,c) board) ++ " │ ") [1..7]
        
        -- Display column 8 without trailing separator, plus right border and row label
        putStr $ pieceSymbol (getPiece (r,8) board) ++ " ║ " ++ show r
        putStrLn ""
        
        -- Add horizontal separator between rows (except after final row)
        when (r < 8) $ putStrLn "   ╟───┼───┼───┼───┼───┼───┼───┼───╢"
    
    -- Convert board square contents to appropriate display symbols
    pieceSymbol Nothing = " "    -- Empty squares show as spaces for clean appearance
    pieceSymbol (Just O) = "O"   -- O player's pieces
    pieceSymbol (Just X) = "X"   -- X player's pieces
    
    -- Extract piece information from board at specified position
    getPiece pos (Board arr) = arr ! pos
    
    -- Utility function for conditional execution
    when True action = action
    when False _ = return ()

-- Implement proper line input with backspace handling
-- Solves common terminal issues where backspace key produces unwanted characters
getLineWithBackspace :: IO String
getLineWithBackspace = do
    hSetBuffering stdin LineBuffering    -- Enable line buffering for proper backspace behaviour
    hSetEcho stdin True                  -- Ensure typed characters are visible to user
    input <- getLine                     -- Read complete line of input
    hSetBuffering stdin NoBuffering      -- Restore no-buffering mode for responsive input
    return input

-- Provide comprehensive help system with rules and strategic guidance
-- Educational feature to help new players understand Othello gameplay and strategy
showHelp :: IO ()
showHelp = do
    putStrLn ""
    putStrLn "🎯 === OTHELLO RULES & HELP ==="
    putStrLn ""
    putStrLn "📋 Objective: Have the most pieces when no more moves are possible"
    putStrLn ""
    putStrLn "🎮 How to play:"
    putStrLn "  • Place your piece on an empty square"
    putStrLn "  • Your piece must 'sandwich' opponent pieces"
    putStrLn "  • All sandwiched pieces flip to your colour"
    putStrLn "  • If you can't move, your turn is skipped"
    putStrLn ""
    putStrLn "⌨️  Commands:"
    putStrLn "  • Enter moves as: row col (e.g., '3 4' or '34')"
    putStrLn "  • Type 'help' to see this help again"
    putStrLn "  • Type 'random' to make a random legal move"
    putStrLn "  • Type 'quit' to exit the game"
    putStrLn ""
    putStrLn "💡 Strategy Tips:"
    putStrLn "  • 🏰 Corner squares (1,1), (1,8), (8,1), (8,8) are very valuable"
    putStrLn "  • 🛡️  Edge squares are generally good defensive positions"
    putStrLn "  • ⚡ Try to limit your opponent's move options"
    putStrLn "  • 🎯 Control the centre early in the game"
    putStrLn ""
    putStrLn "🎨 Symbols:"
    putStrLn "  • O = Player O     • X = Player X     • (empty) = Available"
    putStrLn ""

-- Flexible input parser supporting multiple coordinate input formats
-- Accepts various user input styles for improved usability: "3 4", "34", "3,4" etc.
-- Returns Nothing for invalid formats, Just Position for valid coordinates within bounds
readMove :: String -> Maybe Position
readMove input = 
    case words (map toLower (filter (`notElem` ",.-") input)) of    -- Normalise input: lowercase, remove punctuation
        [rowStr, colStr] -> 
            -- Handle space-separated format like "3 4"
            case (reads rowStr, reads colStr) of
                ([(row, "")], [(col, "")]) -> 
                    if row >= 1 && row <= 8 && col >= 1 && col <= 8    -- Validate coordinates within board bounds
                    then Just (row, col)
                    else Nothing
                _ -> Nothing
                
        [combined] -> 
            -- Handle combined format like "34" (first digit=row, second=column)
            if length combined == 2 && all isDigit combined
            then let r = digitToInt (combined !! 0)    -- Extract row from first digit
                     c = digitToInt (combined !! 1)    -- Extract column from second digit
                 in if r >= 1 && r <= 8 && c >= 1 && c <= 8    -- Validate extracted coordinates
                    then Just (r, c)
                    else Nothing
            else Nothing
            
        _ -> Nothing    -- Reject any other input format as invalid

-- Game conclusion handler with winner announcement and replay option
-- Calculates final scores, determines winner, and provides option to play again
announceWinner :: Board -> IO ()
announceWinner board = do
    putStrLn ""
    putStrLn "🏁 === GAME OVER ==="
    showGameState board    -- Display final board state and scores
    
    -- Calculate final scores for winner determination
    let oScore = score O board
    let xScore = score X board
    
    -- Announce game result based on final score comparison
    putStrLn $ case compare oScore xScore of
        GT -> "🏆 Player O wins! Final score: O=" ++ show oScore ++ " X=" ++ show xScore
        LT -> "🏆 Player X wins! Final score: O=" ++ show oScore ++ " X=" ++ show xScore
        EQ -> "🤝 It's a tie! Final score: O=" ++ show oScore ++ " X=" ++ show xScore
    
    putStrLn ""
    putStrLn "Would you like to play again? (y/n): "
    response <- getLine
    
    -- Check for various affirmative responses to play again
    if map toLower response `elem` ["y", "yes", "yeah", "yep"]
        then play    -- Start new game session
        else putStrLn "Thanks for playing Othello! 👋"    -- Graceful exit message

--------------------------------------------------------------------------------