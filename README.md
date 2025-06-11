# Haskell Othello Game 🎮

A feature-rich implementation of the classic Othello (Reversi) board game written in Haskell.

## Features

- **Multiple Game Modes:**
  - Human vs Human
  - Human vs AI
  - AI vs AI (demonstration mode)

- **Interactive Gameplay:**
  - Beautiful ASCII board display
  - Real-time score tracking
  - Input validation and error handling
  - Multiple input formats supported

- **Smart AI Player:**
  - Random move selection from legal moves
  - Follows all Othello rules correctly

- **User-Friendly Interface:**
  - Comprehensive help system
  - Command system (help, random, quit)
  - Clear visual feedback

## How to Run

### Prerequisites
- GHC (Glasgow Haskell Compiler) installed
- Basic familiarity with terminal/command line

### Running the Game
```bash
# Compile the game
ghc -o othello Othello.hs

# Run the game
./othello
```

Or compile and run in one step:
```bash
runhaskell Othello.hs
```

## Game Rules

**Objective:** Have the most pieces when no more moves are possible.

**How to Play:**
1. Place your piece on an empty square
2. Your piece must 'sandwich' opponent pieces between your new piece and an existing piece
3. All sandwiched pieces flip to your color
4. If you can't make a legal move, your turn is skipped
5. Game ends when neither player can move

**Strategy Tips:**
- 🏰 Corner squares are extremely valuable
- 🛡️ Edge squares provide good defensive positions  
- ⚡ Try to limit your opponent's available moves
- 🎯 Control the center early in the game

## Input Formats

The game accepts moves in several formats:
- Space-separated: `3 4`
- Combined digits: `34`
- With punctuation: `3,4` or `3-4`

## Commands

- `help` - Display rules and strategy tips
- `random` - Make a random legal move
- `quit` - Exit the game

## Code Structure

- **Board Representation:** Efficient Array-based implementation
- **Move Validation:** 8-directional scanning for legal moves
- **AI Implementation:** Random selection from legal moves
- **Game Loop:** Turn-based gameplay with mode switching
- **User Interface:** Comprehensive input handling and display

## Technical Details

- Uses `Data.Array` for O(1) position access
- Functional programming approach with immutable data structures
- Comprehensive error handling and input validation
- Modular design with clear separation of concerns

## Example Gameplay

```
🎮 === ENHANCED OTHELLO GAME === 🎮

Select game mode:
1. Human vs Human
2. Human vs AI (You are O, AI is X)  
3. AI vs AI (Watch AI play)
4. Quit
Enter choice (1-4): 2

     1   2   3   4   5   6   7   8
   ╔═══╤═══╤═══╤═══╤═══╤═══╤═══╤═══╗
1  ║   │   │   │   │   │   │   │   ║ 1
   ╟───┼───┼───┼───┼───┼───┼───┼───╢
2  ║   │   │   │   │   │   │   │   ║ 2
   ╟───┼───┼───┼───┼───┼───┼───┼───╢
3  ║   │   │   │   │   │   │   │   ║ 3
   ╟───┼───┼───┼───┼───┼───┼───┼───╢
4  ║   │   │   │ X │ O │   │   │   ║ 4
   ╟───┼───┼───┼───┼───┼───┼───┼───╢
5  ║   │   │   │ O │ X │   │   │   ║ 5
   ╟───┼───┼───┼───┼───┼───┼───┼───╢
6  ║   │   │   │   │   │   │   │   ║ 6
   ╟───┼───┼───┼───┼───┼───┼───┼───╢
7  ║   │   │   │   │   │   │   │   ║ 7
   ╟───┼───┼───┼───┼───┼───┼───┼───╢
8  ║   │   │   │   │   │   │   │   ║ 8
   ╚═══╧═══╧═══╧═══╧═══╧═══╧═══╧═══╝
     1   2   3   4   5   6   7   8

📊 Scores: O=2 X=2

👤 Player O's turn
Legal moves: [(3,4),(4,3),(5,6),(6,5)]
Enter move (row col), 'help', 'random', or 'quit':
```

## Contributing

Feel free to fork this project and submit pull requests for improvements such as:
- Enhanced AI algorithms (minimax, alpha-beta pruning)
- Better board visualization
- Save/load game functionality
- Tournament mode
- Network multiplayer

## License

This project is open source. Feel free to use and modify as needed.

---
*Built with ❤️ in Haskell*
