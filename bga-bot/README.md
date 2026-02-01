# Tigris & Euphrates BGA Bot

This folder contains tools for playing T&E on Board Game Arena using the AI engine.

## Setup

```bash
# Install dependencies
cd /path/to/tigris-and-euphrates
source .venv/bin/activate
pip install selenium webdriver-manager

# Build the Python module (requires Rust)
maturin develop --features python
```

## Tools

### 1. BGA Assistant (Semi-Automated)

Interactive assistant that suggests moves while you play on BGA. You manually input moves from both players to keep the game state in sync.

```bash
python3 bga-bot/bga_assistant.py
```

Commands:
- `tile A3 red` - Place a red tile at A3
- `leader B2 black` - Place the black leader (King) at B2
- `withdraw C4` - Withdraw a leader from C4
- `catastrophe D5` - Place catastrophe at D5
- `pass` - Pass the current action
- `support 3` - Add 3 support tiles in conflict
- `ai` - Execute AI's suggested move
- `moves` - Show all legal moves
- `help` - Show all commands
- `quit` - Exit

### 2. Full Automation (Experimental)

Selenium-based bot that attempts to fully automate play on BGA. **Note: This is experimental and may not work due to BGA's UI structure changing.**

```bash
python bga-bot/bga_bot.py --game-url "https://boardgamearena.com/table?id=12345" --username "your_email" --password "your_password"
```

Options:
- `--depth N` - AI search depth (default: 5)
- `--headless` - Run browser in headless mode

### 3. Interactive Test

Test the Python bindings locally without BGA:

```bash
python bga-bot/test_interactive.py
```

## How It Works

1. The Rust AI engine is compiled to a Python module via PyO3/maturin
2. The Python module exposes:
   - `TnEGame()` - Create a new game
   - `game.get_ai_move(depth)` - Get AI's recommended move
   - `game.get_legal_moves()` - List all legal moves
   - `game.process(move_idx)` - Execute a move
   - `game.get_board_state()` - Get human-readable board
   - `game.is_game_over()` - Check if game ended
   - `game.current_player()` - Get current player (1 or 2)

## Coordinate System

- BGA uses letter-number format: A1 to P11
- Column A is leftmost, column P is rightmost
- Row 1 is top, row 11 is bottom

## Tips

- The AI runs at depth 5 by default, which takes ~1-2 seconds per move
- Use depth 3-4 for faster (but weaker) play
- The AI focuses on building balanced scores (min of 4 colors wins)

## Legal Notice

⚠️ **Using bots on BGA may violate their Terms of Service.** Use at your own risk. This is intended for educational/research purposes.
