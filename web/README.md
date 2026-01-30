# Tigris & Euphrates Web Version

A browser-based implementation of the classic board game Tigris & Euphrates, playable against a strong AI opponent.

## Features

- **Human vs AI**: Play against a challenging minimax-based AI with adjustable difficulty
- **AI vs AI**: Watch two AI players compete against each other
- **Full Game Implementation**: All core rules including conflicts (revolts and wars), monuments, treasures, and catastrophes
- **Responsive UI**: Modern interface with visual feedback for valid moves

## Building

### Prerequisites

1. **Rust** - Install from [rustup.rs](https://rustup.rs/)
2. **wasm-pack** - Will be automatically installed by the build script, or install manually:
   ```bash
   cargo install wasm-pack
   ```

### Build Steps

From the project root directory:

```bash
# Build the WASM module
./build-web.sh

# Or using make
make web
```

This compiles the Rust code to WebAssembly and outputs it to `web/pkg/`.

## Running

After building, start a local web server:

```bash
# Using Python
cd web && python3 -m http.server 8080

# Or using make
make serve

# Or using npx
npx serve web
```

Then open http://localhost:8080 in your browser.

## How to Play

### Controls

1. **Select a tile/leader** - Click on a tile or leader in your hand
2. **Place it** - Click on a valid board position (highlighted cells)
3. **Pass** - Click the "Pass" button to skip your action
4. **Support during conflicts** - Use the dialog to choose how many tiles to commit

### AI Settings

- **Easy (Depth 1)** - Quick but not very strategic
- **Medium (Depth 2)** - Decent challenge
- **Hard (Depth 3)** - Strong play
- **Expert (Depth 4)** - Very challenging (may take a moment to think)

### AI vs AI Mode

1. Click "AI vs AI" mode
2. Use "Step" for move-by-move play
3. Use "Auto Play" for continuous play
4. Adjust speed with the slider

## Technical Details

- **Frontend**: Vanilla JavaScript with ES6 modules
- **Game Engine**: Rust compiled to WebAssembly
- **AI**: Negamax algorithm with alpha-beta pruning and move ordering

## Game Rules Quick Reference

- Place tiles to score points (matching leader color)
- Leaders must be adjacent to temples (red tiles)
- Uniting two kingdoms with leaders of the same color triggers war
- Placing a leader in a kingdom with an opposing leader triggers revolt
- Collect treasures when connecting kingdoms with 2+ treasures (green leader)
- Build monuments on 2x2 same-color tile squares
- Game ends when 2 treasures remain or the tile bag is empty
- Final score = minimum of your 4 color scores (treasures are wildcards)
