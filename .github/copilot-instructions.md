# Tigris and Euphrates

A Rust implementation of the board game Tigris & Euphrates with AI opponents (minimax, MCTS) and multiple frontends (native GUI, WebAssembly, Python bindings).

## Build and Test

```bash
# Build and test
cargo build --release
cargo test --release            # Run all tests
cargo test --release test_name  # Run single test

# Fast tests only (excludes slow depth scaling test)
cargo test --release -- --skip test_depth_scaling

# Run game modes (requires --features game)
make play      # Human vs AI
make           # AI vs AI (selfplay)
make perft     # Performance test

# Web version
make web       # Build WASM (requires wasm-pack)
make serve     # Build and serve at localhost:8080

# Python bindings
maturin develop --features python
```

## Architecture

### Core Game Engine (`src/`)
- **game.rs** - Game state, rules, move generation, and validation. Board is 16x11 (W×H). Uses `Bitboard` (backed by `U256`) for efficient tile/position tracking.
- **solver.rs** - Implements `minimax::Game` trait for AI integration. Contains the `Evaluator` for position scoring and Zobrist hashing for transposition tables.
- **history.rs** - Game history recording and replay.

### Frontends
- **visualizer.rs** - Native GUI using macroquad (feature `game`)
- **wasm.rs** - WebAssembly bindings exposing `TnEWeb` for browser play (feature `wasm`)
- **interop.rs** - Python bindings via PyO3 for ML training (feature `python`)

### AI Training (`a0-jax/`)
AlphaZero implementation in JAX for training neural network policies. See `a0-jax/README.md` for training commands.

### Local Minimax Fork (`minimax-local/`)
Forked minimax-rs library with strategies: Negamax with alpha-beta pruning, iterative deepening, transposition tables, and MCTS.

## Key Conventions

### Position System
Positions use `Pos { x, y }` where x is row (0-10) and y is column (0-15). Use the `pos!` macro:
```rust
pos!(5, 10)      // Pos { x: 5, y: 10 }
pos!("1A")       // Parse board notation
```

### Feature Flags
The crate uses feature flags to control compilation:
- `game` - Native GUI with macroquad
- `python` - PyO3 bindings for ML
- `wasm` - WebAssembly support

### Game State Machine
`TnEGame.state` is a stack of `GameState` variants. An empty stack means game over. Actions are validated against `next_state()` before processing via `game.process(action)`.

### Evaluator Development Workflow
When improving the AI evaluator in `solver.rs`:
1. Copy the current `Evaluator::get_eval()` logic into `BaselineEvaluator::eval()` before making changes
2. Make experimental changes to `Evaluator::get_eval()`
3. Run `cargo test --release test_experimental_vs_baseline` to compare performance
4. If experimental wins > baseline wins, keep the change and update the baseline

### Code Style
- Max line width: 100 characters (`.rustfmt.toml`)
- Library crate name: `tne`
