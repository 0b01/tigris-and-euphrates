default:
	cargo run --release -- play
flame:
	cargo flamegraph --root -- test && open flamegraph.svg
perft:
	cargo run --release -- perft
test:
	cargo test --release