default:
	cargo run --release -- play
flame:
	cargo flamegraph --root -- test && open flamegraph.svg
perft:
	cargo run --release -- perft
test:
	cargo test --release

# Web version targets
web:
	./build-web.sh

serve:
	cd web && python3 -m http.server 8080

web-dev: web serve