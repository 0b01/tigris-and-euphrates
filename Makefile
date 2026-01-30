default:
	cargo run --release --features game -- play 
flame:
	cargo flamegraph --root -- test && open flamegraph.svg
perft:
	cargo run --release -- perft
test:
	cargo test --release

# Web version targets
.PHONY: web serve web-dev
web:
	./build-web.sh
serve: web
	cd web && python3 -m http.server 8080