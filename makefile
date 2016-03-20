all: simple-hex

clean:
	find . -name "*.js_o" -type f -delete
	find . -name "*.js_hi" -type f -delete

simple-hex:
	ghcjs -o js_out examples/simple-hex.hs -isrc
