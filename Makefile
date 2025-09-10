build:
	dune build ./src/main.exe

trace:
	WAYLAND_DEBUG=1 dune exec ./src/main.exe 4

clean:
	dune clean
