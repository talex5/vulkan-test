build:
	dune build ./src/main.exe

trace:
	WAYLAND_DEBUG=1 dune exec ./src/main.exe 4

download-example:
	wget https://vulkan-tutorial.com/resources/viking_room.png
	wget https://vulkan-tutorial.com/resources/viking_room.obj

clean:
	dune clean
