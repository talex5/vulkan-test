Example OCaml code that uses GPU acceleration to render 3D scene and display it in a Wayland window,
using Vulkan but without its Wayland-support extension.

This is intended for tracing, to help me learn about graphics.

To run:

```
git clone https://github.com/talex5/vulkan-test.git
cd vulkan-test
nix develop
make download-example
dune exec -- ./src/main.exe 10000 viking_room.obj viking_room.png
```

You should see an animation of a 3D viking room.
The `10000` is the number of frames it will show before quitting.

For more information, see these blog posts:

- [Investigating Linux Graphics](https://roscidus.com/blog/blog/2025/06/24/graphics/)
- [Vulkan Graphics in OCaml vs C](https://roscidus.com/blog/blog/2025/09/20/ocaml-vulkan/)
