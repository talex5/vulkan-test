Example OCaml code that uses GPU acceleration to render 3D scene and display it in a Wayland window,
using Vulkan but without its Wayland-support extension.

If run without `$WAYLAND_DISPLAY` set then it will use KMS to render directly to the screen.

This is intended to help me learn about graphics.

## Running

Get the code with:

```
git clone https://github.com/talex5/vulkan-test.git
cd vulkan-test
```

### Build and run using Nix

```
nix develop
dune exec -- ./src/main.exe
```

### Using opam

You'll need to ensure that `slangc` is available in `$PATH`
(from [shader-slang releases][]; use v2025.22.1).

```
wget https://github.com/shader-slang/slang/releases/download/v2025.22.1/slang-2025.22.1-linux-x86_64.tar.gz
mkdir slang
tar x -C slang -f slang-2025.22.1-linux-x86_64.tar.gz
export PATH=$PWD/slang/bin:$PATH
opam install --deps-only .
dune exec -- ./src/main.exe
```

## Playing

The game starts with the ship flying across a randomly generated landscape.
Move the mouse to angle the craft and use button-1 for thrust.
The aim of the game is to find the landing pad and land on it.

## More information

For more information, see these blog posts:

- [Investigating Linux Graphics](https://roscidus.com/blog/blog/2025/06/24/graphics/)
- [Vulkan Graphics in OCaml vs C](https://roscidus.com/blog/blog/2025/09/20/ocaml-vulkan/)
- [Linux mode setting, from the comfort of OCaml](https://roscidus.com/blog/blog/2025/11/16/libdrm-ocaml/)

[shader-slang releases]: https://github.com/shader-slang/slang/releases/tag/v2025.22.1
