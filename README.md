Example code that uses GPU acceleration to render a triangle and display it in a Wayland window, using Vulkan but without its Wayland-support extension.

This is intended for tracing, to help me learn about graphics. It is not good code to copy. For example, I generally don't bother freeing things.

To run:

```
git clone https://github.com/talex5/vulkan-test.git
cd vulkan-test
nix develop
make && ./vulkan-test 200
```

You should see a triangle sliding to the right.
The `200` is the number of frames it will show before quitting.

See the [Investigating Linux Graphics](https://roscidus.com/blog/blog/2025/06/24/graphics/) blog post for more information.
