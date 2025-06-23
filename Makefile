all: vulkan-test

HEADERS=xdg-shell-client-protocol.h linux-dmabuf-v1.h
SOURCES=main.c xdg-shell-protocol.c linux-dmabuf-v1.c helpers.c wayland.c render.c

vulkan-test: shaders/frag.spv shaders/vert.spv ${SOURCES} ${HEADERS}
	${CC} -g -O2 `pkgconf --cflags --libs wayland-client vulkan libdrm` -I${LINUX_HEADERS} -lm \
		${SOURCES} -o vulkan-test

trace: vulkan-test
	WAYLAND_DEBUG=1 ./vulkan-test 4 2>&1 | sed 's/^\[[ 0-9.]*\] {[^}]*} //'

WAYLAND_PROTOCOLS_DIR = $(shell pkgconf wayland-protocols --variable=pkgdatadir)
XDG_SHELL_PROTOCOL = $(WAYLAND_PROTOCOLS_DIR)/stable/xdg-shell/xdg-shell.xml
LINUX_DMABUF_PROTOCOL = $(WAYLAND_PROTOCOLS_DIR)/stable/linux-dmabuf/linux-dmabuf-v1.xml

WAYLAND_SCANNER = $(shell pkgconf --variable=wayland_scanner wayland-scanner)

xdg-shell-client-protocol.h:
	$(WAYLAND_SCANNER) client-header $(XDG_SHELL_PROTOCOL) xdg-shell-client-protocol.h

xdg-shell-protocol.c:
	$(WAYLAND_SCANNER) private-code $(XDG_SHELL_PROTOCOL) xdg-shell-protocol.c

linux-dmabuf-v1.h:
	$(WAYLAND_SCANNER) client-header $(LINUX_DMABUF_PROTOCOL) $@

linux-dmabuf-v1.c:
	$(WAYLAND_SCANNER) private-code $(LINUX_DMABUF_PROTOCOL) $@

shaders/vert.spv: shaders/shader.vert
	glslc $< -o $@

shaders/frag.spv: shaders/shader.frag
	glslc $< -o $@
