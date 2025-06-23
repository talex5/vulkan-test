#include <wayland-client.h>
#include <drm/drm_fourcc.h>
#include "linux-dmabuf-v1.h"

struct dmabuf_feedback_format_table {
   unsigned int size;
   struct {
      uint32_t format;
      uint32_t padding; /* unused */
      uint64_t modifier;
   } *data;
};

struct wayland_state {
	struct wl_compositor *compositor;
	struct xdg_wm_base *xdg_wm_base;
	struct zwp_linux_dmabuf_v1 *linux_dmabuf_v1;
	struct wl_display *display;
	struct wl_surface *surface;
	struct dmabuf_feedback_format_table format_table;
	dev_t main_device;
	uint64_t drm_format;
	uint64_t drm_modifier;
};

typedef struct wayland_state *wayland_state;

extern wayland_state wayland_init(void);
