#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <wayland-client.h>
#include <assert.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/sysmacros.h>
#include <sys/mman.h>

#include "xdg-shell-client-protocol.h"
#include "linux-dmabuf-v1.h"
#include "wayland.h"
#include "helpers.h"

// xdg_wm_base

static void xdg_wm_base_ping(void *data, struct xdg_wm_base *xdg_wm_base, uint32_t serial)
{
	xdg_wm_base_pong(xdg_wm_base, serial);
}

static const struct xdg_wm_base_listener xdg_wm_base_listener = {
	.ping = xdg_wm_base_ping,
};

// Registry

static void registry_handle_global(void *data, struct wl_registry *wl_registry,
		uint32_t name, const char *interface, uint32_t version)
{
	struct wayland_state *state = data;
	if (strcmp(interface, wl_compositor_interface.name) == 0) {
		state->compositor = wl_registry_bind(wl_registry, name, &wl_compositor_interface, 4);
	} else if (strcmp(interface, xdg_wm_base_interface.name) == 0) {
		state->xdg_wm_base = wl_registry_bind(wl_registry, name, &xdg_wm_base_interface, 1);
		xdg_wm_base_add_listener(state->xdg_wm_base, &xdg_wm_base_listener, state);
	} else if (strcmp(interface, zwp_linux_dmabuf_v1_interface.name) == 0) {
		state->linux_dmabuf_v1 = wl_registry_bind(wl_registry, name, &zwp_linux_dmabuf_v1_interface, 4);
		zwp_linux_dmabuf_v1_add_listener(state->linux_dmabuf_v1, NULL, state);
	}
}

static void registry_handle_global_remove(void *data, struct wl_registry *registry,
		uint32_t name) {
}

static const struct wl_registry_listener registry_listener = {
	.global = registry_handle_global,
	.global_remove = registry_handle_global_remove,
};

// Surfaces

static void xdg_surface_configure(void *data, struct xdg_surface *xdg_surface, uint32_t serial) {
	struct client_state *state = data;
	xdg_surface_ack_configure(xdg_surface, serial);
}

static const struct xdg_surface_listener xdg_surface_listener = {
	.configure = xdg_surface_configure,
};

// dmabuf feedback

static void feedback_format_table(void *data, struct zwp_linux_dmabuf_feedback_v1 *zwp_linux_dmabuf_feedback_v1, int32_t fd, uint32_t size) {
   struct wayland_state *state = data;

   if (state->format_table.data != MAP_FAILED) {
	   munmap(state->format_table.data, state->format_table.size);
	   state->format_table.data = MAP_FAILED;
   }

   state->format_table.size = size;
   state->format_table.data = mmap(NULL, size, PROT_READ, MAP_PRIVATE, fd, 0);

   close(fd);
}

static void feedback_main_device(void *data, struct zwp_linux_dmabuf_feedback_v1 *dmabuf_feedback, struct wl_array *device) {
   struct wayland_state *state = data;

   assert(device->size == sizeof(dev_t));
   memcpy(&state->main_device, device->data, device->size);
}

static void feedback_tranche_target_device(void *data, struct zwp_linux_dmabuf_feedback_v1 *dmabuf_feedback, struct wl_array *device) { }

static void feedback_tranche_flags(void *data, struct zwp_linux_dmabuf_feedback_v1 *dmabuf_feedback, uint32_t flags) { }

static void feedback_tranche_formats(void *data, struct zwp_linux_dmabuf_feedback_v1 *dmabuf_feedback, struct wl_array *indices) {
	struct wayland_state *state = data;
	uint16_t *index;

	if (state->format_table.data == MAP_FAILED) return;

	wl_array_for_each(index, indices) {
		uint32_t format = state->format_table.data[*index].format;
		uint64_t modifier = state->format_table.data[*index].modifier;

		//LOG("feedback_tranche_formats: %4s : %lx\n", (char *) &format, (long) modifier);

		// My card doesn't support modifiers, so use the DRM_FORMAT_RESERVED fallback
		if (state->drm_format == DRM_FORMAT_INVALID && format == DRM_FORMAT_XRGB8888 && modifier == DRM_FORMAT_RESERVED) {
			LOG("Wayland compositor supports DRM_FORMAT_XRGB8888 with modifier 0x%lx\n", (long) modifier);
			state->drm_format = format;
			state->drm_modifier = modifier;
		}
	}
}

static void feedback_tranche_done(void *data, struct zwp_linux_dmabuf_feedback_v1 *dmabuf_feedback) { }

static void feedback_done(void *data, struct zwp_linux_dmabuf_feedback_v1 *dmabuf_feedback) { }

static const struct zwp_linux_dmabuf_feedback_v1_listener feedback_listener = {
	.format_table = feedback_format_table,
	.main_device = feedback_main_device,
	.tranche_target_device = feedback_tranche_target_device,
	.tranche_flags = feedback_tranche_flags,
	.tranche_formats = feedback_tranche_formats,
	.tranche_done = feedback_tranche_done,
	.done = feedback_done,
};

// Initialisation

wayland_state wayland_init(void) {
	wayland_state state = malloc(sizeof(struct wayland_state));
	state->format_table.data = MAP_FAILED;
	state->drm_format = DRM_FORMAT_INVALID;

	// Connect to the Wayland compositor
	state->display = wl_display_connect(NULL);
	if (!state->display) {
		DIE("Failed to connect to Wayland display.\n");
	}

	// Get the list of extensions and bind the ones we want
	struct wl_registry *registry = wl_display_get_registry(state->display);
	wl_registry_add_listener(registry, &registry_listener, state);
	wl_display_roundtrip(state->display);
	assert(state->compositor != NULL);
	assert(state->xdg_wm_base != NULL);
	assert(state->linux_dmabuf_v1 != NULL);

	// Create the window
	state->surface = wl_compositor_create_surface(state->compositor);
	struct xdg_surface *xdg_surface = xdg_wm_base_get_xdg_surface(state->xdg_wm_base, state->surface);
	xdg_surface_add_listener(xdg_surface, &xdg_surface_listener, state);
	struct xdg_toplevel *xdg_toplevel = xdg_surface_get_toplevel(xdg_surface);
	xdg_toplevel_set_title(xdg_toplevel, "Example client");
	wl_surface_commit(state->surface);

	// Ask for information about which GPU to use for this surface
	struct zwp_linux_dmabuf_feedback_v1 *feedback =
		zwp_linux_dmabuf_v1_get_surface_feedback(state->linux_dmabuf_v1, state->surface);
	zwp_linux_dmabuf_feedback_v1_add_listener(feedback, &feedback_listener, state);
	wl_display_roundtrip(state->display);
	assert (state->drm_format != DRM_FORMAT_INVALID);
	// Simplify the trace by stopping future updates
	zwp_linux_dmabuf_feedback_v1_destroy(feedback);

	return state;
}
