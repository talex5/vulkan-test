/*
 * A simple Vulkan/Wayland test app.
 * This is intended for tracing, to help me learn about graphics.
 * It is not good code to copy. For example, I generally don't bother freeing things.
 */

#include <stdio.h>
#include <stdbool.h>
#include <assert.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdint.h>
#include <time.h>
#include <fcntl.h>
#include <string.h>
#include <wayland-client.h>
#include <vulkan/vulkan.h>
#include <vulkan/vulkan_wayland.h>
#include <linux/dma-buf.h>
#include <xf86drm.h>
#include "helpers.h"
#include "wayland.h"
#include "render.h"
#include "linux-dmabuf-v1.h"
#include <unistd.h>
#include <sys/sysmacros.h>

//#define VALIDATION

const char* validationLayers[] = {
#ifdef VALIDATION
	"VK_LAYER_KHRONOS_validation"
#endif
};

struct state {
	VkDevice device;
	VkQueue graphicsQueue;
	struct frame_state *frames;		// Pool of framebuffers
	struct wayland_state *wayland_state;
	int frame;
	int frameLimit;
	VkFence inFlightFence;			// Signalled when GPU finishes rendering pipeline

	// Temporaries used inside draw_frame (only here to avoid recreating them all the time):
	VkSemaphore imageAvailableSemaphore;	// Signalled when the compositer has finished showing the old image
	VkCommandBuffer commandBuffer;		// Used to submit drawing commands

	// These functions don't link statically and need looking up at runtime:
	PFN_vkImportSemaphoreFdKHR vkImportSemaphoreFdKHR;
	PFN_vkGetSemaphoreFdKHR vkGetSemaphoreFdKHR;
};

void draw_frame(struct state *t);

// Called when the Wayland compositor wants us to send the next frame
static void wl_surface_frame_done(void *data, struct wl_callback *cb, uint32_t time) {
	struct state *t = data;

	wl_callback_destroy(cb);

	t->frame++;
	// For simplicity, we don't have a quit feature, so just exit after a bit
	if (t->frame > t->frameLimit) {
		LOG("Frame limit reached; exiting\n");
		exit(0);
	}

	draw_frame(t);
}

static const struct wl_callback_listener wl_surface_frame_listener = {
	.done = wl_surface_frame_done,
};

// Get active jobs on dma_buf_fd (i.e. the compositor's job) and set semaphore to be signalled when they're done.
static void import_semaphore(struct state *t, int dma_buf_fd, VkSemaphore semaphore) {
	struct dma_buf_export_sync_file export = {
		.flags = DMA_BUF_SYNC_RW,
		.fd = -1,
	};
	int ret = drmIoctl(dma_buf_fd, DMA_BUF_IOCTL_EXPORT_SYNC_FILE, &export);
	if (ret != 0) {
		perror("DMA_BUF_IOCTL_EXPORT_SYNC_FILE");
		exit(1);
	}
	VkImportSemaphoreFdInfoKHR importInfo = {
		.sType = VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_FD_INFO_KHR,
		.semaphore = semaphore,
		.flags = VK_SEMAPHORE_IMPORT_TEMPORARY_BIT,
		.handleType = VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT,
		.fd = export.fd, // Takes ownership of the FD
	};
	if (t->vkImportSemaphoreFdKHR(t->device, &importInfo) != VK_SUCCESS) {
		DIE("Importing image semaphore failed!\n");
	}
}

// Add semaphore's current fence to dma_buf_fd, so the compositor can wait on it.
static void export_semaphore(struct state *t, int dma_buf_fd, VkSemaphore semaphore) {
	const VkSemaphoreGetFdInfoKHR get_fd_info = {
		.sType = VK_STRUCTURE_TYPE_SEMAPHORE_GET_FD_INFO_KHR,
		.semaphore = semaphore,
		.handleType = VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT,
	};
	int sync_file_fd = -1;
	if (t->vkGetSemaphoreFdKHR(t->device, &get_fd_info, &sync_file_fd) != VK_SUCCESS) {
		DIE("Failed to snapshot fences!\n");
	}

	struct dma_buf_import_sync_file import = {
		.flags = DMA_BUF_SYNC_RW,
		.fd = sync_file_fd,
	};
	int ret = drmIoctl(dma_buf_fd, DMA_BUF_IOCTL_IMPORT_SYNC_FILE, &import);
	close(sync_file_fd);
}


void draw_frame(struct state *t) {
	struct wl_surface *surface = t->wayland_state->surface;

	// Ask compositor to tell us when it wants the frame after this one
	struct wl_callback *cb = wl_surface_frame(surface);
	wl_callback_add_listener(cb, &wl_surface_frame_listener, t);

	// If we're still rendering the last frame, wait for that to finish.
	// Needed because e.g. imageAvailableSemaphore isn't per-framebuffer.
	LOG("Wait for inFlightFence\n");
	vkWaitForFences(t->device, 1, &t->inFlightFence, VK_TRUE, UINT64_MAX);
	vkResetFences(t->device, 1, &t->inFlightFence);
	// At this point, imageAvailableSemaphore is no longer in use,
	// because the pipeline waited on it and reset it before finishing,
	// which trigged inFlightFence.

	uint32_t imageIndex = t->frame % N_IMAGES;
	LOG("Rendering frame %d with framebuffer %d\n", t->frame, imageIndex);
	struct frame_state *frame_state = &t->frames[imageIndex];

	// Get the semaphore that the compositor's render job will signal when
	// it's done reading the image:
	LOG("Import imageAvailableSemaphore\n");
	import_semaphore(t, frame_state->dma_buf_fd, t->imageAvailableSemaphore);

	// Put commands in commandBuffer
	record_render_commands(frame_state, t->frame, t->commandBuffer);

	// Submit commandBuffer to GPU
	LOG("Submit to graphicsQueue\n");
	VkSemaphore imageAvailableSemaphores[] = {t->imageAvailableSemaphore};
	VkPipelineStageFlags waitStages[] = {VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT};
	VkSemaphore renderFinishedSemaphores[] = {frame_state->renderFinishedSemaphore};
	VkSubmitInfo submitInfo = {
		.sType = VK_STRUCTURE_TYPE_SUBMIT_INFO,
		// Wait for imageAvailableSemaphore before writing the pixel data
		.waitSemaphoreCount = 1,
		.pWaitSemaphores = imageAvailableSemaphores,
		.pWaitDstStageMask = waitStages,
		// Then render the image
		.commandBufferCount = 1,
		.pCommandBuffers = &t->commandBuffer,
		// Signal renderFinishedSemaphore when finished
		.signalSemaphoreCount = 1,
		.pSignalSemaphores = renderFinishedSemaphores,
	};
	if (vkQueueSubmit(t->graphicsQueue, 1, &submitInfo, t->inFlightFence) != VK_SUCCESS) {
		DIE("Failed to submit draw command buffer!\n");
	}

	// Attach renderFinishedSemaphore to the dmabuf so that the compositor doesn't
	// start displaying the image until the GPU has finished rendering it.
	LOG("Export renderFinishedSemaphore\n");
	export_semaphore(t, frame_state->dma_buf_fd, frame_state->renderFinishedSemaphore);

	// Note: Mesa's WSI also calls set_memory_ownership when acquiring and presenting images.
	// I'm not sure what that's for (it doesn't do anything on my GPU) so I skipped it.

	// Tell the compositor to show the new buffer
	// (the compositor will wait for renderFinishedSemaphore, but we don't)
	wl_surface_attach(surface, frame_state->buffer, 0, 0);
	wl_surface_damage(surface, 0, 0, INT32_MAX, INT32_MAX);
	wl_surface_commit(surface);
}

// Find the graphics card that the Wayland compositor suggested we use
static void find_wayland_device(VkInstance instance, wayland_state wayland_state, VkPhysicalDevice *physicalDevice) {
	dev_t wayland_dev = wayland_state->main_device;
	LOG("Wayland compositor main device is %d,%d\n", major(wayland_dev), minor(wayland_dev));

	uint32_t deviceCount;
	vkEnumeratePhysicalDevices(instance, &deviceCount, NULL);
	VkPhysicalDevice *physicalDevices = calloc(deviceCount, sizeof(VkPhysicalDevice));
	vkEnumeratePhysicalDevices(instance, &deviceCount, physicalDevices);
	LOG("Vulkan found %d physical devices\n", deviceCount);

	int found = -1;
	char *match = "";
	for (int i = 0; i < deviceCount; i++) {
		VkPhysicalDeviceDrmPropertiesEXT ext_drm = {
			.sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DRM_PROPERTIES_EXT,
		};
		VkPhysicalDeviceProperties2 props2 = {
			.sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2,
			.pNext = &ext_drm,
		};
		vkGetPhysicalDeviceProperties2(physicalDevices[i], &props2);
		LOG("%d: %s\n", i, props2.properties.deviceName);
		if (ext_drm.renderMajor == major(wayland_dev) && ext_drm.renderMinor == minor(wayland_dev)) {
			match = "rendering";
			found = i;
		}
		// The compositor might give us the primary instead, so check for that too
		if (ext_drm.primaryMajor == major(wayland_dev) && ext_drm.primaryMinor == minor(wayland_dev)) {
			match = "primary";
			found = i;
		}
	}

	if (found == -1) {
		DIE("Wayland GPU device not found!\n");
	}

	LOG("Using device %d (matches Wayland %s node)\n", found, match);
	*physicalDevice = physicalDevices[found];
}

int main(int argc, char **argv) {
	// The cache spawns two threads and makes traces more confusing, so disable it.
	putenv("MESA_SHADER_CACHE_DISABLE=1");

	//show_extensions();

	const char* instanceExtensions[] = {
		VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME, // Get Unix device ID to compare with Wayland
		VK_KHR_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME,	// Share images over Wayland
		VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_EXTENSION_NAME,  // Use Linux sync files
	};

	VkInstance instance;
	VkApplicationInfo appInfo = {
		.sType = VK_STRUCTURE_TYPE_APPLICATION_INFO,
		.pApplicationName = "Hello Triangle",
		.applicationVersion = VK_MAKE_VERSION(1, 0, 0),
		.pEngineName = "No Engine",
		.engineVersion = VK_MAKE_VERSION(1, 0, 0),
		.apiVersion = VK_API_VERSION_1_2,
	};
	VkInstanceCreateInfo createInfo = {
		.sType = VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO,
		.pApplicationInfo = &appInfo,
		.enabledExtensionCount = sizeof(instanceExtensions) / sizeof(char *),
		.ppEnabledExtensionNames = instanceExtensions,
		.enabledLayerCount = sizeof(validationLayers) / sizeof(char *),
		.ppEnabledLayerNames = validationLayers,
	};
	LOG("Create instance with %d layers\n", createInfo.enabledLayerCount);
	if (vkCreateInstance(&createInfo, NULL, &instance) != VK_SUCCESS) {
		DIE("Failed to create instance!\n");
	}

	// Initialise Wayland display and create window
	wayland_state wayland_state = wayland_init();

	// Find the physical device that the Wayland compositor suggested
	VkPhysicalDevice physicalDevice;
	find_wayland_device(instance, wayland_state, &physicalDevice);

	// Create logical device with one graphics queue
	float queuePriority = 1.0f;
	uint32_t graphicsFamily = find_graphics_family(physicalDevice);
	VkDeviceQueueCreateInfo graphicsInfo = {
		.sType = VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO,
		.queueFamilyIndex = graphicsFamily,
		.queueCount = 1,
		.pQueuePriorities = &queuePriority,
	};
	VkDeviceQueueCreateInfo queueCreateInfos[] = { graphicsInfo };
	VkPhysicalDeviceFeatures deviceFeatures = {};
	const char *deviceExtensions[] = {
		// Export image FD so we can send it to the compositor
		VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME,
		VK_KHR_EXTERNAL_MEMORY_FD_EXTENSION_NAME,
		VK_EXT_EXTERNAL_MEMORY_DMA_BUF_EXTENSION_NAME,
		// Export fences to synchronise with the compositor
		VK_KHR_EXTERNAL_SEMAPHORE_EXTENSION_NAME,
		VK_KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME,
	};
	VkDeviceCreateInfo createDevInfo = {
		.sType = VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO,
		.pQueueCreateInfos = queueCreateInfos,
		.queueCreateInfoCount = 1,
		.pEnabledFeatures = &deviceFeatures,
		.enabledExtensionCount = sizeof(deviceExtensions) / sizeof(*deviceExtensions),
		.ppEnabledExtensionNames = deviceExtensions,
	};
	VkDevice device;
	LOG("Create logical device\n");
	if (vkCreateDevice(physicalDevice, &createDevInfo, NULL, &device) != VK_SUCCESS) {
		DIE("Failed to create logical device!\n");
	}

	VkQueue graphicsQueue;
	vkGetDeviceQueue(device, graphicsFamily, 0, &graphicsQueue);

	// Create command buffer
	VkCommandPool commandPool;
	VkCommandPoolCreateInfo poolInfo = {
		.sType = VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO,
		.flags = VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT,
		.queueFamilyIndex = graphicsFamily,
	};
	LOG("Create command pool\n");
	if (vkCreateCommandPool(device, &poolInfo, NULL, &commandPool) != VK_SUCCESS) {
		DIE("Failed to create command pool!\n");
	}
	VkCommandBufferAllocateInfo allocInfo = {
		.sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO,
		.commandPool = commandPool,
		.level = VK_COMMAND_BUFFER_LEVEL_PRIMARY,
		.commandBufferCount = 1,
	};
	VkCommandBuffer commandBuffer;
	if (vkAllocateCommandBuffers(device, &allocInfo, &commandBuffer) != VK_SUCCESS) {
		DIE("Failed to allocate command buffers\n");
	}

	// Properties for creating framebuffers
	assert (wayland_state->drm_format == DRM_FORMAT_XRGB8888);
	VkFormat format = VK_FORMAT_B8G8R8A8_SRGB;
	VkExtent2D extent = { .width = 640, .height = 480 };
	VkFormat view_formats[] = { format };
	VkImageFormatListCreateInfo ext2 = {
		.sType = VK_STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO,
		.viewFormatCount = 1,
		.pViewFormats = view_formats,
	};
	VkExternalMemoryImageCreateInfo ext = {
		.sType = VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO,
		// This should probably be VK_EXTERNAL_MEMORY_HANDLE_TYPE_DMA_BUF_BIT_EXT,
		// but then the validation layer complains (VUID-VkImageCreateInfo-pNext-00990).
		// I think because my card is too old for vkGetPhysicalDeviceImageFormatProperties2,
		// which always returns VK_ERROR_FORMAT_NOT_SUPPORTED.
		.handleTypes = VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT,
		.pNext = &ext2,
	};
	VkImageCreateInfo imgInfo = {
		.sType = VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO,
		.pNext = &ext,
		.flags = VK_IMAGE_CREATE_ALIAS_BIT,		// Is this needed? Mesa does this.
		.imageType = VK_IMAGE_TYPE_2D,
		.mipLevels = 1,
		.arrayLayers = 1,
		.format = format,
		.extent = {
			.width = extent.width,
			.height = extent.height,
			.depth = 1,
		},
		.samples = VK_SAMPLE_COUNT_1_BIT,
		// Validation layer says this must be LINEAR or DRM_FORMAT_MODIFIER_EXT,
		// and it doesn't like DRM_FORMAT_MODIFIER_EXT
		// (requires VK_EXT_image_drm_format_modifier, which isn't available for me):
		.tiling = VK_IMAGE_TILING_LINEAR,
		.usage = VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT,
		.sharingMode = VK_SHARING_MODE_EXCLUSIVE,
		.initialLayout = VK_IMAGE_LAYOUT_UNDEFINED,
	};

	// Create pipeline and framebuffers
	struct frame_state *frames = calloc(N_IMAGES, sizeof(struct frame_state));
	VkRenderPass renderPass = render_create(physicalDevice, device, format, frames);

	VkPhysicalDeviceMemoryProperties memProperties;
	vkGetPhysicalDeviceMemoryProperties(physicalDevice, &memProperties);

	PFN_vkGetMemoryFdKHR vkGetMemoryFdKHR = VK_NULL_HANDLE;
	vkGetMemoryFdKHR = (PFN_vkGetMemoryFdKHR) vkGetInstanceProcAddr(instance, "vkGetMemoryFdKHR");

	for (int i = 0; i < N_IMAGES; i++) {
		LOG("Create framebuffer %d\n", i);
		struct frame_state *frame = &frames[i];
		frame->extent = extent;
		createExportSemaphore(device, &frame->renderFinishedSemaphore);

		// Create the image structure
		VkImage image;
		LOG("vkCreateImage\n");
		if (vkCreateImage(device, &imgInfo, NULL, &image) != VK_SUCCESS) {
			DIE("Failed to create image!\n");
		}

		// Allocate some device memory for the image
		VkMemoryRequirements reqs;
		vkGetImageMemoryRequirements(device, image, &reqs);

		const VkExportMemoryAllocateInfo memory_export_info = {
			.sType = VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO,
			.pNext = NULL,
			.handleTypes = VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT,
		};
		const VkMemoryDedicatedAllocateInfo memory_dedicated_info = {
			.sType = VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO,
			.pNext = &memory_export_info,
			.image = image,
			.buffer = VK_NULL_HANDLE,
		};
		const VkMemoryAllocateInfo memory_info = {
			.sType = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO,
			.pNext = &memory_dedicated_info,
			.allocationSize = reqs.size,
			.memoryTypeIndex = findMemoryType(&memProperties, reqs.memoryTypeBits, VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
		};
		VkDeviceMemory memory;
		LOG("vkAllocateMemory\n");
		if (vkAllocateMemory(device, &memory_info, NULL, &memory) != VK_SUCCESS) {
			DIE("Failed to allocate device memory\n");
		}

		// Attach the memory to the image
		LOG("vkBindImageMemory\n");
		if (vkBindImageMemory(device, image, memory, 0) != VK_SUCCESS) {
			DIE("Failed to bind memory\n");
		}

		// Get Linux dmabuf FD for the memory
		LOG("vkGetMemoryFdKHR\n");
		VkMemoryGetFdInfoKHR getFdInfo = {
			.sType = VK_STRUCTURE_TYPE_MEMORY_GET_FD_INFO_KHR,
			.memory = memory,
			.handleType = VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT,
		};
		if (vkGetMemoryFdKHR(device, &getFdInfo, &frame->dma_buf_fd) != VK_SUCCESS) {
			DIE("Failed to export image FD!\n");
		}

		// Get the rowPitch and offset
		const VkImageSubresource imageSubresource = {
			.aspectMask = VK_IMAGE_ASPECT_COLOR_BIT,
			.mipLevel = 0,
			.arrayLayer = 0,
		};
		VkSubresourceLayout imageLayout;
		vkGetImageSubresourceLayout(device, image, &imageSubresource, &imageLayout);

		// Register the dmabuf with the Wayland compositor
		struct zwp_linux_buffer_params_v1 *params = zwp_linux_dmabuf_v1_create_params(wayland_state->linux_dmabuf_v1);
		zwp_linux_buffer_params_v1_add(params,
				frame->dma_buf_fd,
				0,	// plane_idx
				imageLayout.offset,
				imageLayout.rowPitch,
				wayland_state->drm_modifier >> 32,
				wayland_state->drm_modifier & 0xffffffff);
		frame->buffer = zwp_linux_buffer_params_v1_create_immed(params,
					extent.width,
					extent.height,
					wayland_state->drm_format,
					0);
		zwp_linux_buffer_params_v1_destroy(params);

		// Wrap the image in a framebuffer for rendering
		VkImageViewCreateInfo createInfo = {
			.sType = VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO,
			.image = image,
			.viewType = VK_IMAGE_VIEW_TYPE_2D,
			.format = format,
			.components.r = VK_COMPONENT_SWIZZLE_IDENTITY,
			.components.g = VK_COMPONENT_SWIZZLE_IDENTITY,
			.components.b = VK_COMPONENT_SWIZZLE_IDENTITY,
			.components.a = VK_COMPONENT_SWIZZLE_IDENTITY,
			.subresourceRange.aspectMask = VK_IMAGE_ASPECT_COLOR_BIT,
			.subresourceRange.baseMipLevel = 0,
			.subresourceRange.levelCount = 1,
			.subresourceRange.baseArrayLayer = 0,
			.subresourceRange.layerCount = 1,
		};
		VkImageView view;
		if (vkCreateImageView(device, &createInfo, NULL, &view) != VK_SUCCESS) {
			DIE("Failed to create image views!\n");
		}

		VkImageView attachments[] = { view };
		VkFramebufferCreateInfo framebufferInfo = {
			.sType = VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO,
			.renderPass = renderPass,
			.attachmentCount = 1,
			.pAttachments = attachments,
			.width = extent.width,
			.height = extent.height,
			.layers = 1,
		};

		if (vkCreateFramebuffer(device, &framebufferInfo, NULL, &frame->framebuffer) != VK_SUCCESS) {
			DIE("Failed to create framebuffer!\n");
		}
	}

	// Set up the shared state for the draw_frame callback
	struct state state = {
		.device = device,
		.graphicsQueue = graphicsQueue,
		.commandBuffer = commandBuffer,
		.frames = frames,
		.wayland_state = wayland_state,
		.frame = 0,
		.frameLimit = 200,
		.vkImportSemaphoreFdKHR = (PFN_vkImportSemaphoreFdKHR) vkGetInstanceProcAddr(instance, "vkImportSemaphoreFdKHR"),
		.vkGetSemaphoreFdKHR = (PFN_vkGetSemaphoreFdKHR) vkGetInstanceProcAddr(instance, "vkGetSemaphoreFdKHR"),
	};
	if (argc > 1) {
		state.frameLimit = atoi(argv[1]);
	}

	VkSemaphoreCreateInfo semaphoreInfo= {
		.sType = VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO,
	};
	VkFenceCreateInfo fenceInfo = {
		.sType = VK_STRUCTURE_TYPE_FENCE_CREATE_INFO,
		.flags = VK_FENCE_CREATE_SIGNALED_BIT,		// Start in the signalled state
	};
	if (vkCreateSemaphore(device, &semaphoreInfo, NULL, &state.imageAvailableSemaphore) != VK_SUCCESS ||
			vkCreateFence(device, &fenceInfo, NULL, &state.inFlightFence) != VK_SUCCESS) {
		DIE("Failed to create semaphores!\n");
	}

	LOG("Start main loop\n");
	draw_frame(&state);
	while (wl_display_dispatch(wayland_state->display) != -1) {
	}

	return 0;
}
