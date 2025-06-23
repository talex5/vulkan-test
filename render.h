#include <vulkan/vulkan.h>

// The number of framebuffer images to allocate.
// This is the same number used by Mesa (WSI_WL_BUMPED_NUM_IMAGES)
#define N_IMAGES 4

// There is one of these for each allocated framebuffer
struct frame_state {
	VkExtent2D extent;			// Size input for pipeline
	void *uniformBufferMapped;		// Input arguments for shaders

	VkFramebuffer framebuffer;		// Buffer for output image
	struct wl_buffer *buffer;		// Corresponding Wayland object
	int dma_buf_fd;				// Corresponding Linux dmabuf
	VkSemaphore renderFinishedSemaphore;	// Signalled when rendering is complete

	VkDescriptorSet descriptorSets;		// Says to pass uniformBufferMapped to shaders
	struct render_state *ctx;		// Holds renderPass, pipelineLayout and graphicsPipeline
};

// Create rendering pipeline. Fills ctx, descriptorSets and uniformBufferMapped for each frame.
VkRenderPass render_create(VkPhysicalDevice physicalDevice, VkDevice device, VkFormat format, struct frame_state frames[4]);

// Note: frame_number increases every frame; it's not bound by N_IMAGES.
extern void record_render_commands(struct frame_state *frame, int frame_number, VkCommandBuffer commandBuffer);
