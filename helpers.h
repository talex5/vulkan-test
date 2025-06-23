#include <vulkan/vulkan.h>

#define LOG(fmt, ...) fprintf(stderr, fmt, ##__VA_ARGS__)
#define DIE(fmt, ...) ({ fprintf(stderr, fmt, ##__VA_ARGS__); exit(1); })

extern VkShaderModule loadShaderModule(VkDevice device, char *path);
extern uint32_t find_graphics_family(VkPhysicalDevice device);
extern uint32_t findMemoryType(VkPhysicalDeviceMemoryProperties *memProperties,
			uint32_t typeFilter,
			VkMemoryPropertyFlags properties);
extern void createBuffer(VkDevice device,
		  VkPhysicalDeviceMemoryProperties *memProperties,
		  VkDeviceSize size,
		  VkBufferUsageFlags usage,
		  VkMemoryPropertyFlags properties,
		  VkBuffer *buffer,
		  VkDeviceMemory *bufferMemory);
extern void show_extensions(void);
extern void createExportSemaphore(VkDevice device, VkSemaphore *semaphore);
