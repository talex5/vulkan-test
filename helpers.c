#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>
#include <vulkan/vulkan.h>
#include "helpers.h"

static char *readFile(const char *path, size_t *lenOut) {
	int fd = open(path, O_RDONLY, 0);
	if (fd < 0) perror(path);
	struct stat info;
	fstat(fd, &info);
	int len = info.st_size;
	*lenOut = len;
	char *r = malloc(len);
	char *pos = r;
	while (len > 0) {
		int got = read(fd, pos, len);
		if (got < 0) perror("read");
		len -= got;
		pos += got;
	}
	close(fd);
	LOG("Loaded %s (%d bytes)\n", path, (int) *lenOut);
	return r;
}

VkShaderModule loadShaderModule(VkDevice device, char *path) {
	size_t len;
	char *code = readFile(path, &len);

	VkShaderModuleCreateInfo info = {
		.sType = VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO,
		.codeSize = len,
		.pCode = (const uint32_t*) code,
	};
	VkShaderModule shaderModule;
	if (vkCreateShaderModule(device, &info, NULL, &shaderModule) != VK_SUCCESS) {
		DIE("failed to create shader module!\n");
	}
	return shaderModule;
}

uint32_t find_graphics_family(VkPhysicalDevice device) {
	uint32_t queueFamilyCount = 0;
	vkGetPhysicalDeviceQueueFamilyProperties(device, &queueFamilyCount, NULL);
	VkQueueFamilyProperties *queueFamilies = malloc(sizeof(VkQueueFamilyProperties) * queueFamilyCount);
	vkGetPhysicalDeviceQueueFamilyProperties(device, &queueFamilyCount, queueFamilies);
	LOG("Device has %d queue families\n", queueFamilyCount);
	for (int i = 0; i < queueFamilyCount; i++) {
		VkQueueFamilyProperties queueFamily = queueFamilies[i];
		if (queueFamily.queueFlags & VK_QUEUE_GRAPHICS_BIT) {
			LOG("Found graphics queue family (%d)\n", i);
			return i;
		}
	}

	DIE("No graphics queue!\n");
}

uint32_t findMemoryType(VkPhysicalDeviceMemoryProperties *memProperties,
			uint32_t typeFilter,
			VkMemoryPropertyFlags properties) {
	for (uint32_t i = 0; i < memProperties->memoryTypeCount; i++) {
		if ((typeFilter & (1 << i)) &&
				(memProperties->memoryTypes[i].propertyFlags & properties) == properties) {
			return i;
		}
	}

	DIE("failed to find suitable memory type!\n");
}

void createBuffer(VkDevice device,
		  VkPhysicalDeviceMemoryProperties *memProperties,
		  VkDeviceSize size,
		  VkBufferUsageFlags usage,
		  VkMemoryPropertyFlags properties,
		  VkBuffer *buffer,
		  VkDeviceMemory *bufferMemory) {
    VkBufferCreateInfo bufferInfo = {
	    .sType = VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO,
	    .size = size,
	    .usage = usage,
	    .sharingMode = VK_SHARING_MODE_EXCLUSIVE,
    };

    if (vkCreateBuffer(device, &bufferInfo, NULL, buffer) != VK_SUCCESS) {
        DIE("failed to create buffer!\n");
    }

    VkMemoryRequirements memRequirements;
    vkGetBufferMemoryRequirements(device, *buffer, &memRequirements);

    VkMemoryAllocateInfo allocInfo = {
	    .sType = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO,
	    .allocationSize = memRequirements.size,
	    .memoryTypeIndex = findMemoryType(memProperties, memRequirements.memoryTypeBits, properties),
    };

    if (vkAllocateMemory(device, &allocInfo, NULL, bufferMemory) != VK_SUCCESS) {
        DIE("failed to allocate buffer memory!\n");
    }

    vkBindBufferMemory(device, *buffer, *bufferMemory, 0);
}

void show_extensions(void) {
	uint32_t extensionCount = 0;
	vkEnumerateInstanceExtensionProperties(NULL, &extensionCount, NULL);
	LOG("Extentions: %d\n", extensionCount);
	VkExtensionProperties *extensions = malloc(sizeof(VkExtensionProperties) * extensionCount);
	vkEnumerateInstanceExtensionProperties(NULL, &extensionCount, extensions);
	for (int i = 0; i < extensionCount; i++) {
		LOG("- %s\n", extensions[i].extensionName);
	}
}

void createExportSemaphore(VkDevice device, VkSemaphore *semaphore) {
	const VkExportSemaphoreCreateInfo exportInfo = {
		.sType = VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO,
		.handleTypes = VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT,
	};
	const VkSemaphoreCreateInfo semaphoreInfo = {
		.sType = VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO,
		.pNext = &exportInfo,
	};
	if (vkCreateSemaphore(device, &semaphoreInfo, NULL, semaphore) != VK_SUCCESS) {
		DIE("failed to create semaphore!\n");
	}
}
