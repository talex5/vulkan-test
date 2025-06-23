#include <stdio.h>
#include <stdlib.h>
#include <vulkan/vulkan.h>
#include "helpers.h"
#include "render.h"

struct UniformBufferObject {
	float dist;
};

struct render_state {
	VkRenderPass renderPass;
	VkPipeline graphicsPipeline;
	VkPipelineLayout pipelineLayout;
};

VkRenderPass render_create(VkPhysicalDevice physicalDevice, VkDevice device, VkFormat format, struct frame_state frames[4]) {
	struct render_state *x = malloc(sizeof(struct render_state));

	// Shaders

	VkShaderModule vertShaderModule = loadShaderModule(device, "shaders/vert.spv");
	VkShaderModule fragShaderModule = loadShaderModule(device, "shaders/frag.spv");

	VkPipelineShaderStageCreateInfo vertShaderStageInfo = {
		.sType = VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO,
		.stage = VK_SHADER_STAGE_VERTEX_BIT,
		.module = vertShaderModule,
		.pName = "main",
	};
	VkPipelineShaderStageCreateInfo fragShaderStageInfo = {
		.sType = VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO,
		.stage = VK_SHADER_STAGE_FRAGMENT_BIT,
		.module = fragShaderModule,
		.pName = "main",
	};
	VkPipelineShaderStageCreateInfo shaderStages[] = {vertShaderStageInfo, fragShaderStageInfo};

	// Pipeline input data

	LOG("vkCreateDescriptorSetLayout\n");
	VkDescriptorSetLayoutBinding uboLayoutBinding = {
		.binding = 0,
		.descriptorType = VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
		.descriptorCount = 1,
		.stageFlags = VK_SHADER_STAGE_VERTEX_BIT,
	};
	VkDescriptorSetLayoutCreateInfo layoutInfo = {
		.sType = VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO,
		.bindingCount = 1,
		.pBindings = &uboLayoutBinding,
	};
	VkDescriptorSetLayout descriptorSetLayout;
	if (vkCreateDescriptorSetLayout(device, &layoutInfo, NULL, &descriptorSetLayout) != VK_SUCCESS) {
		DIE("failed to create descriptor set layout!\n");
	}

	// Vertex information (none)

	VkPipelineVertexInputStateCreateInfo vertexInputInfo = {
		.sType = VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO,
		.vertexBindingDescriptionCount = 0,		// Shader hard-codes them
		.vertexAttributeDescriptionCount = 0,
	};
	VkPipelineInputAssemblyStateCreateInfo inputAssembly = {
		.sType = VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO,
		.topology = VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST,
		.primitiveRestartEnable = VK_FALSE,
	};

	// Viewport and scissors (values are set dynamically later)

	VkViewport viewport = {
		.x = 0.0f,
		.y = 0.0f,
		.minDepth = 0.0f,
		.maxDepth = 1.0f,
	};
	VkRect2D scissor = {
		.offset = {0, 0},
		.extent = {0},
	};
	VkPipelineViewportStateCreateInfo viewportState = {
		.sType = VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO,
		.viewportCount = 1,
		.pViewports = &viewport,
		.scissorCount = 1,
		.pScissors = &scissor,
	};

	// Rendering

	VkPipelineRasterizationStateCreateInfo rasterizer = {
		.sType = VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO,
		.depthClampEnable = VK_FALSE,
		.rasterizerDiscardEnable = VK_FALSE,
		.polygonMode = VK_POLYGON_MODE_FILL,
		.lineWidth = 1.0f,
		.cullMode = VK_CULL_MODE_BACK_BIT,
		.frontFace = VK_FRONT_FACE_CLOCKWISE,
		.depthBiasEnable = VK_FALSE,
	};
	VkPipelineMultisampleStateCreateInfo multisampling = {
		.sType = VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO,
		.sampleShadingEnable = VK_FALSE,
		.rasterizationSamples = VK_SAMPLE_COUNT_1_BIT,
	};
	VkPipelineColorBlendAttachmentState colorBlendAttachment = {
		.colorWriteMask =
			VK_COLOR_COMPONENT_R_BIT |
			VK_COLOR_COMPONENT_G_BIT |
			VK_COLOR_COMPONENT_B_BIT |
			VK_COLOR_COMPONENT_A_BIT,
		.blendEnable = VK_FALSE,
	};
	VkPipelineColorBlendStateCreateInfo colorBlending = {
		.sType = VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO,
		.logicOpEnable = VK_FALSE,
		.attachmentCount = 1,
		.pAttachments = &colorBlendAttachment,
	};
	VkPipelineLayoutCreateInfo pipelineLayoutInfo = {
		.sType = VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO,
		.setLayoutCount = 1,
		.pSetLayouts = &descriptorSetLayout,
		.pushConstantRangeCount = 0, // Optional
		.pPushConstantRanges = NULL, // Optional
	};
	LOG("vkCreatePipelineLayout\n");
	if (vkCreatePipelineLayout(device, &pipelineLayoutInfo, NULL, &x->pipelineLayout) != VK_SUCCESS) {
		DIE("failed to create pipeline layout!\n");
	}

	VkAttachmentDescription colorAttachment = {
		.format = format,
		.samples = VK_SAMPLE_COUNT_1_BIT,
		.loadOp = VK_ATTACHMENT_LOAD_OP_CLEAR,	// Clear framebuffer before rendering
		.storeOp = VK_ATTACHMENT_STORE_OP_STORE,
		.stencilLoadOp = VK_ATTACHMENT_LOAD_OP_DONT_CARE,
		.stencilStoreOp = VK_ATTACHMENT_STORE_OP_DONT_CARE,
		.initialLayout = VK_IMAGE_LAYOUT_UNDEFINED,
		.finalLayout = VK_IMAGE_LAYOUT_GENERAL,
	};
	VkAttachmentReference colorAttachmentRef = {
		.attachment = 0,
		.layout = VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
	};
	VkSubpassDescription subpass = {
		.pipelineBindPoint = VK_PIPELINE_BIND_POINT_GRAPHICS,
		.colorAttachmentCount = 1,
		.pColorAttachments = &colorAttachmentRef,
	};
	VkSubpassDependency dependency = {
		.srcSubpass = VK_SUBPASS_EXTERNAL,
		.dstSubpass = 0,
		.srcStageMask = VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
		.srcAccessMask = 0,
		.dstStageMask = VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
		.dstAccessMask = VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT,
	};
	VkRenderPassCreateInfo renderPassInfo = {
		.sType = VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO,
		.attachmentCount = 1,
		.pAttachments = &colorAttachment,
		.subpassCount = 1,
		.pSubpasses = &subpass,
		.dependencyCount = 1,
		.pDependencies = &dependency,
	};
	LOG("vkCreateRenderPass\n");
	if (vkCreateRenderPass(device, &renderPassInfo, NULL, &x->renderPass) != VK_SUCCESS) {
		DIE("failed to create render pass!\n");
	}

	VkDynamicState dynamicStates[] = {
		VK_DYNAMIC_STATE_VIEWPORT,
		VK_DYNAMIC_STATE_SCISSOR
	};

	VkPipelineDynamicStateCreateInfo dynamicState = {
		.sType = VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO,
		.dynamicStateCount = 2,
		.pDynamicStates = dynamicStates,
	};


	VkPhysicalDeviceMemoryProperties memProperties;
	vkGetPhysicalDeviceMemoryProperties(physicalDevice, &memProperties);

	VkDeviceSize bufferSize = sizeof(struct UniformBufferObject);

	VkDescriptorPoolSize dPoolSize = {
		.type = VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
		.descriptorCount = (uint32_t) N_IMAGES,
	};
	VkDescriptorPoolCreateInfo dPoolInfo = {
		.sType = VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO,
		.poolSizeCount = 1,
		.pPoolSizes = &dPoolSize,
		.maxSets = (uint32_t) N_IMAGES,
	};
	VkDescriptorPool descriptorPool;
	LOG("vkCreateDescriptorPool\n");
	if (vkCreateDescriptorPool(device, &dPoolInfo, NULL, &descriptorPool) != VK_SUCCESS) {
		DIE("failed to create descriptor pool!\n");
	}

	VkDescriptorSetLayout layouts[N_IMAGES];
	for (int i = 0; i < N_IMAGES; i++) {
		layouts[i] = descriptorSetLayout;
	}
	VkDescriptorSetAllocateInfo dAllocInfo = {
		.sType = VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO,
		.descriptorPool = descriptorPool,
		.descriptorSetCount = (uint32_t) N_IMAGES,
		.pSetLayouts = layouts,
	};
	VkDescriptorSet descriptorSets[N_IMAGES];
	LOG("vkAllocateDescriptorSets\n");
	if (vkAllocateDescriptorSets(device, &dAllocInfo, descriptorSets) != VK_SUCCESS) {
		DIE("failed to allocate descriptor sets!\n");
	}
	for (int i = 0; i < N_IMAGES; i++) {
		LOG("Create uniform buffer %d\n", i);
		VkBuffer uniformBuffer;
		VkDeviceMemory uniformBufferMemory;
		struct frame_state *frame = &frames[i];
		frame->ctx = x;

		LOG("createBuffer\n");
		createBuffer(device, &memProperties,
				bufferSize, VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT,
				VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT,
				&uniformBuffer,
				&uniformBufferMemory);
		LOG("vkMapMemory\n");
		vkMapMemory(device, uniformBufferMemory, 0, bufferSize, 0, &frame->uniformBufferMapped);

		frame->descriptorSets = descriptorSets[i];

		VkDescriptorBufferInfo bufferInfo = {
			.buffer = uniformBuffer,
			.offset = 0,
			.range = sizeof(struct UniformBufferObject),
		};
		VkWriteDescriptorSet descriptorWrite = {
			.sType = VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET,
			.dstSet = frame->descriptorSets,
			.dstBinding = 0,
			.dstArrayElement = 0,
			.descriptorType = VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
			.descriptorCount = 1,
			.pBufferInfo = &bufferInfo,
		};
		LOG("vkUpdateDescriptorSets\n");
		vkUpdateDescriptorSets(device, 1, &descriptorWrite, 0, NULL);
	}

	VkGraphicsPipelineCreateInfo pipelineInfo = {
		.sType = VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO,
		.stageCount = 2,
		.pStages = shaderStages,
		.pVertexInputState = &vertexInputInfo,
		.pInputAssemblyState = &inputAssembly,
		.pViewportState = &viewportState,
		.pRasterizationState = &rasterizer,
		.pMultisampleState = &multisampling,
		.pColorBlendState = &colorBlending,
		.pDynamicState = &dynamicState,
		.layout = x->pipelineLayout,
		.renderPass = x->renderPass,
		.subpass = 0,
	};
	LOG("vkCreateGraphicsPipeline\n");
	if (vkCreateGraphicsPipelines(device, VK_NULL_HANDLE, 1, &pipelineInfo, NULL, &x->graphicsPipeline) != VK_SUCCESS) {
		DIE("failed to create graphics pipeline!\n");
	}

	return x->renderPass;
}

void record_render_commands(struct frame_state *frame, int frame_number, VkCommandBuffer commandBuffer) {
	struct UniformBufferObject *ubo = frame->uniformBufferMapped;
	ubo->dist = (((float) (frame_number & 0xff)) - 100) / 100;

	vkResetCommandBuffer(commandBuffer, 0);
	VkDescriptorSet descriptors = frame->descriptorSets;

	VkCommandBufferBeginInfo beginInfo = {
		.sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO,
	};
	if (vkBeginCommandBuffer(commandBuffer, &beginInfo) != VK_SUCCESS) {
		DIE("failed to begin recording command buffer!\n");
	}

	VkClearValue clearColor = {{{0.0f, 0.0f, 0.0f, 1.0f}}};
	VkRenderPassBeginInfo renderPassInfo = {
		.sType = VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO,
		.renderPass = frame->ctx->renderPass,
		.framebuffer = frame->framebuffer,
		.renderArea.offset = {0, 0},
		.renderArea.extent = frame->extent,
		.clearValueCount = 1,
		.pClearValues = &clearColor,
	};
	vkCmdBeginRenderPass(commandBuffer, &renderPassInfo, VK_SUBPASS_CONTENTS_INLINE);
	vkCmdBindPipeline(commandBuffer, VK_PIPELINE_BIND_POINT_GRAPHICS, frame->ctx->graphicsPipeline);

	VkViewport viewport = {
		.x = 0.0f,
		.y = 0.0f,
		.width = (float) frame->extent.width,
		.height = (float) frame->extent.height,
		.minDepth = 0.0f,
		.maxDepth = 1.0f,
	};
	vkCmdSetViewport(commandBuffer, 0, 1, &viewport);

	VkRect2D scissor = {
		.offset = {0, 0},
		.extent = frame->extent,
	};
	vkCmdSetScissor(commandBuffer, 0, 1, &scissor);

	vkCmdBindDescriptorSets(commandBuffer, VK_PIPELINE_BIND_POINT_GRAPHICS, frame->ctx->pipelineLayout,
			0, 1, &descriptors, 0, NULL);

	vkCmdDraw(commandBuffer, 3, 1, 0, 0);

	vkCmdEndRenderPass(commandBuffer);

	if (vkEndCommandBuffer(commandBuffer) != VK_SUCCESS) {
		DIE("failed to record command buffer!\n");
	}
}
