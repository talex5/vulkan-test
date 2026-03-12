struct UniformBufferObject {
  float4x4 project_world;
  float4x4 ship_rot;
  float3 ship_pos;
  float3 camera_pos;
  float2 land_visible_nw;
  uint32_t2 land_view_size;
  uint32_t2 map_size;
};
