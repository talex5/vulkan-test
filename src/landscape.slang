#include "ubo.slang"

layout(binding = 0) uniform ConstantBuffer<UniformBufferObject> ubo;
layout(binding = 1) uniform Sampler2D heightSampler;
layout(binding = 2) uniform Sampler2D tileSampler;

// slang fmod and % have weird rounding
#define mod(x, y) ((x) - (y) * floor((x) / (y)))

static const float deepSeaLevel = 0.1;
static const float seaLevel = 0.2;
static const float beachLevel = 0.5;

struct VSOutput {
    float4 screen_pos : SV_Position;
    float3 world_pos;
    float clarity;	// 0 for complete fog
};

[shader("vertex")]
VSOutput vertLandscape(uint vertexID : SV_VertexID) {
    VSOutput output;

    // Each vertex is mostly fixed in position on the screen.
    // e.g. vertex 0 is always the left-most and furthest back.
    // But vertex 0 changes height according to what bit of the map to show.
    // Convert the ID to a 2D point on this visible grid (e.g. 0 -> (0, 0)):
    int2 model_pos = int2(vertexID % ubo.land_view_size.x,
                          -(vertexID / ubo.land_view_size.x));

    // Find which bit of the map this vertex should show at the moment:
    int2 world_pos = model_pos + int2(floor(ubo.land_visible_nw));
    float z = heightSampler.SampleLevel(world_pos % ubo.map_size, 0).r * 10;

    // Store the world position for later; we'll need it to get the tile colour.
    output.world_pos = float3(world_pos, z);
    output.clarity = (frac(ubo.land_visible_nw.y) - model_pos.y) / ubo.land_view_size.y;

    // Render the surface of the sea, not the bottom:
    float4 world_pos_clamped = float4(world_pos, max(z, seaLevel), 1.0);
    output.screen_pos = mul(ubo.project_world, world_pos_clamped);

    return output;
}

float4 landColour(float3 world_pos) {
    if (world_pos.z < deepSeaLevel) return float4(0.0, 0.2, 0.8, 1.0);
    if (world_pos.z < seaLevel) return float4(0.0, 0.4, 1.0, 1.0);
    if (world_pos.z < beachLevel) return float4(1.0, 0.8, 0.0, 1.0);
    float2 pos = mod(floor(world_pos.xy), ubo.map_size);
    return tileSampler.SampleLevel(pos, 0);
}

[shader("fragment")]
float4 fragLandscape(VSOutput inVert) : SV_Target {
    float4 c = landColour(inVert.world_pos);
    float fog = smoothstep(0.0, 0.5, inVert.clarity);
    float2 to_ship_xy = inVert.world_pos.xy - ubo.ship_pos.xy;
    float shadow = 0.2 + 0.8 * smoothstep(0.5, 1.5, length(to_ship_xy % ubo.map_size));
    return c * fog * shadow;
}
