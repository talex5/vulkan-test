#include "ubo.slang"

layout(binding = 0) uniform ConstantBuffer<UniformBufferObject> ubo;

// slang fmod and % have weird rounding
#define mod(x, y) ((x) - (y) * floor((x) / (y)))

struct VSOutput {
    float4 pos : SV_Position;
    float3 colour;
    float size : SV_PointSize;
};

[shader("vertex")]
VSOutput vertMain(uint vertexID : SV_VertexID) {
    VSOutput output;
    Particle p = ubo.thrust[vertexID];

    // We need to handle the map wrapping. The particles will be near the ship,
    // so if they seem to be more than half a map away, treat them as negative.
    float2 to_ship = mod(p.pos.xy - ubo.ship_pos.xy + ubo.map_size / 2, ubo.map_size.xy) - ubo.map_size / 2;
    float4 world_pos = float4(to_ship + ubo.ship_pos.xy, p.pos.z, 1);

    output.pos = mul(ubo.project_world, world_pos);
    output.colour = float3(1 * p.brightness, 1 * smoothstep(0.5, 1, p.brightness), 0);
    output.size = 4;

    return output;
}

[shader("fragment")]
float4 fragMain(VSOutput inVert) : SV_Target {
    return float4(inVert.colour, 1);
}
