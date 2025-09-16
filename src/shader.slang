struct UniformBufferObject {
  float dist;
};

layout(binding = 0) uniform ConstantBuffer<UniformBufferObject> ubo;

static float2 positions[3] = float2[](
    float2(0.0, -0.5),
    float2(0.5, 0.5),
    float2(-0.5, 0.5)
);

static float3 colours[3] = float3[](
    float3(1.0, 0.0, 0.0),
    float3(0.0, 1.0, 0.0),
    float3(0.0, 0.0, 1.0)
);

struct VSOutput {
    float4 pos : SV_Position;
    float3 colour;
};

[shader("vertex")]
VSOutput vertMain(uint vid : SV_VertexID) {
    VSOutput output;
    output.pos = float4(positions[vid], 0.0, 1.0);
    output.pos.x += ubo.dist;
    output.colour = colours[vid];
    return output;
}

[shader("fragment")]
float4 fragMain(VSOutput inVert) : SV_Target {
    return float4(inVert.colour, 1.0);
}
