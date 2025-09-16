struct UniformBufferObject {
  float4x4 rot;
  float4x4 proj;
};

layout(binding = 0) uniform ConstantBuffer<UniformBufferObject> ubo;
Sampler2D texture;

struct VSInput {
    float3 inPosition;
    float2 inTexCoord;
};

struct VSOutput {
    float4 pos : SV_Position;
    float2 fragTexCoord;
};

[shader("vertex")]
VSOutput vertMain(VSInput input) {
    VSOutput output;
    float4 model = mul(ubo.rot, float4(input.inPosition, 1.0));
    output.pos = mul(ubo.proj, model);
    output.fragTexCoord = input.inTexCoord;
    return output;
}

[shader("fragment")]
float4 fragMain(VSOutput inVert) : SV_Target {
    return texture.Sample(inVert.fragTexCoord);
}
