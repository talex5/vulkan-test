#include "ubo.slang"

layout(binding = 0) uniform ConstantBuffer<UniformBufferObject> ubo;

struct ShipVertex {
    float3 pos;
    float3 colour;
    float3 normal;
};

struct VSOutput {
    float4 pos : SV_Position;
    nointerpolation float3 colour;
};

static const float3 sun_dir = { 0, 0, 1 };              // Directly above us
static const float3 sunlight = { 1, 1, 0.1 };           // Yellowish

[shader("vertex")]
VSOutput vertShip(ShipVertex vert) {
    VSOutput output;

    float4 world_pos = float4(ubo.ship_pos, 0) + mul(ubo.ship_rot, float4(vert.pos, 1));
    output.pos = mul(ubo.project_world, world_pos);

    // Calculate face colour (Phong shading)

    float3 normal = mul(ubo.ship_rot, float4(vert.normal, 1)).xyz;

    // Degree to which this face is towards the sun
    float dot_sun_normal = dot(normal, sun_dir);                // -ve if facing away

    float diffuse = max(0, dot_sun_normal);

    // Vector of reflected sunlight
    float3 refl = normalize(2.0 * dot(sun_dir, normal) * normal - sun_dir);

    // In theory, we should do this in the fragment shader, but it's simpler
    // to do it once and just take the centre of the ship as the starting point.
    float3 viewer = normalize(ubo.camera_pos - ubo.ship_pos);

    // Degree to which the viewer is in the direction of the sun's reflection
    float dot_refl_viewer = dot(refl, viewer);          // -ve if facing away

    float specular = dot_sun_normal > 0 && dot_refl_viewer > 0 ? pow(dot_refl_viewer, 5) : 0;

    output.colour =
      0.2 * vert.colour +               // ambient light
      0.8 * vert.colour * diffuse +
      0.4 * sunlight * specular;

    return output;
}

[shader("fragment")]
float4 fragShip(VSOutput inVert) : SV_Target {
    return float4(inVert.colour, 1);
}
