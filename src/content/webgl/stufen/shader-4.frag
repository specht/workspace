#version 300 es
precision highp float;
in vec3 v_normal;
uniform vec3 light;
out vec4 color;
void main() {
    vec3 base = vec3(0.9, 0.88, 0.84);
    float diffuse = max(dot(normalize(v_normal), normalize(light)), 0.0);
    color = vec4(base * (0.3 + 0.7 * diffuse), 1.0);
}
