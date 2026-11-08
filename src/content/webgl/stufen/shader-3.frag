#version 300 es
precision highp float;
in vec3 v_color;
in vec3 v_normal;
uniform vec3 light;
out vec4 color;
void main() {
    float diffuse = max(dot(normalize(v_normal), normalize(light)), 0.0);
    color = vec4(v_color * (0.3 + 0.7 * diffuse), 1.0);
}
