#version 300 es
in vec3 position;
in vec2 uv;
in vec3 normal;
uniform mat4 projection;
uniform mat4 view;
out vec3 v_position;
out vec2 v_uv;
out vec3 v_normal;
void main() {
    gl_Position = projection * view * vec4(position, 1.0);
    v_position = position;
    v_uv = uv;
    v_normal = normal;
}
