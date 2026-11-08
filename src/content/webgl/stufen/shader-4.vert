#version 300 es
in vec3 position;
in vec3 normal;
uniform mat4 projection;
uniform mat4 view;
out vec3 v_normal;
void main() {
    gl_Position = projection * view * vec4(position, 1.0);
    v_normal = normal;
}
