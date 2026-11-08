#version 300 es
in vec3 position;
in vec3 color;
in vec3 normal;
uniform mat4 projection;
uniform mat4 view;
uniform mat4 model;
out vec3 v_color;
out vec3 v_normal;
void main() {
    gl_Position = projection * view * model * vec4(position, 1.0);
    v_color = color;
    v_normal = mat3(model) * normal;
}
