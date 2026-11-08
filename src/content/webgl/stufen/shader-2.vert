#version 300 es
in vec3 position;
in vec3 color;
uniform mat4 projection;
uniform mat4 view;
uniform mat4 model;
out vec3 v_color;
void main() {
    gl_Position = projection * view * model * vec4(position, 1.0);
    v_color = color;
}
