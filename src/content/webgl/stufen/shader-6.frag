#version 300 es
precision highp float;
in vec3 v_position;
in vec3 v_normal;
uniform vec3 light;
uniform vec3 eye;
uniform bool wire;
out vec4 color;
void main() {
    if (wire) {
        color = vec4(1.0, 1.0, 1.0, 1.0);
        return;
    }
    vec3 base = vec3(0.9, 0.88, 0.84);
    vec3 n = normalize(v_normal);
    vec3 l = normalize(light);
    vec3 v = normalize(eye - v_position);
    vec3 h = normalize(l + v);
    float diffuse = max(dot(n, l), 0.0);
    float specular = pow(max(dot(n, h), 0.0), 40.0);
    color = vec4(base * (0.3 + 0.7 * diffuse) + 0.4 * specular, 1.0);
}
