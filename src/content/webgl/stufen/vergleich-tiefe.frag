#version 300 es
precision highp float;
in vec3 v_color;
out vec4 color;
void main() {
    // die Tiefe im Z-Buffer, von 0 (near) bis 1 (far), zurückgerechnet in
    // den Abstand zur Kamera
    float z = gl_FragCoord.z * 2.0 - 1.0;
    float near = 0.1;
    float far = 100.0;
    float distance = 2.0 * far * near / (far + near - z * (far - near));
    // 3,5 Einheiten weit weiß, 6,5 Einheiten weit schwarz
    float gray = clamp((6.5 - distance) / 3.0, 0.0, 1.0);
    color = vec4(gray, gray, gray, 1.0);
}
