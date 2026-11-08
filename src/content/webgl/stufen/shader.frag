#version 300 es
precision highp float;
in vec3 v_position;
in vec2 v_uv;
in vec3 v_normal;
uniform sampler2D colorMap;
uniform sampler2D normalMap;
uniform sampler2D roughnessMap;
uniform sampler2D aoMap;
uniform sampler2D metalMap;
uniform sampler2D environment;
uniform bool useColor;
uniform bool useNormal;
uniform bool useRoughness;
uniform bool useAo;
uniform bool useMetal;
uniform bool glass;
uniform bool wire;
uniform vec3 light;
uniform vec3 eye;
out vec4 color;

// die Normale aus der Normal Map, gedreht auf die Fläche
vec3 mapNormal(vec3 n) {
    // wie sich Lage und Stelle in der Textur von Pixel zu Pixel ändern
    vec3 dp1 = dFdx(v_position);
    vec3 dp2 = dFdy(v_position);
    vec2 duv1 = dFdx(v_uv);
    vec2 duv2 = dFdy(v_uv);
    // daraus die Richtungen t und b, in die u und v auf der Fläche wachsen
    float s = sign(duv1.x * duv2.y - duv1.y * duv2.x);
    vec3 t = normalize(dp1 * duv2.y - dp2 * duv1.y) * s;
    vec3 b = normalize(dp2 * duv1.x - dp1 * duv2.x) * s;
    // die Farbe der Normal Map von 0 bis 1 wird eine Richtung von -1 bis 1
    vec3 m = texture(normalMap, v_uv).rgb * 2.0 - 1.0;
    return normalize(m.x * t + m.y * b + m.z * n);
}

// die Farbe der Umgebung in Richtung d, aus dem Panorama; blur wählt eine
// kleinere, verschwommenere Fassung (eine Mipmap)
vec3 environmentColor(vec3 d, float blur) {
    float u = 0.5 + atan(d.x, -d.z) / (2.0 * 3.14159265);
    float v = 0.5 + asin(clamp(d.y, -1.0, 1.0)) / 3.14159265;
    return textureLod(environment, vec2(u, v), blur).rgb;
}

void main() {
    if (wire) {
        color = vec4(1.0, 1.0, 1.0, 1.0);
        return;
    }
    vec3 base = useColor ? texture(colorMap, v_uv).rgb : vec3(0.9, 0.88, 0.84);
    vec3 n = normalize(v_normal);
    if (useNormal) n = mapNormal(n);
    float roughness = 0.5;
    float shininess = 40.0;
    float strength = 0.4;
    if (useRoughness) {
        roughness = texture(roughnessMap, v_uv).r;
        shininess = mix(200.0, 4.0, roughness);
        strength = 1.0 - roughness;
    }
    float ao = useAo ? texture(aoMap, v_uv).r : 1.0;
    float metal = useMetal ? texture(metalMap, v_uv).r : 0.0;

    vec3 l = normalize(light);
    vec3 v = normalize(eye - v_position);
    vec3 h = normalize(l + v);
    float diffuse = max(dot(n, l), 0.0);
    float specular = strength * pow(max(dot(n, h), 0.0), shininess);

    // was die Fläche spiegelt: die Umgebung in Richtung des gespiegelten Blicks
    vec3 mirrored = environmentColor(reflect(-v, n), roughness * 8.0);
    // schräg gesehen spiegelt jede Fläche mehr (nach Schlick)
    float fresnel = 0.04 + 0.96 * pow(1.0 - max(dot(n, v), 0.0), 5.0);
    if (glass) {
        // Glas: fast nur Spiegelung, schräg mehr als von vorne. Echtes Glas
        // spiegelt von vorne 4 Prozent, hier sind es 15, damit man es sieht
        float reflectance = 0.15 + 0.85 * pow(1.0 - max(dot(n, v), 0.0), 5.0);
        color = vec4(mirrored * reflectance + specular, reflectance);
        return;
    }
    // Lack und Farbe: Licht, Glanz und ein wenig Spiegelung
    vec3 painted = base * (0.3 * ao + 0.7 * diffuse) + specular + mirrored * fresnel * (1.0 - roughness);
    // Metall: spiegelt alles, in seiner eigenen Farbe
    vec3 shiny = mirrored * base * ao + specular * base;
    color = vec4(mix(painted, shiny, metal), 1.0);
}
