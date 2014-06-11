#version 110

uniform sampler2D texture;
uniform sampler2D colorMap;
varying vec2 f_coord;

uniform float offset;
uniform float scale;

void main() {
    float intensity = texture2D(texture, f_coord).r;
    gl_FragColor    = texture2D(colorMap, vec2(intensity * scale + offset, 0));
    //gl_FragColor    = vec4(0, 0, intensity, 0); 
}

