#version 110

uniform sampler2D texture;
varying vec2 f_coord;

void main() {
    gl_FragColor   = vec4(0, 0, texture2D(texture, f_coord).r, 0);
}

