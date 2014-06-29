#version 110

uniform sampler2D texture;
varying vec2 f_coord;

void main() {
    gl_FragColor = texture2D(texture, f_coord);
}

