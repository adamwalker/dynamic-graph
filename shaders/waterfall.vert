#version 110

attribute vec2 coord;
varying vec2 f_coord;
uniform float voffset;

void main(){
    gl_Position = vec4(coord, 0, 1);
    float f_x   = (coord.x + 1.0) / 2.0;
    float f_y   = (coord.y + 1.0) / 2.0 + voffset;
    f_coord     = vec2(f_x, f_y);
}
