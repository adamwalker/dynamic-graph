#version 110

attribute vec2 coord;
varying vec2 f_coord;

void main(){
    gl_Position    = vec4(coord, 0, 1);
    vec2 f_coord_p = (coord + vec2(1.0, 1.0)) / 2.0;
    f_coord        = vec2(f_coord_p.x, 1.0 - f_coord_p.y);
}
