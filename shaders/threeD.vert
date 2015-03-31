#version 110

attribute vec2      coord2d;
uniform   mat4      vertex_transform;
uniform   sampler2D texture;
uniform   float     voffset;

void main(void) {
    gl_Position = vertex_transform * vec4(coord2d, texture2D(texture, (coord2d / 2.0 + 0.5) + vec2(0, voffset)).r, 1);
}
