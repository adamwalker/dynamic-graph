#version 110

attribute float coord;
uniform sampler2D texture;

void main(){
    float y     = texture2D(texture, vec2((coord + 1.0)/2.0, 0)).r;
    gl_Position = vec4(coord, y, 0, 1);
}
