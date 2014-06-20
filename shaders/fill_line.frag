#version 110

uniform sampler2D texture;
uniform sampler2D colorMap;
varying vec2 f_coord;

uniform float offset;
uniform float scale;

void main() {
    float graph_height = texture2D(texture, vec2(f_coord.x, 0)).r;

    if(f_coord.y < graph_height){
        gl_FragColor = texture2D(colorMap, vec2(f_coord.y * scale + offset, 0));
    } else {
        gl_FragColor = vec4(0, 0, 0, 0);
    }
}

