#version 110

uniform sampler2D texture;
varying vec2 f_coord;

void main() {
    float height = texture2D(texture, vec2(f_coord.x, 0)).r;

    if(f_coord.y < height){
        gl_FragColor = vec4(1, 1, 1, 0);
    } else {
        gl_FragColor = vec4(0, 0, 0, 0);
    }
}

