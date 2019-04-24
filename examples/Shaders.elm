module Shaders exposing (..)

import WebGL


{-| This shader uses Spherical Environment Mapping (SEM).
Here are some relevant links:

  - [very cool demo](https://www.clicktorelease.com/code/spherical-normal-mapping/#)
  - <https://www.clicktorelease.com/blog/creating-spherical-environment-mapping-shader>
  - <http://www.ozone3d.net/tutorials/glsl_texturing_p04.php>

-}
reflectionVert =
    [glsl|

attribute vec3 position;
attribute vec3 normal;
uniform mat4 mvMat;
uniform mat4 camera;
varying vec3 vNormal;

void main()
{
    vec4 vertex4 = mvMat * vec4(position, 1.0);
    vNormal = vec3(mvMat * vec4(normal, 0.0));
    vec3 nm_z = normalize(vec3(vertex4));
    vec3 nm_x = cross(nm_z, vec3(0.0, 1.0, 0.0));
    vec3 nm_y = cross(nm_x, nm_z);
    vNormal = vec3(dot(vNormal, nm_x), dot(vNormal, nm_y), dot(vNormal, nm_z));
    gl_Position = camera * vertex4;
}

|]


reflectionFrag =
    [glsl|
precision mediump float;

uniform sampler2D texture;

varying vec3 vNormal;

void main()
{
    vec2 texCoord = vec2(0.5 * vNormal.x + 0.5, - 0.5 * vNormal.y - 0.5);
    vec4 fragColor = texture2D(texture, texCoord);
    fragColor.a = 1.0;

    gl_FragColor = fragColor;
}

|]


{-| normal mapping according to:
<http://www.gamasutra.com/blogs/RobertBasler/20131122/205462/Three_Normal_Mapping_Techniques_Explained_For_the_Mathematically_Uninclined.php?print=1>
-}
normalVert =
    [glsl|
attribute vec3 position;
attribute vec3 normal;
attribute vec2 texCoord;
attribute vec4 tangent;

varying vec2 vTexCoord;
varying vec3 vLightDirection;
varying vec3 vViewDirection;


uniform mat4 modelViewProjectionMatrix;
uniform mat4 modelMatrix;
uniform vec3 lightPosition;
uniform vec3 viewPosition;

mat3 transpose(mat3 m) {
    return mat3(m[0][0], m[1][0], m[2][0],
                m[0][1], m[1][1], m[2][1],
                m[0][2], m[1][2], m[2][2]);
}


void main()
{
    vec4 pos = vec4(position, 1.0 );
    vec3 posWorld = (modelMatrix * pos).xyz;

    // Tangent, Bitangent, Normal space matrix TBN
    // this is not entirely correct, it should use the normal matrix
    // In this special case it works out well,
    // since my model matrix does not contain any rotation or translation.
    // http://www.lighthouse3d.com/tutorials/glsl-12-tutorial/the-normal-matrix/
    vec3 n = normalize((modelMatrix * vec4(normal, 0.0)).xyz);
    vec3 t = normalize((modelMatrix * vec4(tangent.xyz, 0.0)).xyz);
    vec3 b = normalize((modelMatrix * vec4((cross(normal, tangent.xyz) * tangent.w), 0.0)).xyz);
    mat3 tbn = transpose(mat3(t, b, n));
    vLightDirection = tbn*(lightPosition - posWorld);
    vViewDirection = tbn*(viewPosition - posWorld);
    vTexCoord = texCoord;
    gl_Position = modelViewProjectionMatrix * pos;
}
|]


normalFrag =
    [glsl|
precision mediump float;

uniform sampler2D textureDiff;
uniform sampler2D textureNorm;

varying vec2 vTexCoord;
varying vec3 vLightDirection;
varying vec3 vViewDirection;

void main() {

    vec3 lightDir = normalize(vLightDirection);

    // Local normal, in tangent space
    vec3 pixelNormal = normalize(texture2D(textureNorm, vTexCoord).rgb*2.0 - 1.0);
    float lambert = max(dot(pixelNormal, lightDir), 0.0);


    // diffuse + lambert
    vec3 lightIntensities = vec3(1.5, 1.0, 1.0);
    vec3 diffuseColor = texture2D(textureDiff, vTexCoord).rgb;
    vec3 diffuse = lambert * diffuseColor * lightIntensities;

    // ambient
    vec3 ambient = 0.3 * diffuseColor;

    // specular
    float shininess = 32.0;
    vec3 viewDir = normalize(vViewDirection);
    vec3 reflectDir = reflect(-lightDir, pixelNormal);
    vec3 halfwayDir = normalize(lightDir + viewDir);
    float spec = pow(max(dot(pixelNormal, halfwayDir), 0.0), shininess);
    vec3 specular = vec3(0.2) * spec * lightIntensities;

    // attenuation
    float lightAttenuation = 0.3;
    float attenuation = 1.0 / (1.0 + lightAttenuation * pow(length(vLightDirection), 2.0));

    vec3 final_color = ambient + (diffuse + specular) * attenuation;
    gl_FragColor = vec4(final_color, 1.0);
}
|]


{-| same as the normal mapping shader, but without deforming normals.
-}
noNormalVert =
    [glsl|
attribute vec3 position;
attribute vec3 normal;
attribute vec2 texCoord;

varying vec2 vTexCoord;
varying vec3 vLightDirection;
varying vec3 vViewDirection;
varying vec3 vNormal;

uniform mat4 modelViewProjectionMatrix;
uniform mat4 modelMatrix;
uniform vec3 lightPosition;
uniform vec3 viewPosition;

void main()
{
    vec4 pos = vec4(position, 1.0 );
    vec3 posWorld = (modelMatrix * pos).xyz;

    vLightDirection = lightPosition - posWorld;
    vViewDirection = viewPosition - posWorld;
    vTexCoord = texCoord;
    // this is incorrect, it should use the normal matrix
    vNormal = mat3(modelMatrix) * normal;
    gl_Position = modelViewProjectionMatrix * pos;
}
|]


noNormalFrag =
    [glsl|
precision mediump float;

uniform sampler2D textureDiff;

varying vec2 vTexCoord;
varying vec3 vLightDirection;
varying vec3 vViewDirection;
varying vec3 vNormal;

void main()
{
    vec3 lightDir = normalize(vLightDirection);

    // lambert
    vec3 pixelNormal = normalize(vNormal);
    float lambert = max(dot(pixelNormal, lightDir), 0.0);

    // diffuse + lambert
    vec3 lightIntensities = vec3(1.5, 1.0, 1.0);
    vec3 diffuseColor = texture2D(textureDiff, vTexCoord).rgb;
    vec3 diffuse = lambert * diffuseColor * lightIntensities;

    // ambient
    vec3 ambient = 0.3 * diffuseColor;

    // specular
    float shininess = 32.0;
    vec3 viewDir = normalize(vViewDirection);
    vec3 reflectDir = reflect(-lightDir, pixelNormal);
    vec3 halfwayDir = normalize(lightDir + viewDir);
    float spec = pow(max(dot(pixelNormal, halfwayDir), 0.0), shininess);
    vec3 specular = vec3(0.2) * spec * lightIntensities;

    // attenuation
    float lightAttenuation = 0.3;
    float attenuation = 1.0 / (1.0 + lightAttenuation * pow(length(vLightDirection), 2.0));

    vec3 final_color = ambient + (diffuse + specular) * attenuation;
    gl_FragColor = vec4(final_color, 1.0);
}
|]


{-| same as above, but without any textures.
-}
simpleVert =
    [glsl|
attribute vec3 position;
attribute vec3 normal;

varying vec3 vLightDirection;
varying vec3 vViewDirection;
varying vec3 vNormal;

uniform mat4 modelViewProjectionMatrix;
uniform mat4 modelMatrix;
uniform vec3 lightPosition;
uniform vec3 viewPosition;

void main()
{
    vec4 pos = vec4(position, 1.0 );
    vec3 posWorld = (modelMatrix * pos).xyz;

    vLightDirection = lightPosition - posWorld;
    vViewDirection = viewPosition - posWorld;
    // this is incorrect, it should use the normal matrix, like this:
    // vNormal = mat3(normalMatrix) * normal;
    // it works in this case, since the modelMatrix is the identity matrix
    vNormal = normal;
    gl_Position = modelViewProjectionMatrix * pos;
}
|]


simpleFrag =
    [glsl|
precision mediump float;

varying vec3 vLightDirection;
varying vec3 vViewDirection;
varying vec3 vNormal;

void main()
{
    vec3 lightDir = normalize(vLightDirection);

    // lambert
    vec3 pixelNormal = normalize(vNormal);
    float lambert = max(dot(pixelNormal, lightDir), 0.0);

    // diffuse + lambert
    vec3 lightIntensities = vec3(1.5, 1.0, 1.0);
    vec3 diffuseColor = vec3(0.3, 0.2, 0.95);
    vec3 diffuse = lambert * diffuseColor * lightIntensities;

    // ambient
    vec3 ambient = 0.2 * diffuseColor;

    // specular
    float shininess = 32.0;
    vec3 viewDir = normalize(vViewDirection);
    vec3 reflectDir = reflect(-lightDir, pixelNormal);
    vec3 halfwayDir = normalize(lightDir + viewDir);
    float spec = pow(max(dot(pixelNormal, halfwayDir), 0.0), shininess);
    vec3 specular = vec3(0.2) * spec * lightIntensities;

    // attenuation
    float lightAttenuation = 0.3;
    float attenuation = 1.0 / (1.0 + lightAttenuation * pow(length(vLightDirection), 2.0));

    vec3 final_color = ambient + (diffuse + specular) * attenuation;
    gl_FragColor = vec4(final_color, 1.0);
}
|]
