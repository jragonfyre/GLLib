--
-- Shaders.hs
-- Copyright (C) 2016 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Tut6.Shaders
  ( shadTestShader
  , shadowProgInfo
  , progInfo2
  ) where

import Tut6.Types

shadTestShader :: ProgramInfo
shadTestShader = ProgInfo
  { vertexShader = unlines 
      [ "#version 330 core"
      , "in vec3 vertexPos_ms;"
      , "uniform mat4 M;"
      , "uniform mat4 projection;" -- temporary
      , "out vec4 frag_pos_ws;"
      , "void main()"
      , "{"
      ,   "frag_pos_ws = M * vec4(vertexPos_ms,1);"
      ,   "gl_Position = projection * frag_pos_ws;"
      --,   "gl_Position = M * vec4(vertexPos_ms,1);"
      , "}"
      ]
  , geometryShader = Nothing
  , fragmentShader = unlines
      [ "#version 330 core"
      , "in vec4 frag_pos_ws;"
      , "out vec4 fragColor;"
      , "uniform vec3 lightPos;"
      , "uniform float far_plane;"
      , "void main() {"
      ,   "float ld = length(frag_pos_ws.xyz-lightPos);"
      ,   "ld = ld / far_plane;"
      --,   "ld=ld/5.0;"
      ,   "gl_FragDepth = ld;" -- 0.0002;"--ld;"
      --,   "if(ld < 0.5) {"
      --,     "fragColor = vec4(1,0,0,1);"
      --,   "} else {"
      --,     "fragColor = vec4(0,0,1,1);"
      --,   "}"
      ,   "vec3 fragc = (1-ld) * vec3(1);"
      --,   "vec3 fragc = ((frag_pos_ws.xyz/2)+1)/2;"
      ,   "fragColor = vec4(fragc,1);"
      , "}"
      ]
  }

shadowProgInfo :: ProgramInfo
shadowProgInfo = ProgInfo
  { vertexShader = unlines 
      [ "#version 330 core"
      , "in vec3 vertexPos_ms;"
      , "uniform mat4 M;"
      --, "uniform mat4 projection;" -- temporary
      --, "out vec4 frag_pos_ws;"
      , "void main()"
      , "{"
      --,   "vec4 frag_pos_ws = M * vec4(vertexPos_ms,1);"
      --,   "gl_Position = projection * frag_pos_ws;"
      ,   "gl_Position = M * vec4(vertexPos_ms,1);"
      , "}"
      ]
  --, geometryShader = Nothing
  , geometryShader = Just $ unlines
      [ "#version 330 core"
      , "uniform mat4 cubeMapProjections[6];"
      , "layout(triangles) in;"
      , "layout(triangle_strip, max_vertices = 18) out;"
      , "out vec4 frag_pos_ws;"
      , "void main() {"
      ,   "for(int j = 0; j < 6; ++j) {"
      ,     "gl_Layer = j;"
      ,     "for(int i = 0; i < 3; ++i) {"
      ,       "frag_pos_ws = gl_in[i].gl_Position;"
      ,       "gl_Position = cubeMapProjections[j] * frag_pos_ws;"
      ,       "EmitVertex();"
      ,     "}"
      ,     "EndPrimitive();"
      ,   "}"
      , "}"
      ]
  , fragmentShader = unlines
      [ "#version 330 core"
      , "in vec4 frag_pos_ws;"
      , "uniform vec3 lightPos;"
      , "uniform float far_plane;"
      , "void main() {"
      ,   "float ld = length(frag_pos_ws.xyz-lightPos);"
      ,   "ld = ld / far_plane;"
      ,   "gl_FragDepth = ld;" -- 0.0002;"--ld;"
      , "}"
      ]
  }

progInfo2 = ProgInfo vertexShader2 Nothing fragmentShader2

vertexShader2 :: String
vertexShader2 = unlines
  [ "#version 330 core"
  , "in vec3 vertexPos_ms;"
  , "in vec3 vertexColor;"
  , "in vec3 vertexNormal_ms;"
  , "uniform mat4 M;"
  , "uniform mat4 VP;"
  , "out vec3 fragmentColor;"
  , "out vec4 fragmentNormal;"
  , "out vec4 fragPos_ws;"
  , "void main()"
  , "{"
  ,   "fragmentColor = vertexColor;"
  ,   "vec4 v = vec4(vertexPos_ms,1);"
  ,   "fragPos_ws = M * v;"
  ,   "gl_Position = VP * fragPos_ws;"
  ,   "vec4 n = vec4(vertexNormal_ms,0);"
  ,   "fragmentNormal = M * n;"
  , "}"
  ]

-- Type 0
-- Point source
-- Type 1
-- Uniform source
fragmentShader2 :: String
fragmentShader2 = unlines
  [ "#version 330 core"
  , "in vec3 fragmentColor;"
  , "in vec4 fragmentNormal;"
  , "in vec4 fragPos_ws;"
  , "struct Light {"
  ,   "int type;"
  ,   "vec3 lightPos;"
  ,   "vec3 color;"
  , "};"
  , "uniform samplerCube shadowMap;"
  , "uniform float far_plane;"
  , "uniform vec3 cameraPos;"
  , "uniform float specPow;"
  , "uniform Light lights[5];"
  , "uniform int numLights;"
  , "out vec3 finalColor;"
  , "highp float rand(vec2 co) {"
  ,   "highp float a = 12.9898;"
  ,   "highp float b = 78.233;"
  ,   "highp float c = 43758.5453;"
  ,   "highp float dt= dot(co.xy ,vec2(a,b));"
  ,   "highp float sn= mod(dt,3.14);"
  ,   "return fract(sin(sn) * c);"
  , "}"
  , "vec3 sampleOffsetDirections[20] = vec3[]"
  , "("
  ,   "vec3( 1,  1,  1), vec3( 1, -1,  1), vec3(-1, -1,  1), vec3(-1,  1,  1),"
  ,   "vec3( 1,  1, -1), vec3( 1, -1, -1), vec3(-1, -1, -1), vec3(-1,  1, -1),"
  ,   "vec3( 1,  1,  0), vec3( 1, -1,  0), vec3(-1, -1,  0), vec3(-1,  1,  0),"
  ,   "vec3( 1,  0,  1), vec3(-1,  0,  1), vec3( 1,  0, -1), vec3(-1,  0, -1),"
  ,   "vec3( 0,  1,  1), vec3( 0, -1,  1), vec3( 0, -1, -1), vec3( 0,  1, -1)"
  , ");"
  , "float ShadowCalculation(vec3 fromLight, float depth, float viewdist) {"
  ,   "float shadow = 0.0;"
  ,   "float bias = 0.15;"
  ,   "int samples = 20;"
  ,   "float diskRadius = (1.0+(viewdist/far_plane))/500.0;"
  ,   "for(int i = 0; i < samples; ++i) {"
  ,     "float closestDepth = texture(shadowMap,(fromLight/depth)+(sampleOffsetDirections[i]*diskRadius)).r;"
  ,     "closestDepth *= far_plane;"
  ,     "shadow += depth-bias > closestDepth ? 1.0 : 0.0;"
  ,   "}"
  ,   "shadow /= float(samples);"
  ,   "return shadow;"
  , "}"
  , "void main()"
  , "{"
  ,   "vec3 normal = normalize(fragmentNormal.xyz);"
  ,   "vec3 diffLightIntensities=vec3(0);"
  ,   "vec3 specLightIntensities=vec3(0);"
  ,   "vec3 pos=fragPos_ws.xyz;"
  ,   "float shadow=0.0;"
  ,   "vec3 toLight;"
  ,   "for(int j = 0; j < numLights; j++) {"
  ,     "if(lights[j].type == 0) {"
  ,       "toLight=lights[j].lightPos-pos;"
  ,       "float ll = length(toLight);"
  ,       "vec3 l = toLight/ll;"
  ,       "vec3 view = cameraPos-pos;"
  ,       "vec3 v = normalize(view);"
  ,       "vec3 h = normalize(l+v);"
  ,       "shadow = ShadowCalculation(-toLight,ll,length(view));"
  ,       "float att = (1.0-shadow)/(ll*ll);"
  ,       "float diffuseAtt = max(dot(l,normal),0)*att;"
  ,       "float specularAtt = pow(max(dot(h,normal),0),specPow)*att;"
  ,       "diffLightIntensities = diffLightIntensities + diffuseAtt*lights[j].color;"
  ,       "specLightIntensities = specLightIntensities + specularAtt * lights[j].color;"
  ,     "} else if(lights[j].type == 1) {"
  ,       "diffLightIntensities = diffLightIntensities + lights[j].color;"
  ,     "}"
  ,   "}"
  ,   "finalColor = diffLightIntensities * fragmentColor+specLightIntensities;"
  ,   "finalColor = finalColor + 0.0005*vec3(rand(fragPos_ws.xy));"
  -- dither to eliminate banding in
  -- low frequency color changes.
  ,   "finalColor = pow(finalColor, vec3(1.0/2.2));" -- gamma correct
  , "}"
  ] 
















