module ObjData exposing (basicShapeExpectedOutput, basicShapeWithTextureAndNormals)

import Math.Vector2 exposing (vec2)
import Math.Vector3 exposing (vec3)
import Math.Vector4 exposing (vec4)


basicShapeWithTextureAndNormals =
    """
v 0.459620 3.630721 0.459620
v 0.650002 3.630721 0.000000
v 0.000000 3.835475 0.000000
v 0.000000 3.630721 0.650002

vt 0.861694 0.549074
vt 0.877784 0.587917
vt 0.822851 0.587917
vt 0.889835 0.520933
vt 0.861694 0.626760
vt 0.822851 0.532984

vn 0.301361 0.878782 0.370032
vn 0.474745 0.878783 0.048561
vn 0.000000 1.000000 0.000000
vn -0.048561 0.878783 0.474745
vn 0.301361 0.878782 0.370032
vn 0.000000 1.000000 0.000000

f 1/1/1 2/2/2 3/3/3
f 4/6/4 1/1/5 3/3/6
"""


basicShapeExpectedOutput =
    [ { normal = vec3 0.301361 0.878782 0.370032
      , position = vec3 0.45962 3.630721 0.45962
      , tangent = vec4 0.947002 -0.321108 -0.008663 1
      , texCoord = vec2 0.861694 0.549074
      }
    , { normal = vec3 0.474744 0.878782 0.04856
      , position = vec3 0.650002 3.630721 0
      , tangent = vec4 0.879998 -0.474885 -0.009335 1
      , texCoord = vec2 0.877784 0.587917
      }
    , { normal = vec3 0 1 0
      , position = vec3 0 3.835475 0
      , tangent = vec4 1 0 0 1
      , texCoord = vec2 0.822851 0.587917
      }
    , { normal = vec3 -0.04856 0.878782 0.474744
      , position = vec3 0 3.630721 0.650002
      , tangent = vec4 0.996878 0.013016 0.077875 1
      , texCoord = vec2 0.822851 0.532984
      }
    , { normal = vec3 0.301361 0.878782 0.370032
      , position = vec3 0.45962 3.630721 0.45962
      , tangent = vec4 0.952264 -0.297212 -0.069697 1
      , texCoord = vec2 0.861694 0.549074
      }
    , { normal = vec3 0 1 0
      , position = vec3 0 3.835475 0
      , tangent = vec4 0.999999 0 0.000011 1
      , texCoord = vec2 0.822851 0.587917
      }
    ]
