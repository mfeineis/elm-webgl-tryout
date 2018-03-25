module Render exposing (view)

import Html exposing (Html)
import Html.Attributes
import Math.Vector2 exposing (Vec2, vec2)
import WebGL exposing (Mesh, Shader)
import WebGL.Settings.Blend as Blend
import WebGL.Texture as Texture exposing (Texture)


type alias State a =
    { a
        | background : Maybe Texture
        , frame : Int
        , windowSize : { height : Int, width : Int }
    }


view : State a -> Html msg
view state =
    case state.background of
        Nothing ->
            Html.text ""

        Just bg ->
            viewScene bg state


viewScene : Texture -> State a -> Html msg
viewScene bg { frame, windowSize } =
    WebGL.toHtml
        [ Html.Attributes.height windowSize.height
        , Html.Attributes.style [ ( "display", "block" ) ]
        , Html.Attributes.width windowSize.width
        ]
        [ WebGL.entityWith
            [ Blend.add Blend.one Blend.oneMinusSrcAlpha ]
            vertexShader
            fragmentShader
            mesh
            { background = bg
            , frame = frame
            , screenSize = vec2 (toFloat windowSize.width) (toFloat windowSize.height)
            , textureSize = vec2 (toFloat (Tuple.first (Texture.size bg))) (toFloat (Tuple.second (Texture.size bg)))
            }
        ]


mesh : Mesh Vertex
mesh =
    WebGL.triangles
        [ ( Vertex (vec2 0 0)
          , Vertex (vec2 64 128)
          , Vertex (vec2 64 0)
          )
        , ( Vertex (vec2 0 0)
          , Vertex (vec2 0 128)
          , Vertex (vec2 64 128)
          )
        ]


type alias Vertex =
    { position : Vec2
    }


type alias Uniform =
    { background : WebGL.Texture
    , frame : Int
    , screenSize : Vec2
    , textureSize : Vec2
    }


type alias Varying =
    { texturePos : Vec2
    }


vertexShader : WebGL.Shader Vertex Uniform Varying
vertexShader =
    [glsl|

    attribute vec2 position;
    varying vec2 texturePos;

    void main() {
        vec2 offset = vec2(-1.0); //vec2(0.5);
        //vec2 offset = vec2(0.0);
        gl_Position = vec4(position + offset, 0, 1);
        texturePos = position;
    }

    |]


fragmentShader : WebGL.Shader {} { u | background : WebGL.Texture, screenSize : Vec2, textureSize : Vec2 } Varying
fragmentShader =
    [glsl|

    precision mediump float;

    uniform sampler2D background;
    uniform vec2 screenSize;
    uniform vec2 textureSize;

    varying vec2 texturePos;

    void main() {
        //vec2 p = texturePos / 2.0 - 1.0;
        vec2 p = vec2(texturePos / 2.0);
        gl_FragColor = texture2D(background, p);
    }

    |]
