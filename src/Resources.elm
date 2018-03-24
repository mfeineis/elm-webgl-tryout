module Resources exposing (loadBackground)

import Task
import WebGL.Texture as Texture exposing (Texture)


loadBackground : (Texture -> msg) -> (Texture.Error -> msg) -> Cmd msg
loadBackground okMsg errorMsg =
    Texture.load "/assets/tex-svea.jpg"
        |> Task.attempt
            (\result ->
                case result of
                    Ok val ->
                        okMsg val

                    Err err ->
                        errorMsg err
            )
