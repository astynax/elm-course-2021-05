module Utils exposing (print)

import Debug
import Html exposing (Html)

print : a -> Html b
print v =
    Html.pre []
        [ Html.code []
              [ Html.text <| Debug.toString v
              ]
        ]
