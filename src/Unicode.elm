module Unicode exposing (text_, txt, unEsc)

{-| Use HTML escape sequences with elm-lang/html. Supports all Unicode
characters and recognizes decimal, hexadecimal and named escape sequences.
@docs unEsc, text_, txt
-}

import Handlers exposing (handleEscapes)
import Html exposing (Html, text)


{-| Unescapes all valid HTML escape sequences within a string.

    unEsc "© &COPY; &copy; &#169; &#x000A9; &cpy;" == "© © © © © &cpy;"
-}
unEsc : String -> String
unEsc =
    handleEscapes


{-| Unescapes a string and then invokes `Html.text` on the result.

    Html.text "© &copy; &#169;"  -- renders © &copy; &#169; to the page
    text_     "© &copy; &#169;"  -- renders © © ©
-}
text_ : String -> Html msg
text_ =
    text << unEsc


{-| Alias for `text'` that avoids apostrophes.
-}
txt : String -> Html msg
txt =
    text_
