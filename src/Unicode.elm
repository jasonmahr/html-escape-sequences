module Unicode exposing (text', txt, unEsc)

{-| Use HTML escape sequences with elm-html. Supports all Unicode characters and
recognizes decimal, hexadecimal and non-digit escape sequences.
@docs unEsc, text', txt
-}

import Handlers exposing (handleAllEscapes)
import Html exposing (Html, text)


{-| Unescapes all valid HTML escape sequences within a string.

    unEsc "© &COPY; &copy; &#169; &#x000A9; &cpy;" == "© © © © © &cpy;"
-}
unEsc : String -> String
unEsc =
  handleAllEscapes


{-| Unescapes a string and then invokes `Html.text` on the result.

    Html.text "© &copy; &#169;"  -- renders © &copy; &#169; to the page
    text'     "© &copy; &#169;"  -- renders © © ©
-}
text' : String -> Html msg
text' =
  unEsc >> text


{-| Alias for `text'` that avoids apostrophes.
-}
txt : String -> Html msg
txt =
  text'
