module Unicode (text', txt, unEsc) where

{-| Use HTML escape sequences with elm-html. Supports all Unicode characters and
recognizes decimal, hexadecimal and non-digit escape sequences.
@docs unEsc, text', txt
-}

import Handlers exposing (handleEscapes)
import Html exposing (Html, text)


{-| Unescapes all valid HTML escape sequences within a string.

    unEsc "© &COPY; &copy; &#169; &#x000A9; &cpy;" == "© © © © © &cpy;"
-}
unEsc : String -> String
unEsc =
  handleEscapes


{-| unEsc >> Html.text

    Html.text "© &copy; &#169;"  -- renders © &copy; &#169; to the page
    text'     "© &copy; &#169;"  -- renders © © ©
-}
text' : String -> Html
text' =
  unEsc >> text


{-| An alternate name that avoids apostrophes.
-}
txt : String -> Html
txt =
  text'
