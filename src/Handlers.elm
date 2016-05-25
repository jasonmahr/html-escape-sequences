module Handlers exposing (handleEscapes)

import Char exposing (toCode)
import NamedEscapes exposing (codeToStr, getNamedEscape, subm, regexAll)
import Regex exposing (replace)
import String exposing (foldl, toLower, toInt)


handleEscapes : String -> String
handleEscapes =
    flip (List.foldl (uncurry handleMatches)) escapeTypes


handleMatches : String -> (String -> Maybe Int) -> String -> String
handleMatches regEx =
    let
        handleMatch sToCode m =
            Maybe.withDefault m.match <| Maybe.map codeToStr <| sToCode (subm m)
    in
        regexAll replace regEx << handleMatch


{-| Escape types: hexadecimal, decimal, and named escapes.
-}
escapeTypes : List ( String, String -> Maybe Int )
escapeTypes =
    [ ( "&#x([0-9a-fA-F]+);", Just << hexToInt )
    , ( "&#([0-9]+);", Result.toMaybe << toInt )
    , ( "&([0-9a-zA-Z]+);", getNamedEscape )
    ]


hexToInt : String -> Int
hexToInt =
    foldl (\hexDigit int -> int * 16 + toCode hexDigit % 39 - 9) 0 << toLower
