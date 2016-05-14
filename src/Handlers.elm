module Handlers exposing (handleAllEscapes)

import Char exposing (fromCode, isUpper, toCode)
import NamedEscapes exposing (getNamedEscape, getSubmatch)
import Regex exposing (Match, regex, replace)
import String exposing (foldl, fromChar, toUpper, toInt)


handleAllEscapes : String -> String
handleAllEscapes =
  handleHexEscapes >> handleDecEscapes >> handleNamedEscapes


handleHexEscapes : String -> String
handleHexEscapes =
  handleEscapes "&#x([0-9a-fA-F]+);" (hexToInt >> Just)


handleEscapes : String -> (String -> Maybe Int) -> String -> String
handleEscapes regEx escSequenceToCode =
  replace Regex.All (regex regEx) (handleEscape escSequenceToCode)


handleEscape : (String -> Maybe Int) -> Match -> String
handleEscape submatchToCode m =
  let
    codeToGlyph =
      Maybe.map (fromCode >> fromChar)
  in
    Maybe.withDefault m.match <| codeToGlyph <| submatchToCode <| getSubmatch m


{-| Error checking superfluous since all strings passed in match \[0-9a-fA-F]+\.
-}
hexToInt : String -> Int
hexToInt =
  let
    charToInt c =
      if isUpper c then
        toCode c - 55
      else
        toCode c - 48
  in
    toUpper >> foldl (\c int -> int * 16 + charToInt c) 0


handleDecEscapes : String -> String
handleDecEscapes =
  handleEscapes "&#([0-9]+);" (toInt >> Result.toMaybe)


handleNamedEscapes : String -> String
handleNamedEscapes =
  handleEscapes "&([0-9a-zA-Z]+);" getNamedEscape
