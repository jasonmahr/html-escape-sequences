module Handlers (handleAllEscapes) where

import Char exposing (fromCode, isUpper, toCode)
import NamedEscapes exposing (getNamedEscape, getSubmatch)
import Regex exposing (Match, regex, replace)
import String exposing (foldl, fromChar, toUpper, toInt)


handleAllEscapes : String -> String
handleAllEscapes =
  handleHexEscapes >> handleDecEscapes >> handleNamedEscapes


handleHexEscapes : String -> String
handleHexEscapes =
  handleMatches "&#x([0-9a-fA-F]+);" (hexToInt >> Just)


handleMatches : String -> (String -> Maybe Int) -> String -> String
handleMatches regEx escToCode =
  replace Regex.All (regex regEx) (handleMatch escToCode)


handleMatch : (String -> Maybe Int) -> Match -> String
handleMatch submatchToCode m =
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
  handleMatches "&#([0-9]+);" (toInt >> Result.toMaybe)


handleNamedEscapes : String -> String
handleNamedEscapes =
  handleMatches "&([0-9a-zA-Z]+);" getNamedEscape
