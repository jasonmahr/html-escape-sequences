module Handlers (handleAllEscapes) where

{-| The handleAllEscapes function used in Unicode.elm is defined here.
-}

import Char exposing (fromCode, isUpper, toCode)
import NonDigitEscapes exposing (getNonDigitEscape)
import Regex exposing (Match, regex, replace)
import String exposing (fromChar, toUpper, slice, toInt)


handleAllEscapes : String -> String
handleAllEscapes =
  handleHexEscapes >> handleDecEscapes >> handleNonDigitEscapes


handleHexEscapes : String -> String
handleHexEscapes =
  handleEscapes "&#x[0-9a-fA-F]*;" 3 (toUpper >> hexToInt >> Maybe.Just)


handleEscapes : String -> Int -> (String -> Maybe Int) -> String -> String
handleEscapes str startInd escToCode =
  replace Regex.All (regex str) <| handleEscape (slice startInd -1 >> escToCode)


handleEscape : (String -> Maybe Int) -> Match -> String
handleEscape toCode m =
  Maybe.withDefault m.match <| Maybe.map (fromCode >> fromChar) (toCode m.match)


{-| Error checking superfluous since all strings passed in match \[0-9A-F]*\.
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
    String.foldl (\c int -> int * 16 + charToInt c) 0


handleDecEscapes : String -> String
handleDecEscapes =
  handleEscapes "&#[0-9]*;" 2 (toInt >> Result.toMaybe)


handleNonDigitEscapes : String -> String
handleNonDigitEscapes =
  handleEscapes "&[a-zA-Z]*;" 1 getNonDigitEscape
