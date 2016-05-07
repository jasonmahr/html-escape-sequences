module Handlers (handleEscapes) where

{-| The handleEscapes function used in Unicode.elm is defined here.
-}

import Char exposing (fromCode)
import Dict exposing (foldl)
import NonDigitEscapes exposing (nonDigitEscapes)
import ParseInt exposing (parseIntHex)
import Regex exposing (Match, regex, replace)
import String exposing (fromChar, join, slice, split, toInt)


handleEscapes : String -> String
handleEscapes =
  handleNonDigitEscapes >> handleHexEscapes >> handleDecEscapes


handleNonDigitEscapes : String -> String
handleNonDigitEscapes str =
  Dict.foldl (\esc glyph s -> join glyph <| split esc s) str nonDigitEscapes


handleHexEscapes : String -> String
handleHexEscapes =
  replace Regex.All (regex "&#x[0-9a-fA-F]*;") handleHexEscape


handleHexEscape : Match -> String
handleHexEscape m =
  let
    hexEsc =
      m.match

    dec =
      Result.toMaybe <| parseIntHex <| slice 3 -1 hexEsc
  in
    case dec of
      Just code ->
        fromChar (fromCode code)

      Nothing ->
        hexEsc


handleDecEscapes : String -> String
handleDecEscapes =
  replace Regex.All (regex "&#[0-9]*;") handleDecEscape


handleDecEscape : Match -> String
handleDecEscape m =
  let
    decEsc =
      m.match

    dec =
      Result.toMaybe <| toInt <| slice 2 -1 <| decEsc
  in
    case dec of
      Just code ->
        fromChar (fromCode code)

      Nothing ->
        decEsc
