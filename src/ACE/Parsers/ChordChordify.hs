{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall         #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  ChordSeqParser
-- Copyright   :  (c) 2012-2016 Chordify B.V., Groningen
-- License     :  LGPL-3
--
-- Maintainer  :  dreixel@chordify.net, bas@chordify.net
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Parses a text representation of chords as stored in the
-- Chordify database
--
--------------------------------------------------------------------------------
module ACE.Parsers.ChordChordify where

import HarmTrace.Base.Parse
import HarmTrace.Base.Chord.Datatypes
import HarmTrace.Base.Time

--------------------------------------------------------------------------------
-- Parse MIREX style chord annotations
--------------------------------------------------------------------------------

-- | Parses a Chordify chord annotation
parseBillboard :: String -> ([Timed ChordLabel], [Error LineColPos])
parseBillboard = parseDataWithErrors parseAnnotationData

-- | Parses a chord annotation.
parseAnnotationData :: Parser [Timed ChordLabel]
parseAnnotationData =  pListSep_ng pLineEnd pChordSegment
                    <*  (pLineEnd
                    <|> (pLineEnd <* pLineEnd))

-- | Parses the onset, offset and chordlabel on one line
pChordSegment :: Parser (Timed ChordLabel)
pChordSegment = timedData' <$> pBeat    <* pSym ';'
                           <*> pChord   <* pSym ';'
                           <*> pNumData <* pSym ';'
                           <*> pNumData where

  -- | convenient constructor for a 'Timed'
  timedData' :: Beat -> ChordLabel -> NumData -> NumData -> Timed ChordLabel
  timedData' b c on off = Timed c [BeatTime on b, BeatTime off (nextBeat Duple b)]


-- TODO : it would not hurt to move the functions below to HarmTrace-Base
-- because they are very general
-- | Parses a 'Beat'.
pBeat :: Parser Beat
pBeat = One   <$ pSym '1'
    <|> Two   <$ pSym '2'
    <|> Three <$ pSym '3'
    <|> Four  <$ pSym '4'
    <?> "Beat"

-- Parses a time stamp
pNumData :: Parser NumData
pNumData = pDoubleRaw
