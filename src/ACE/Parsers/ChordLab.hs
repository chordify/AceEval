{-# OPTIONS_GHC -Wall          #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE Rank2Types #-}
module ACE.Parsers.ChordLab where --  ( parseLabData ) where

import ACE.MIREX.Data
import HarmTrace.Base.Chord
import HarmTrace.Base.Time
import HarmTrace.Base.Parse.General
import HarmTrace.Base.Parse.ChordParser ( pChord )

--------------------------------------------------------------------------------
-- Parse MIREX style chord annotations
--------------------------------------------------------------------------------

pLabMChords :: Team -> Int -> Year -> Collection -> Parser MChords
pLabMChords t i y c = (\x -> MChords c y t i x Nothing) <$> pLabData

pGroundTruth :: MChords -> Parser MChords
pGroundTruth mc = case groundTruth mc of
  Just _gt -> error "pGroundTruth: this MChord allready has a ground truth"
  Nothing  -> (\x -> mc {groundTruth = Just x}) <$> pLabData
                    
-- | Parses a chord annotation.
pLabData :: Parser [Timed ChordLabel]
pLabData =  pListSep_ng pLineEnd pChordLine <* pLineEnd 
         <* (pLineEnd `opt` "\n")

-- | Parses the onset, offset and chordlabel on one line
pChordLine :: Parser (Timed ChordLabel)
pChordLine = timed' <$>  (pSpaceTab *> (pDoubleRaw <*  pSpaceTab))
                    <*>                 pDoubleRaw <*  pSpaceTab 
                    <*>  pChord 

--------------------------------------------------------------------------------
-- General Parsers and Utils
--------------------------------------------------------------------------------

timed' :: NumData -> NumData -> a -> Timed a
timed' on off chrd = Timed chrd [Time on, Time off]

pSpaceTab :: Parser [Char]
pSpaceTab =  pMany (pSym ' ' <|> pSym '\t')
