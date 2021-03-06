{-# OPTIONS_GHC -Wall          #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE Rank2Types #-}
module ACE.Parsers.ChordLab ( pLabMChords
                            , pGroundTruth
                            , parseChords
                            ) where

import ACE.MIREX.Data
import ACE.Evaluation.ChordEq           ( makeGT )
import HarmTrace.Base.Chord
import HarmTrace.Base.Time
import HarmTrace.Base.Parse.General
import HarmTrace.Base.Parse.ChordParser ( pChord )
import Data.List                        ( intercalate )

--------------------------------------------------------------------------------
-- Parse MIREX style chord annotations
--------------------------------------------------------------------------------

parseChords :: Parser MChords -> String -> MChords
parseChords pf txt = case parseDataWithErrors pf txt of
       (mc, []) -> mc
       (_ , e ) -> error ("parse errors:\n" ++(intercalate "\n" . map show $ e))

pLabMChords :: Team -> Int -> Maybe Year -> Maybe Collection -> Parser MChords
pLabMChords t i y c = (\x -> MChords (maybe Unkown id c)
                                     (maybe Other id y)
                                     t i x Nothing)       <$> pLabData

pGroundTruth :: MChords -> Parser MChords
pGroundTruth mc = case groundTruth mc of
  Just _gt -> error "pGroundTruth: this MChord allready has a ground truth"
  Nothing  -> (\x -> mc {groundTruth = Just (makeGT x)}) <$> pLabData

-- | Parses a chord annotation.
pLabData :: Parser [Timed ChordLabel]
pLabData =  pListSep_ng pLineEnd pChordLine <* pLineEnd
         <* (pLineEnd `opt` "\n")

-- | Parses the onset, offset and chordlabel on one line
pChordLine :: Parser (Timed ChordLabel)
pChordLine =  f <$>  (pSpaceTab *> (pDoubleRaw <*  pSpaceTab))
                <*>                 pDoubleRaw <*  pSpaceTab
                <*>  pChord
  where f x y z = timed z x y
--------------------------------------------------------------------------------
-- General Parsers and Utils
--------------------------------------------------------------------------------

pSpaceTab :: Parser [Char]
pSpaceTab =  pMany (pSym ' ' <|> pSym '\t')
