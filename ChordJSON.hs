{-# OPTIONS_GHC -Wall          #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE Rank2Types #-}
module ChordJSON                     ( pChordJSON )where

import AceMIREX
import HarmTrace.Base.Time           ( Timed (..), BeatTime (..) )
import HarmTrace.Base.Chord          ( ChordLabel )
import HarmTrace.Base.Parse          

--------------------------------------------------------------------------------
-- Parsing ChordJSON into HarmTrace chords
--------------------------------------------------------------------------------

-- | Parses a MIREX results file
pChordJSON :: Year -> Collection -> Parser MChords
pChordJSON y c'' = f <$> pHeader <*> pJSList pChordList <*> pFooter
  where f (s, c, i) [gt,pd] (s', c', i', ["Ground-truth","Prediction"]) 
          | s == s' && c == c' && i == i' && c == c''
                      = MChords c y s i pd (Just gt)
          | otherwise = error "pChordJSON: conflicting meta data"
        f _  _  _     = error "pChordJSON: invalid MIREX Ace data"

-- A header in the form: "var some_name = "
pHeader :: Parser (String, Collection, Int)
pHeader = pVar "data" <?> "header"

-- A footer containing "; \n var another_name = ["some","descriptions"]; 
pFooter :: Parser (String, Collection, Int, [String])
pFooter = g <$> (lexeme (pSym ';') *> pVar "seriesNames") 
            <*>  pJSList pQuotedString
            <*   lexeme (pSym ';')
            <?>  "footer"
            where g (s, c, i) d = (s, c, i, d)

-- | a var statement: "var pmp3chordschordmrx09000087_sfx ="
pVar :: String -> Parser (String, Collection, Int)
-- pVar sfx = f <$> (pString "var "   *> pList1_ng pAscii )
pVar sfx = f <$> (pString "var "   *> pTeam )
             <*> (pString "chords" *> pMaybe (pString "chordmrx09"))
             <*>  pInteger <* pSym '_' <* pString sfx <* (lexeme $ pString " =")
             <?> "var statement"
             where f tm mb s = (tm, maybe Billboard (const Beatles) mb, s)

pTeam :: Parser String
pTeam = snoc <$> pBetween 1 4 pLetter <*> pDigit  <?> "team" where
  snoc l e = l ++ [e]

-- | parsers a single chords list
pChordList :: Parser [Timed ChordLabel]
pChordList = pJSList . pBraces $ pTimedChord

-- {o: 187.13, f: 190.522, l: "D:(9)", a: 0},
pTimedChord :: Parser (Timed ChordLabel)
pTimedChord = timedData <$> (pString "o: "   *> pDoubleRaw)
                        <*> (pString ", f: " *> pDoubleRaw)
                        <*> (pString ", l: \"" *> pChord   )
                        <*   pString "\", a: " <* (pSym '0' <|> pSym '1') 
                        <?> "Onset, offset and chord" where
                        
                        timedData on off c = Timed c [Time on, Time off]

-- replace by listparser?
-- Parsers a typical javascript list: [elem, elem, etc.]
pJSList :: ParserTrafo a [a]
pJSList p = pBrackets . pList1Sep_ng pComma $ p

