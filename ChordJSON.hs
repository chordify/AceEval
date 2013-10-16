{-# OPTIONS_GHC -Wall          #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE Rank2Types #-}
module ChordJSON                     ( pChordJSON )where

import AceMIREX
import HarmTrace.Base.Time           ( Timed (..), BeatTime (..) )
import HarmTrace.Base.Chord          
import HarmTrace.Base.Parse          

--------------------------------------------------------------------------------
-- Parsing ChordJSON into HarmTrace chords
--------------------------------------------------------------------------------

-- | Parses a MIREX results file
pChordJSON :: Year -> Collection -> Parser MChords
pChordJSON y c'' = f <$> pHeader <*> pChordsPair <*> pFooter
  where f (s, c, i) (gt,pd) (s', c', i', ["Ground-truth","Prediction"]) 
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

pChordsPair :: Parser ([Timed ChordLabel],[Timed ChordLabel])
pChordsPair = pBrackets ((,) <$> pChordList True <* pComma -- groundTruth
                             <*> pChordList False )        -- prediction
  
-- | parsers a single chords list
pChordList :: Bool -> Parser [Timed ChordLabel]
pChordList isGT = pJSList . pBraces $ pTimedChord isGT

-- {o: 187.13, f: 190.522, l: "D:(9)", a: 0},
pTimedChord :: Bool -> Parser (Timed ChordLabel)
pTimedChord isGT = timedData <$> (pString "o: "     *> pDoubleRaw)
                             <*> (pString ", f: "   *> pDoubleRaw)
                             <*> (pString ", l: \"" *> pMIREXChord isGT )
                             <*   pString "\", a: " <* (pSym '0' <|> pSym '1') 
                             <?> "Onset, offset and chord" where
                             
                             timedData on off c = Timed c [Time on, Time off]

                        
                        
pMIREXChord :: Bool -> Parser ChordLabel
pMIREXChord isGT = fixChord <$> pChord where
  
  {- Fixes a bug in the NEMA framework. 
     As Johan pauwels explains in an email (10/10/2013): 
     I'm pretty sure there is a bug somewhere in the NEMA framework that somehow
     converts some "N" symbols to "F#:7sus4", just by switching representations.
     I don't know when exactly it happens (obviously it doesn't do it for all 
     "N"s), but it has been there since forever (aka 2010). 

     Actually, that conversion script has been virtually unchanged since 2010 
     (it has been coded in an Utrecht auditorium). Back then, it was an easy and
     safe workaround because I knew that none of the estimation algorithms could
     even generate 7sus4 chords. Only the ground-truth had proper 7sus4 chords 
     in it, but luckily not the conversion of N symbols, so that's why there's 
     that if clause. I haven't verified with the new algorithms, but I suspect 
     that they still cannot generate 7sus4, so the assumption probably still 
     holds and this conversion can still be done safely. It only didn't work 
     for that famous example were Bristol just outputted the exact annotations 
     after some sort of elementary fingerprinting, but I took care of that 
     manually (and that result was useless anyway). -}
  
    fixChord :: ChordLabel -> ChordLabel
    fixChord c@(Chord (Note Sh F) SevSus4 _ _) | isGT      = c
                                               | otherwise = NoChord
    fixChord c                                             = c
                        
-- replace by listparser?
-- Parsers a typical javascript list: [elem, elem, etc.]
pJSList :: ParserTrafo a [a]
pJSList p = pBrackets . pList1Sep_ng pComma $ p

