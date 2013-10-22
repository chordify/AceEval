module PreProcessing ( Edit 
                     , PPLog
                     , postProcess
                     ) where

import AceMIREX
import HarmTrace.Base.Time   ( Timed (..), BeatTime (..), splitTimed
                             , offset, onset, duration, timed )
import HarmTrace.Base.Chord  ( ChordLabel, Chord (..) )
import Control.Monad         ( liftM  )
import Control.Monad.State   ( State, modify, runState )
import Data.List             ( partition, intercalate )
import Data.Maybe            ( fromJust )
import Data.Foldable         ( foldrM )
                     
data Edit = Fill      ChordLabel 
          | AddUnk    ChordLabel
          | FillStart ChordLabel
          | Zero      ChordLabel 
          | Rem       ChordLabel 
          | RemEnd    ChordLabel
data Source = Gt | Pred

instance Show Source where
  show Gt   = " (ground-truth)"
  show Pred = ""
  
instance Show Edit where
  show (Fill      c) = "hole in " ++ show c ++ " "
  show (Zero      c) = "Zero length segment for "  ++ show c ++ " "
  show (Rem       c) = "Removed overlapping chord " ++ show c ++ " "
  show (AddUnk    c) = "Extended the duration "           ++ show c ++ " "
  show (RemEnd    c) = "Removed final chord "             ++ show c ++ " "
  show (FillStart c) = "Reset the start position to 0.0 " ++ show c ++ " "

-- | Pre-Processing logging: a data type that stores all kinds of information
-- to trace back the performed edit operation
data PPLog = PPLog Edit Collection Year String Int Source Double Double 
                       
instance Show PPLog where
  show (PPLog e c y t i src on off) = 
    (show e ++ intercalate " " [show c, show y, t, show i] 
            ++ show src ++ ": " ++ show on ++ " - " ++ show off)

  showList l s = s ++ intercalate "\n" (map show l)
            
fromMChords :: (ChordLabel -> Edit) -> Source -> MChords -> Timed ChordLabel 
            -> PPLog
fromMChords e s mc c = PPLog (e . getData $ c) (collection mc) (year mc) 
                               (team mc) (songID mc)  s (onset c) (offset c)
                               
postProcess :: MChords -> (MChords, [PPLog])
postProcess m = runState (postProcess' m) []
                               
postProcess' :: MChords -> State [PPLog] MChords
postProcess' mc = do c  <- process Pred . chords $ mc
                     gt <- maybeState (process Gt) . groundTruth $ mc
                     return mc { chords = c, groundTruth = gt } where
  
  process :: Source -> [Timed ChordLabel] -> State [PPLog] [Timed ChordLabel]
  process s cs = fill s cs >>= 
                 filterZeroLen s >>= 
                 fixStart s >>= 
                 fixEnd s 
  
  fill :: Source -> [Timed ChordLabel] -> State [PPLog] [Timed ChordLabel]
  fill s cs = do foldrM step [] cs >>= return where

    step :: Timed ChordLabel ->[Timed ChordLabel] -> State [PPLog] [Timed ChordLabel]
    step a []     = return [a]
    step a (b:ts) 
      | off == on =                    return (a        : b : ts)
        -- two segments are overlapping
      | off >  on = modify (logR :) >> return (a'       : b : ts)
        -- there is a "hole", an unmarked space, between two segments
      | otherwise = modify (logH :) >> return (a : hole : b : ts) -- off < on
                       
           where off  = offset a
                 on   = onset  b 
                 hole = timed NoChord off on
                 logH = fromMChords Fill s mc hole
                 a'   = timed (getData a) (onset a) on -- reset a's offset 
                 logR = fromMChords Rem s mc (timed (getData a) on (offset a))
                 
                 
  filterZeroLen :: Source -> [Timed ChordLabel] -> State [PPLog] [Timed ChordLabel] 
  filterZeroLen s td = do let (zero, good) = partition (\x -> duration x == 0) td
                          modify ((map (fromMChords Zero s mc) zero) ++)
                          return good 

  fixEnd :: Source -> [Timed ChordLabel] -> State [PPLog] [Timed ChordLabel]
  fixEnd Gt   d = return d -- don't fix any thing when processing ground-truth
  fixEnd Pred d = 
    let off = getEndTime . fromJust . groundTruth $ mc
    in case span (\x -> offset x < off) d of
         (l,[ ]) -> do --let add = timed UndefChord (getEndTime l) off
                       let add = timed NoChord (getEndTime l) off
                       modify (fromMChords AddUnk Pred mc add :)
                       return (l ++ [add])
         (l,h:t) -> do let (end, r) = splitTimed h off
                       modify (map (fromMChords RemEnd Pred mc) (r:t) ++)
                       return (l ++ [end])

  -- | Checks whether the first Timed element have the same onset, and applies
  -- chrossSegment.
  fixStart :: Source -> [Timed ChordLabel] -> State [PPLog] [Timed ChordLabel]
  fixStart _ [] = return []
  fixStart s d
    | on == 0.0 = return d
    | otherwise = do modify (fromMChords FillStart s mc hole :)
                     return (hole : d)
        
        where  on   = onset (head d)
               hole = timed (toChord s) 0.0 on 
                
               toChord Pred = NoChord -- UndefChord
               toChord Gt   = NoChord
             
                       
getEndTime :: [Timed a] -> Double
getEndTime = offset . last
  
maybeState :: (a -> State b c) -> Maybe a -> State b (Maybe c)
maybeState f ma = case ma of Just a  -> f a >>= return . Just
                             Nothing -> return Nothing