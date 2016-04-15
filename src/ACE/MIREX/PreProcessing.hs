module ACE.MIREX.PreProcessing ( Edit
                               , PPLog
                               , preProcess
                               ) where

import ACE.MIREX.Data
import ACE.Evaluation.ChordEq  ( refLab, makeGT )

import HarmTrace.Base.Time     ( Timed, Timed' (..), splitTimed, getEndTime
                               , offset, onset, duration, timed, concatTimed )
import HarmTrace.Base.Chord    ( ChordLabel, Chord (..) )
import Control.Monad.State     ( State, modify, runState )
import Data.List               ( partition, intercalate )
import Data.Maybe              ( fromJust )
import Data.Foldable           ( foldrM )

--------------------------------------------------------------------------------
-- Error messages
--------------------------------------------------------------------------------

-- | The different edit operations that can be performed and logged during the
-- pre-processing of the 'ChordLabel' sequences.
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
data PPLog = PPLog Edit Collection Year String Int Source Float Float

instance Show PPLog where
  show (PPLog e c y t i src on off) =
    show e ++ unwords [show c, show y, t, show i]
           ++ show src ++ ": " ++ show on ++ " - " ++ show off

  showList l s = s ++ intercalate "\n" (map show l)

-- | Constructs a 'PPLog' pre-processing log
fromMChords :: (ChordLabel -> Edit) -> Source -> MChords -> Timed ChordLabel
            -> PPLog
fromMChords e s mc c = PPLog (e . getData $ c) (collection mc) (year mc)
                               (team mc) (songID mc)  s (onset c) (offset c)

--------------------------------------------------------------------------------
-- Pre-processing
--------------------------------------------------------------------------------

-- | Pre-processes an 'MChords' and returns the updated 'MChords' together with
-- a list of logged 'PPLog' 'Edit' operations.
preProcess :: MChords -> (MChords, [PPLog])
preProcess m = runState (preProcess' m) []

-- applies 'process' to both the predicted as well as the ground truth
-- chord sequence, if any.
preProcess' :: MChords -> State [PPLog] MChords
preProcess' mc = do c  <- process Pred . chords $ mc
                    gt <- maybeState ( fmap makeGT . process Gt . map (fmap refLab))
                                     . groundTruth $ mc
                    return mc { chords = c, groundTruth = gt } where

  -- Performs a series of pre-processing operations
  process :: Source -> [Timed ChordLabel] -> State [PPLog] [Timed ChordLabel]
  process s cs = filterZeroLen s cs >>= fill s >>= fixStart s >>= fixEnd s
                                    >>= return . reduceTimed

  -- fill "holes" in a chord sequence
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

  -- remove chord segments that have a length smaller than 0
  filterZeroLen :: Source -> [Timed ChordLabel] -> State [PPLog] [Timed ChordLabel]
  filterZeroLen s td = do let (zero, good) = partition (\x -> duration x <= 0) td
                          modify ((map (fromMChords Zero s mc) zero) ++)
                          return good

  -- synchronises the last segment(s) of the prediction with the groundtruth
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

-- | Returns the reduced chord sequences, where repeated chords are merged
-- into one 'ProbChord', wrapped in a 'Timed' type.
reduceTimed :: Eq a => [Timed a] -> [Timed a]
reduceTimed = foldr group [] where

   group :: Eq a => Timed a -> [Timed a] -> [Timed a]
   group c [] = [c]
   -- group tc@(Timed c tsc ) (th@(Timed h tsh ) : t)
   group c (h : t)
     | getData c == getData h = concatTimed (getData h) c   h : t
     | otherwise              =                         c : h : t

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

maybeState :: (a -> State b c) -> Maybe a -> State b (Maybe c)
maybeState f ma = case ma of Just a  -> f a >>= return . Just
                             Nothing -> return Nothing
