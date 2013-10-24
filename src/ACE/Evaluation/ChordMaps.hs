module ACE.Evaluation.ChordMaps ( ChordClass
                                , toChordClass
                                , compareCC
                                , CCEval
                                , toCCEval
                                , toCCEval
                                )where

import ACE.Evaluation.EqIgnore
import HarmTrace.Base.Chord hiding (toMajMin, ClassType (..))
import Data.IntSet (IntSet)

data MajMin = MajClass | MinClass | NoMajMin deriving (Eq, Show)
data Inv    = FstInv | SecInv | NoInv deriving (Eq, Show)
newtype RootPC = RootPC Int deriving (Eq, Show)
data ChordClass = ChordClass RootPC MajMin Sevth Inv
                | NoChordClass
                | IgnoreClass 
                  deriving (Eq, Show)

type CCEval = (EqIgnore, EqIgnore, EqIgnore, EqIgnore, EqIgnore)

toCCEval :: EqIgnore -> CCEval
toCCEval e = (e,e,e,e,e)
                
toChordClass :: ChordLabel -> ChordClass
toChordClass UndefChord = IgnoreClass
toChordClass NoChord    = NoChordClass
toChordClass c = let s = toIntSet c
                     m = toMajMin s
                 in ChordClass (toRootPC c) 
                               m 
                               (toSevth m s) 
                               (toInv m (chordBass c))

toRootPC :: ChordLabel -> RootPC
toRootPC = RootPC . rootPC

toSevth :: MajMin -> IntSet -> Sevth
toSevth m s = case (m, analyseSevth s) of
                (_       , DimSev) -> NoSev -- :dim     unsupported
                (MinClass, MajSev) -> NoSev -- :minmaj7 unsupported
                (_       ,s      ) -> s
                  
toMajMin :: IntSet -> MajMin
toMajMin s = case analyseTriad s of
               MajTriad -> MajClass
               MinTriad -> MinClass
               _        -> NoMajMin -- DimTriad, AugTriad, NoTriad are unsupported

               
toInv :: MajMin -> Interval -> Inv
toInv _        (Note Nat I1) = NoInv
toInv _        (Note Nat I5) = SecInv
toInv MajClass (Note Nat I3) = FstInv
toInv MinClass (Note Fl  I3) = FstInv
toInv _        _             = NoInv  -- other inversions are unsupported
  
  
compareCC :: ChordClass -> ChordClass -> CCEval
compareCC (ChordClass _ NoMajMin _ _) (ChordClass _ _ _ _) 
              = (Ignore, Ignore, Ignore, Ignore, Ignore)
compareCC (ChordClass ra ma sa ia) (ChordClass rb mb sb ib) 
  | ra /= rb  = (NotEq , NotEq , NotEq , NotEq , NotEq)
  | otherwise = let mm = ma ==* mb
                    sm = sa ==* sb
                    im = ia ==* ib
                in ( Equal
                   , mm                -- only major an minor 
                   , mm &&* sm         -- major minor and 
                   , mm &&* im
                   , mm &&* (sm &&* im)
                   )

                   
                   