module ACE.Evaluation.ChordMaps where

import HarmTrace.Base.Chord hiding (toMajMin, ClassType (..))
import Data.IntSet (IntSet)

data MajMin = MajClass | MinClass | NoMajMin 
data Inv    = FstInv | SecInv | NoInv
-- newtype RootPC = RootPC Int
data ChordClass = ChordClass MajMin Sevth Inv

toChordClass :: ChordLabel -> ChordClass
toChordClass = undefined

-- toRootPC :: ChordLabel -> RootPC
-- toRootPC = RootPC . rootPC

toSevth :: IntSet -> Sevth
toSevth s = case analyseSevth s of
              DimSev -> NoSev -- unsupported
              s      -> s
                  
toMajMin :: IntSet -> MajMin
toMajMin s = case analyseTriad s of
               MajTriad -> MajClass
               MinTriad -> MinClass
               NoTriad  -> NoMajMin
               DimTriad -> MajClass -- unsupported
               AugTriad -> MinClass
               
toInv :: MajMin -> Interval -> Inv
toInv _        (Note Nat I1) = NoInv
toInv _        (Note Nat I5) = SecInv
toInv MajClass (Note Nat I3) = FstInv
toInv MinClass (Note Fl  I3) = FstInv
toInv _        _             = NoInv  -- other inversions are unsupported