module ACE.Evaluation.ChordMaps where

import HarmTrace.Base.Chord hiding (toMajMin, ClassType (..))
import Data.IntSet (IntSet)

data MajMin = MajClass | MinClass | NoMajMin 
data Inv    = FstInv | SecInv
newtype RootPC = RootPC Int
data ChordClass = ChordClass RootPC MajMin Sevth Inv

toChordClass :: ChordLabel -> ChordClass
toChordClass = undefined

toRootPC :: ChordLabel -> RootPC
toRootPC = RootPC . rootPC

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
               