{-# LANGUAGE DeriveFunctor        #-}
module ACE.Evaluation.ChordClass ( ChordClass
                                 , toChordClass
                                 , compareCC
                                 , CCEval (..)
                                 , toCCEval
                                 , unzipCCEval
                                 )where

import ACE.Evaluation.EqIgnore
import HarmTrace.Base.Chord hiding ( toMajMin, ClassType (..) )
import Data.IntSet                 ( IntSet )
import Data.List                   ( intercalate )

data MajMin = MajClass | MinClass | NoMajMin deriving (Eq, Show)
data Inv    = FstInv | SecInv | NoInv deriving (Eq, Show)
newtype RootPC = RootPC Int deriving (Eq, Show)
data ChordClass = ChordClass RootPC MajMin Sevth Inv  deriving (Eq, Show)

data CCEval a = CCEval a  -- root
                       a  -- majmin
                       a  -- seventh
                       a  -- majmin inv
                       a  -- seventh inv
                       deriving (Eq, Functor)
                       
instance Show a => Show (CCEval a) where
  show (CCEval r m s im is) = intercalate "," . map show $ [r,m,s,im,is] 

toCCEval :: a -> CCEval a
toCCEval e = CCEval e e e e e
                
unzipCCEval :: [CCEval a] -> CCEval [a]
unzipCCEval = foldr step (CCEval [] [] [] [] []) where
  step :: CCEval b -> CCEval [b] -> CCEval [b] 
  step (CCEval r m s im is) (CCEval rs ms ss ims iss) 
    = CCEval (r:rs) (m:ms) (s:ss) (im:ims) (is:iss)
                
toChordClass :: ChordLabel -> ChordClass
toChordClass UndefChord = error "cannot create ChordClass for UndefChord"
toChordClass NoChord    = error "cannot create ChordClass for NoChord"
toChordClass c = let s = toIntSet c
                     m = toMajMin s
                 in ChordClass (toRootPC c) 
                               m 
                               (toSevth m s) 
                               (toInv m (chordBass c))

toRootPC :: ChordLabel -> RootPC
toRootPC = RootPC . rootPC

toSevth :: MajMin -> IntSet -> Sevth
toSevth m is = case (m, analyseSevth is) of
                (_       , DimSev) -> NoSev -- :dim     unsupported
                (MinClass, MajSev) -> NoSev -- :minmaj7 unsupported
                (_       ,s      ) -> s
                  
-- | We require the third and the fifth to be present. Hence, @D:maj(*5)@ will
-- yield 'NoMajMin'
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
  

compareCC :: ChordClass -> ChordClass -> CCEval EqIgnore
-- compareCC (ChordClass _ NoMajMin _ _) (ChordClass _ _ _ _)  = toCCEval Ignore
compareCC (ChordClass ra ma sa ia) (ChordClass rb mb sb ib) 
  | ra /= rb  = toCCEval NotEq
  | otherwise = let mm = case (ma,mb) of 
                           (NoMajMin, _) -> Ignore
                           (_, NoMajMin) -> Ignore
                           _             -> ma ==* mb
                    sm = sa ==* sb
                    im = ia ==* ib
                in CCEval Equal
                   ( mm                 ) -- only major an minor 
                   ( mm &&*  sm         ) -- major minor and 
                   ( mm &&*  im         ) 
                   ( mm &&* (sm &&* im) ) 
                   

                   
                   