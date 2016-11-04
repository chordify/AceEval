{-# LANGUAGE DeriveFunctor        #-}
module ACE.Evaluation.ChordClass ( ChordClass
                                 , toChordClass
                                 , compareCC
                                 , CCEval (..)
                                 , toCCEval
                                 , sequenceCCEval
                                 )where

import ACE.Evaluation.EqIgnore
import HarmTrace.Base.Chord hiding ( toMajMin, ClassType (..), Sev )
import Data.IntSet                 ( IntSet )
import Data.List                   ( intercalate )

data MajMin = MajClass | MinClass | NoMajMin deriving (Eq, Show)
data Inv    = FstInv | SecInv | NoInv | OtherBass deriving (Eq, Show)
newtype RootPC = RootPC Int deriving (Eq, Show)
data ChordClass = ChordClass RootPC MajMin Sevth Inv  deriving (Eq, Show)

data CCEval a = CCEval { eIsChord   :: a  -- chord/nochord eval
                       , eRoot      :: a  -- root
                       , eMajMin    :: a  -- majmin
                       , eSevth     :: a  -- seventh
                       , eMajMinInv :: a  -- majmin inv
                       , eSevthInv  :: a  -- seventh inv
                       } deriving (Eq, Functor)
                       
instance Show a => Show (CCEval a) where
  show (CCEval _ r m s im is) = intercalate "," . map show $ [r,m,s,im,is] 

toCCEval :: a -> CCEval a
toCCEval e = CCEval e e e e e e
                
sequenceCCEval :: [CCEval a] -> CCEval [a]
sequenceCCEval = foldr step (CCEval [] [] [] [] [] []) where
  step :: CCEval b -> CCEval [b] -> CCEval [b] 
  step (CCEval c r m s im is) (CCEval cs rs ms ss ims iss) 
    = CCEval (c:cs) (r:rs) (m:ms) (s:ss) (im:ims) (is:iss)
                
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
             -- We use the DimSev as a unsupported value
             --   (_       , DimSev) -> NoSev -- :dim     unsupported
                (MinClass, MajSev) -> DimSev -- :minmaj7 unsupported
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
toInv _        _             = OtherBass  -- other inversions are unsupported
  
compareCC :: ChordClass -> ChordClass -> CCEval EqIgnore
compareCC (ChordClass rgt mgt sgt igt) (ChordClass rp mp sp ip) 
  = let rt = rgt ==* rp
        mm = case (mgt,mp) of (NoMajMin, _) -> Ignore
                              (_, NoMajMin) -> NotEq
                              _             -> mgt ==* mp
        sm = case (sgt,sp) of (DimSev, _  ) -> Ignore
                              (_ , DimSev ) -> NotEq
                              _               -> sgt ==* sp
        im = case (igt,ip) of (OtherBass, _ ) -> Ignore
                              (_ ,OtherBass ) -> NotEq
                              _               -> igt ==* ip
    in CCEval Equal
                rt
              ( rt &&*  mm                  ) -- only major an minor 
              ( rt &&* (mm &&*  sm)         ) -- major minor and 
              ( rt &&* (mm &&*  im)         ) 
              ( rt &&* (mm &&* (sm &&* im)) ) 
                   
                   
