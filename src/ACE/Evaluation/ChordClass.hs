{-# LANGUAGE DeriveFunctor        #-}
module ACE.Evaluation.ChordClass ( ChordClass (..)
                                 , toChordClass
                                 , compareCC
                                 , CCEval (..)
                                 , toCCEval
                                 , sequenceCCEval
                                 , toRootPC
                                 , toSevth
                                 , RootPC (..)
                                 , fromRootPC
                                 , fromIntRootPC
                                 , fromStringRootPC
                                 , testCC
                                 , toCCRoots
                                 , toCCMajMins
                                 , fromIntRootPCs
                                 , toChordClass
                                 , fromStringRootPCs
                                 , toChordClasses
                                 , intPCtoChordLabel
                                 , rootPCwithN
                                 , mchordsToInt
                                 )where

import ACE.Evaluation.EqIgnore
import HarmTrace.Base.Chord hiding ( toMajMin, ClassType (..), Sev )
import Data.IntSet                 ( IntSet )
import Data.List                   ( intercalate )
import HarmTrace.Base.Parse.ChordParser
import HarmTrace.Base.Parse.General 
import ACE.MIREX.Data 
import HarmTrace.Base.Time  

data MajMin = MajClass | MinClass | NoMajMin deriving (Eq, Show)
data Inv    = FstInv | SecInv | NoInv | OtherBass deriving (Eq, Show)
newtype RootPC = RootPC Int deriving (Eq, Show)
data ChordClass = ChordClass RootPC MajMin Sevth Inv  deriving (Eq, Show)

data CCEval a = CCEval { eRoot      :: a  -- root
                       , eMajMin    :: a  -- majmin
                       , eSevth     :: a  -- seventh
                       , eMajMinInv :: a  -- majmin inv
                       , eSevthInv  :: a  -- seventh inv
                       } deriving (Eq, Functor)
                       
instance Show a => Show (CCEval a) where
  show (CCEval r m s im is) = intercalate "," . map show $ [r,m,s,im,is] 

-- Create different ChordLabels from a ChordClass
--toMajMinCL :: ChordClass -> ChordLabel
--toMajMinCL cc = 

--toSevthCL :: ChordClass -> ChordLabel

--toInvCL :: ChordClass -> ChordLabel

testCC = ChordClass (RootPC 0) NoMajMin NoSev NoInv

mchordsToInt :: MChords -> [Int]
mchordsToInt = (map rootPCwithN) . dropTimed . chords

-- same as rootPC, but NoChords become -1
rootPCwithN :: ChordLabel -> Int
rootPCwithN NoChord = -1
rootPCwithN c       = rootPC c

intPCtoChordLabel :: Int -> ChordLabel
intPCtoChordLabel (-1) = NoChord
intPCtoChordLabel   i  = Chord (pcToRoot i) None [] (Note Nat I1)

-- make different versions of the chords for evaluation
toCCRoots :: [ChordClass] -> [Int]
toCCRoots = map toCCRoot

toCCRoot :: ChordClass -> Int
toCCRoot = rts where 
  rts (ChordClass (RootPC r) m s i) = r

toCCMajMins :: [ChordClass] -> [ChordLabel]
toCCMajMins = map toCCMajMin

toCCMajMin :: ChordClass -> ChordLabel
toCCMajMin (ChordClass (RootPC r) m s i) = chord where 
  chord = Chord root (toShortHand m) [] (Note Nat I1)
  root = pcToRoot r

toShortHand :: MajMin -> Shorthand
toShortHand MajClass = Maj
toShortHand MinClass = Min
toShortHand NoMajMin = None

fromStringRootPCs :: [String] -> [ChordClass]
fromStringRootPCs = map fromStringRootPC

fromStringRootPC :: String -> ChordClass
fromStringRootPC i = ChordClass (RootPC r) NoMajMin NoSev NoInv where
  r = (read i :: Int )

fromIntRootPCs :: [Int] -> [ChordClass]
fromIntRootPCs = map fromIntRootPC

fromIntRootPC :: Int -> ChordClass
fromIntRootPC i = ChordClass (RootPC i) NoMajMin NoSev NoInv

fromRootPC :: RootPC -> ChordClass
fromRootPC r = ChordClass r NoMajMin NoSev NoInv

toCCEval :: a -> CCEval a
toCCEval e = CCEval e e e e e
                
sequenceCCEval :: [CCEval a] -> CCEval [a]
sequenceCCEval = foldr step (CCEval [] [] [] [] []) where
  step :: CCEval b -> CCEval [b] -> CCEval [b] 
  step (CCEval r m s im is) (CCEval rs ms ss ims iss) 
    = CCEval (r:rs) (m:ms) (s:ss) (im:ims) (is:iss)

toChordClasses :: [String] -> [ChordClass]
toChordClasses = map (toChordClass . parseData pChord)
                
toChordClass :: ChordLabel -> ChordClass
toChordClass UndefChord = error "cannot create ChordClass for UndefChord"
toChordClass NoChord    = error "cannot create ChordClass for NoChord"
toChordClass c = let s = toIntSet c
                     m = toMajMin s
                 in ChordClass (toRootPC c) 
                               m 
                               (toSevth m s) 
                               (toInv m (chordBass c))

sintPCtoChordLabel :: String -> ChordLabel
sintPCtoChordLabel i = Chord (pcToRoot s) None [] (Note Nat I1) where
  s = (read i :: Int)

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
    in CCEval   rt
              ( rt &&*  mm                  ) -- only major an minor 
              ( rt &&* (mm &&*  sm)         ) -- major minor and 
              ( rt &&* (mm &&*  im)         ) 
              ( rt &&* (mm &&* (sm &&* im)) ) 
                   
                   