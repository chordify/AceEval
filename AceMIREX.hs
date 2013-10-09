module AceMIREX  ( Collection (..)
                 , Year (..)
                 , Team
                 , MChords (..)
                 ) where
                 
import HarmTrace.Base.Time           ( Timed )
import HarmTrace.Base.Chord          ( ChordLabel )
import Data.List                     ( intercalate )

data Collection = Billboard | Beatles   deriving (Show, Eq)
data Year       = Y2010 | Y2011 | Y2012
type Team       = String

instance Show Year where
  show Y2010 = "2010"
  show Y2011 = "2011"
  show Y2012 = "2012"
  
data MChords    = MChords { collection  :: Collection
                          , year        :: Year
                          , team        :: String
                          , songID      :: Int
                          , chords      :: [Timed ChordLabel]
                          , groundTruth :: Maybe [Timed ChordLabel]
                          }  
                          
instance Show MChords where
  show (MChords c y t i _cs _mgt) = intercalate " " [show c, show y, t, show i]
