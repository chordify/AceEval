module ACE.Evaluation.ResultsPrinter where

import ACE.Evaluation.Segmentation   ( SegEval  )
import ACE.Evaluation.ChordClass     ( CCEval   )
import ACE.Evaluation.EqIgnore       ( EqIgnore )

import Data.List                     ( intercalate )

-- | A foldable resuls aggregator 
resultsAggr :: Eval a => (a -> [Double]) -> a -> [Double] -> [Double]
resultsAggr f a rs = rs ++ f a

csvResults :: String -> [[Double]] -> IO ()
csvResults hdr rs = do putStrLn hdr
                       mapM_ csvResultLine rs

csvResultLine :: [Double] -> IO ()
csvResultLine = putStrLn . intercalate "," . map show 

class Show a => Eval a 

instance Show a => Eval (CCEval  a)
instance Show a => Eval (SegEval a)
instance Eval EqIgnore
instance Eval Double
instance Eval Int