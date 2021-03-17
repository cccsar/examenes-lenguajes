module Computation where

import Types
import qualified Data.Map as M

import Control.Monad (liftM2) 


-- Computes the stored size of a given type descriptor
actualStoredSize :: TypeDescriptor -> Memory -> Maybe Size
actualStoredSize (Atom sz _)    _      = Just sz
actualStoredSize (Struct names) memory = foldr (reduce memory (+)) (Just 0) names
actualStoredSize (Union names) memory  = foldr (reduce memory max) (Just 0) names


-- Helper function to traverse nested structures/unions and compute their sizes
reduce :: Memory -> (Int -> Int -> Int) -> String -> Maybe Int -> Maybe Int
reduce memory op name currentCount = do 
                          aType   <- M.lookup name memory
                          accSize <- currentCount
                          case aType of 
                            (Atom sz _) -> return ( op accSize sz )
                            _           -> liftM2 op currentCount (actualStoredSize aType memory )


{- Function for sizes -} 

getSize :: TypeDescriptor -> StorageType -> Memory -> Maybe Size
getSize (Atom sz _) _ _ = Just sz
getSize t@(Struct _) st mem = case st of 
                          Normal -> getNormalSize t st mem
                          Packaged -> undefined
                          Reordered -> undefined
getSize t@(Union _) st mem = case st of 
                          Normal -> getNormalSize t st mem
                          Packaged -> undefined
                          Reordered -> undefined


getNormalSize, getPackagedSize, getReorderedSize :: TypeDescriptor -> StorageType -> Memory -> Maybe Size

-- Normal Size computation
getNormalSize (Struct names) stType memory = foldr foo (Just 0) names
 where 
  foo name accumBytes = do 
       aType         <- M.lookup name memory                   -- get Current type
       alignment     <- getAlignment aType memory       -- find it's alignment
       size          <- getSize aType stType memory            -- find it's size

       currentOffset <- accumBytes

       return ( nextAlignment alignment currentOffset + size )  -- return accumulated size
getNormalSize (Union names) stType memory = foldr foo (Just 0) names
 where 
  foo name maxBytes = do 
       aType          <- M.lookup name memory                   -- get Current type
       size           <- getSize aType stType memory            -- find it's size

       currentMaximum <- maxBytes                                 
       
       return ( max size currentMaximum )                       -- return maximum
       

-- Packaged size computation
getPackagedSize (Struct names) = undefined
getPackagedSize (Union names)  = undefined


-- Reordered size computation
getReorderedSize (Struct names) = undefined
getReorderedSize (Union names)  = undefined


{- Functions for Alignments -} 

getAlignment :: TypeDescriptor -> Memory -> Maybe Alignment
getAlignment (Atom _ alg)   _ = Just alg
getAlignment (Struct names) memory = case names of 
                                (x:xs) -> M.lookup x memory >>= flip getAlignment memory
                                []     -> Nothing
getAlignment (Union names) memory = case names of 
                                (x:xs) -> foldr foo (Just 1) names
                                []     -> Nothing
 where 
  foo name currentAlignment = do 
            aType <- M.lookup name memory
            alignment <- getAlignment aType memory 
            currAl <- currentAlignment

            return (lcm alignment currAl) 


getNormalAlignment, getPackagedAlignment, getReorderedAlignment :: TypeDescriptor -> Memory -> Either String Size

-- Normal alignment computation. 
getNormalAlignment (Struct names) = undefined
getNormalAlignment (Union names)  = undefined


-- Packaged alignment computation
getPackagedAlignment = undefined


-- Reordered alignment computation
getReorderedAlignment = undefined


{- Helpers -} 

-- Computes next alignmet to choose for a system that doesn't emply any particular
-- storage method. 
nextAlignment :: Alignment -> Size -> Size
nextAlignment alignment size
 | size `mod` alignment == 0 = alignment * quot
 | otherwise                 = alignment * (quot + 1)
 where quot = size `div` alignment
