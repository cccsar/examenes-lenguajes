module Computation ( 
 getSize, 
 actualStoredSize,
 getAlignment
) where


import Types
import qualified Data.Map as M (lookup) 

import Data.List (permutations, minimumBy) 
import Control.Monad (liftM2) 


{- Size and alignment breakdown
 -
 - Storage type:               Struct                          Union                    
 -      Normal
 -        Size       |  that of nextAlignment function   |  Maximum element
 -        Alignment  |  That of first element            |  lcm of alignments
 -      Packaged     |  ----------------------------------------------------
 -        Size       |  Continuous but considers waste * |  Maximum elemtn
 -                   |  for nested structs               |
 -        Alignment  |  That of first element            |  lcm of alignments
 -      Reoredered   |  -----------------------------------------------------
 -        Size       |  The one produced by the best   * |  Maximum element
 -                   |  reordering of fields using       |
 -                   |  normal size alginment            |
 -        Alignment  |  That of first element            |  lcm of alignments
 -
 -}


-- Computes the stored size of a given type descriptor
actualStoredSize :: TypeDescriptor -> Memory -> Maybe Size
actualStoredSize (Atom sz _)    _      = Just sz
actualStoredSize (Struct names) memory = foldr (reduce memory (+)) (Just 0) names
actualStoredSize (Union names) memory  = foldr (reduce memory max) (Just 0) names


-- Helper function to traverse nested structures/unions and compute their sizes
reduce :: Memory -> (Int -> Int -> Int) -> String -> Maybe Int -> Maybe Int
reduce memory op name currentCount = 
 do 
   aType   <- M.lookup name memory
   accSize <- currentCount
   case aType of 
     (Atom sz _) -> return ( op accSize sz )
     _           -> liftM2 op currentCount (actualStoredSize aType memory )


{- Functions for sizes -} 


{- - Front end size computation. 
 -
 - Notice that, for an Union the way to compute the size is always the same, 
 - (maximum size of the fields ) independently of the StorageType. Thus 
 - getNormalSize is called on Unions but the StorageType makes sure 
 - that nested struct's size is properly computed.
 -}
getSize :: TypeDescriptor -> StorageType -> Memory -> Maybe Size
getSize (Atom sz _)  _  _      = Just sz
getSize t@(Struct _) st memory = case st of 
                               Normal    -> getNormalSize t st memory
                               Packaged  -> actualStoredSize t memory
                               Reordered -> getReorderedSize t st memory
getSize t@(Union _)  st memory = getNormalSize t st memory 

----------------------------------------------------------------------------------------

getNormalSize, getReorderedSize :: TypeDescriptor -> StorageType -> Memory -> Maybe Size

-- Normal Size computation. 
getNormalSize (Struct names) stType memory = foldl foo (Just 0) names
 where 
  foo accumBytes name = do 
       aType         <- M.lookup name memory                   -- get Current type

       size          <- getSize aType stType memory
       alignment     <- getAlignment aType stType memory

       currentOffset <- accumBytes

       return ( nextAlignment alignment currentOffset + size )  -- return accumulated size
getNormalSize (Union names) stType memory = foldr foo (Just 0) names
 where 
  foo name maxBytes = do 
       aType          <- M.lookup name memory                   -- get Current type
       size           <- getSize aType stType memory            -- find it's size

       currentMaximum <- maxBytes                                 
       
       return ( max size currentMaximum )                       -- return maximum
       

-- Reordered size computation
getReorderedSize aType _ memory = fmap snd (retrieveBestFitSize aType memory)

----------------------------------------------------------------------------------------



{- Functions for Alignments -} 

{- Front end for computing alignment. 
 - Notice that, just as with size, the alignment for a Union is always computed the
 - same way (lcm of fields alignments) independently of the storage type. Thus 
 - the same abstraction as with getNormalAlignment is made, to reduce code.
 -}
getAlignment :: TypeDescriptor -> StorageType -> Memory -> Maybe Alignment
getAlignment (Atom _ alg)   _   _      = Just alg
getAlignment t@(Struct _)   stT memory = 
  case stT of 
    Reordered -> getReorderedAlignment t stT memory
    _         -> getNormalAlignment t stT memory
getAlignment t@(Union names) stT memory = getNormalAlignment t stT memory


-------------------------------------------------------------------------------------------------------

getNormalAlignment, getReorderedAlignment :: TypeDescriptor -> StorageType -> Memory -> Maybe Alignment

-- Normal alignment computation. 
getNormalAlignment (Struct names) stT memory = case names of 
                                (x:_) -> M.lookup x memory >>= flip (flip getAlignment stT) memory
                                []    -> Nothing
getNormalAlignment (Union names)  stT memory= case names of 
                                (_:_) -> foldr foo (Just 1) names
                                []    -> Nothing
 where 
  foo name currentAlignment = do 
            aType     <- M.lookup name memory
            alignment <- getAlignment aType stT memory 
            currAl    <- currentAlignment

            return (lcm alignment currAl) 


-- Reordered alignment computation
getReorderedAlignment aType _ memory = getAlignmentPerms memory aType

-------------------------------------------------------------------------------------------------------

{- Helpers -} 

-- Retrieves the permutation that causes a best fit size for a Struct and it's corresponding size.
retrieveBestFitSize :: TypeDescriptor -> Memory -> Maybe ( [Name] , Size ) 
retrieveBestFitSize (Struct names) memory = 
  do sizes <- mapM (getSizePerms memory) structPerms

     let  matchSizes  = zip perms sizes
          expectedMin = minimumBy (\(_,a) (_,b) -> a `compare` b) matchSizes ::  ([String],Int) 

     return expectedMin
 where perms = permutations names ; structPerms = map Struct perms 


-- Computes the size for a TypeDescriptor that uses reordering as storage size.
getSizePerms :: Memory -> TypeDescriptor -> Maybe ( Size ) 
getSizePerms _      (Atom size  _) = Just size
getSizePerms memory (Struct names)   = foldl foo (Just 0) names
 where 
  foo accumBytes name =
    do aType         <- M.lookup name memory                   -- get current type

       size          <- case aType of                          -- get it's size considering nested structs
                          (Struct _) -> fmap snd (retrieveBestFitSize aType memory)
                          _          -> getSizePerms memory aType
       alignment     <- getAlignmentPerms memory aType
       currentOffset <- accumBytes

       return ( nextAlignment alignment currentOffset + size ) 
getSizePerms memory (Union names) = foldr foo (Just minBound) names
 where 
  foo name maxBytes = 
    do aType          <- M.lookup name memory                  -- get current type

       size           <- getSizePerms memory aType             -- find it's size
       currentMaximum <- maxBytes                                 
      
       return ( max size currentMaximum )                      -- return maximum
 

-- Computes the alignment for a TypeDescriptor that uses reordering as storage size.
getAlignmentPerms :: Memory -> TypeDescriptor -> Maybe (Alignment) 
getAlignmentPerms memory (Atom _ alignment ) = Just alignment
getAlignmentPerms memory t@(Struct names)    = 
  do (orderedNames, _) <- retrieveBestFitSize t memory
     case orderedNames of 
       (x:_) -> M.lookup x memory >>= getAlignmentPerms memory
       []    -> Nothing
getAlignmentPerms memory (Union names)       = 
  case names of 
    (_:_) -> foldr foo (Just 1) names
    []    -> Nothing
 where 
  foo name currentAlignment = 
    do aType     <- M.lookup name memory
       alignment <- getAlignmentPerms memory aType 
       currAl    <- currentAlignment

       return (lcm alignment currAl) 
       

-- Computes next alignmet to choose for a system that doesn't use any particular
-- storage method. 
nextAlignment :: Alignment -> Size -> Size
nextAlignment alignment offset
 | offset `mod` alignment == 0 = alignment * quot
 | otherwise                   = alignment * (quot + 1)
 where quot = offset `div` alignment
