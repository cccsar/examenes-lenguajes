module Impure where

import Computation
import Constants
import Input
import Types

import Data.Maybe (fromJust)
import System.IO
import qualified Data.Map as M (insert, member, lookup)


{- Obtaining input and querying memory -} 

-- gets an ATOM from input
getAtomic :: Memory -> [String] -> IO Memory
getAtomic memory (name:size:offset:[])
  | pred       = let actSize = read size; actOffset = read offset
                 in return $ M.insert name (Atom actSize actOffset) memory
  | otherwise  = warning stderr invalidAtomDescription >> return memory
 where pred = isNumber size && isNumber offset && not (M.member name memory)  
getAtomic memory _  =  warning stderr invalidAtomDescription >> return memory


-- gets a struct or an union from input
getStructOrUnion :: Memory -> [Name] -> Bool -> IO Memory
getStructOrUnion memory (name:types) guard
   | pred      = if guard then return $ M.insert name (Struct types) memory 
                                    else  return $ M.insert name (Union types) memory
   | otherwise = warning stderr invalidCompoundFormat >> return memory
  where pred = checkNameSpace memory types && not (M.member name memory) 
getStructOrUnion memory _            _ = warning stderr invalidCompoundFormat >> return memory


-- Describes the memory usage of a given type by giving total size, alignment 
-- and wasted space. This information is given for normal, packaged and reoredered
-- storage types (in the case of a compound type) 
describeType :: Memory -> Name -> IO ()
describeType memory name = case M.lookup name memory of 
    (Just aType) -> case aType of 
        (Atom size alignment) -> typeDescription size 0 alignment Nothing 
        (Struct names)        -> do 
            let actualSize = actualStoredSize aType memory
                -- totalSize = getSize aType Normal memory
                alignment = getAlignment aType memory 
                   
            typeDescription (fromJust actualSize) 0 (fromJust alignment) (Just Normal) 

        (Union names)         -> do
            let actualSize = actualStoredSize aType memory
            -- totalSize = getSize aType Normal memory
                alignment = getAlignment aType memory 
                   
            typeDescription (fromJust actualSize) 0 (fromJust alignment) (Just Normal) 
    _            -> warning stderr invalidName 


typeDescription :: Size -> Size -> Alignment -> Maybe StorageType -> IO () 
typeDescription size waste alignment st = do 
      case st of 
       (Just e) ->  putStrLn $ "Tipo de almacenamiento: " ++ show e
       _        ->  putStrLn $ "Información relevante: "  

      putStrLn $ "\tEspacio total ocupado: " ++ show size
      putStrLn $ "\tEspacio desperdiciado: " ++ show waste
      putStrLn $ "\tAlineación: " ++ show alignment


-- Given a list of names, make sure they have already been defined
checkNameSpace :: Memory -> [String] -> Bool
checkNameSpace memory names = foldr (\name acc -> M.member name memory && acc) True names



{- Helpers for text layout -} 

-- Requests input using prompt string.
requestInfo :: Handle -> String -> IO String
requestInfo handle message = do 
                   hPutStr handle (prompt ++ " " ++ message ++ " ") 
                   hFlush stdout
                   getLine
                   


-- Warns/tells the user messages using prompt and the appropriate file descriptor.
warning :: Handle -> String -> IO () 
warning handle message = hPutStrLn handle ('\t' : message) 
