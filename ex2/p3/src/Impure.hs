module Impure ( 
 getAtomic, 
 getStructOrUnion,
 describeType,
 requestInfo, 
 warning
) where


import Computation
import Constants (prompt, invalidAtomDescription, invalidCompoundFormat, invalidName) 
import Types

import Data.Char (isDigit) 
import Data.Maybe (fromJust)
import System.IO (stdout, stderr, hPutStrLn, hPutStr, hFlush, Handle) 
import qualified Data.Map as M (insert, member, lookup)


{- Obtaining input and querying memory -} 

-- gets an ATOM from input
getAtomic :: Memory -> [String] -> IO Memory
getAtomic memory (name:size:offset:[])
  | pred       = let actSize = read size; actOffset = read offset
                 in return $ M.insert name (Atom actSize actOffset) memory
  | otherwise  = warning stderr invalidAtomDescription >> return memory
 where 
   pred = isNumber size && isNumber offset && not (M.member name memory)  
   isNumber = all isDigit
getAtomic memory _  =  warning stderr invalidAtomDescription >> return memory


-- gets a struct or an union from input
getStructOrUnion :: Memory -> [Name] -> Bool -> IO Memory
getStructOrUnion memory (name:types) guard
   | pred      = if guard then return $ M.insert name (Struct types) memory 
                                    else  return $ M.insert name (Union types) memory
   | otherwise = warning stderr invalidCompoundFormat >> return memory
  where pred = checkNameSpace memory types && not (M.member name memory) && not (null types)
getStructOrUnion memory _            _ = warning stderr invalidCompoundFormat >> return memory


-- Describes the memory usage of a given type by giving total size, alignment 
-- and wasted space. This information is given for normal, packaged and reoredered
-- storage types (in the case of a compound type) 
describeType :: Memory -> Name -> IO ()
describeType memory name = 
 case M.lookup name memory of 
   (Just aType) -> 
     case aType of 
       (Atom size alignment) -> typeDescription size (size, alignment, Nothing) 
       _                     -> do 
          let actualSize = fromJust $ actualStoredSize aType memory
              stTypes    = [minBound..maxBound] :: [StorageType]

              sizes      = [ fromJust $ getSize aType x memory | x <- stTypes ] 
              alignments = [ fromJust $ getAlignment aType x memory | x <- stTypes ] 

              wastes     = map (\sz -> sz - actualSize) sizes

          mapM_ (typeDescription actualSize) (zip3 sizes alignments (map Just stTypes)) 

   _            -> warning stderr invalidName 


-- Displays a formated type description for a single type descriptor.
typeDescription :: Size -> (Size , Alignment , Maybe StorageType) -> IO () 
typeDescription realSize (occupied, alignment, st) = do 
      case st of 
       (Just e) ->  putStrLn $ "\tTipo de almacenamiento: " ++ show e
       _        ->  putStrLn $ "\tInformación relevante: "  

      putStrLn $ "\t\tEspacio total ocupado: " ++ show occupied
      putStrLn $ "\t\tEspacio desperdiciado: " ++ show (occupied - realSize) 
      putStrLn $ "\t\tAlineación: " ++ show alignment


-- Given a list of names, make sure they have already been defined
checkNameSpace :: Memory -> [String] -> Bool
checkNameSpace memory names = foldr (\name acc -> M.member name memory && acc) True names



{- Helpers for text layout -} 

-- Requests input using prompt string.
requestInfo :: Handle -> IO String
requestInfo handle = do 
                   hPutStr handle prompt
                   hFlush stdout
                   getLine


-- Warns/tells the user messages using prompt and the appropriate file descriptor.
warning :: Handle -> String -> IO () 
warning handle message = hPutStrLn handle ('\t' : message) 
