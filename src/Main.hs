module Main where

import Args
  ( AddOptions (..),
    Args (..),
    GetOptions (..),
    SearchOptions (..),
    parseArgs,
  )
import qualified Data.List as L
import qualified Entry.DB as DB
import System.IO (readFile)
import Entry.Entry
  ( Entry (..),
    FmtEntry (FmtEntry),
    matchedByAllQueries,
    matchedByQuery,
  )
import Result
import System.Environment (getArgs)
import Test.SimpleTest.Mock
import Prelude hiding (print, putStrLn, readFile)
import qualified Prelude

usageMsg :: String
usageMsg =
  L.intercalate
    "\n"
    [ "snip - code snippet manager",
      "Usage: ",
      "snip add <filename> lang [description] [..tags]",
      "snip search [code:term] [desc:term] [tag:term] [lang:term]",
      "snip get <id>",
      "snip init"
    ]

-- | Handle the init command
handleInit :: TestableMonadIO m => m ()
handleInit = do
  let emptyDb = DB.empty  -- Create an empty database
  result <- DB.save emptyDb -- Save the empty database
  case result of
    Error err -> putStrLn $ "Error initializing database: " ++ show err
    Success _ -> putStrLn "Database initialized successfully"

-- | Handle the get command
handleGet :: TestableMonadIO m => GetOptions -> m ()
handleGet getOpts = do
  db <- DB.load
  case db of
    (Error r) -> putStrLn "Failed to load DB"
    (Success db') -> do
          case DB.findFirst (\x->entryId x == getOptId getOpts) db' of
            Nothing -> putStrLn ("There is no matching entry with id" ++ show (getOptId getOpts) ++ " in the database.")
            (Just id1) -> putStrLn (entrySnippet id1)

-- | Handle the search command
handleSearch :: TestableMonadIO m => SearchOptions -> m ()
handleSearch searchOpts = do
  db <- DB.load
  case db of
    (Error r) -> putStrLn "Failed to load DB"
    (Success db') -> do
      let mat = DB.findAll (matchedByAllQueries (searchOptTerms searchOpts)) db'
      case mat of
        [] -> putStrLn "No entries found"
        _ -> putStrLn (foldl (++) "" (map (\x -> show (FmtEntry x) ++ "\n") mat))


-- | Handle the add command
handleAdd :: TestableMonadIO m => AddOptions -> m ()
handleAdd addOpts = do
  db <- DB.load
  entries <- Test.SimpleTest.Mock.readFile (addOptFilename addOpts)
 
  case db of

    (Error r) -> putStrLn "Failed to load DB"
    (Success db') -> do

     
      case  DB.findFirst (\x -> entrySnippet x == entries) db' of
        Just id -> putStrLn ("Entry with this content already exists: \n" ++ show (FmtEntry id))
        Nothing -> do
          DB.modify
            ( DB.insertWith
                ( \id ->
                    Entry
                      { entryId = id,
                        entrySnippet = entries,
                        entryFilename = addOptFilename addOpts,
                        entryLanguage = addOptLanguage addOpts,
                        entryDescription = addOptDescription addOpts,
                        entryTags = addOptTags addOpts
                      }
                )
            )
          return ()

-- | Dispatch the handler for each command
run :: TestableMonadIO m => Args -> m ()
run (Add addOpts) = handleAdd addOpts
run (Search searchOpts) = handleSearch searchOpts
run (Get getOpts) = handleGet getOpts
run Init = handleInit
run Help = putStrLn usageMsg

main :: IO ()
main = do
  args <- getArgs
  let parsed = parseArgs args
  case parsed of
    (Error err) -> Prelude.putStrLn usageMsg
    (Success args) -> run args
