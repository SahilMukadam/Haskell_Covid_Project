module Main where

import System.Environment
import System.IO

import Types
import Fetch
import Parse
import Database

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["create"] -> do
            conn <- initialiseDB
            createTables conn
            hSetBuffering stdout NoBuffering
        ["loaddata"] -> do
            conn <- initialiseDB
            let url = "https://opendata.ecdc.europa.eu/covid19/casedistribution/json/"
            print "Downloading..."
            json <- download url
            print "Parsing..."
            case (parseRecords json) of
                Left err -> print err
                Right recs -> do
                    print "Saving on DB..."
                    let firstFew = take 100000 (records recs)
                    saveRecords conn firstFew
                    print "Saved!"
        ["country", countryName] -> do
            conn <- initialiseDB
            entries <- queryCountryAllEntries conn countryName
            mapM_ print entries
        ["countrytotal", countryName] -> do
            conn <- initialiseDB
            queryCountryTotalCases conn countryName
        _ -> syntaxError

syntaxError = putStrLn 
    "Usage: stack run -- [args]\n\
    \\n\
    \create                   Setup the database\n\
    \loaddata                 Download COVID data and save on DB\n\
    \country <country>        Print all entries for a given country (e.g. United_Kingdom)\n\
    \countrytotal <country>   Print total number of entries for a given country (e.g. United_Kingdom)\n"

