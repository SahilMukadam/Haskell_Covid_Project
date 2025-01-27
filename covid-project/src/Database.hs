{-# LANGUAGE OverloadedStrings #-}

-- or, on GHCI:
-- > :set -XOverloadedStrings

module Database (
    initialiseDB,
    createTables,
    getOrCreateCountry,
    saveRecords,
    queryCountryAllEntries,
    queryCountryTotalCases
) where

-- See more Database.SQLite.Simple examples at
-- https://hackage.haskell.org/package/sqlite-simple-0.4.18.0/docs/Database-SQLite-Simple.html

import Types
import Database.SQLite.Simple
import Database.SQLite.Simple.Internal
import Control.Applicative
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow

instance FromRow Record where
    fromRow = Record <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance FromRow Country where
    fromRow = Country <$> field <*> field <*> field <*> field

instance ToRow Country where
    toRow (Country id_ country_ continent_ population_)
        = toRow (id_, country_, continent_, population_)

instance FromRow Entry where
    fromRow = Entry <$> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToRow Entry where
    toRow (Entry date_ day_ month_ year_ cases_ deaths_ fk_country)
        = toRow (date_, day_, month_, year_, cases_, deaths_, fk_country)

initialiseDB :: IO Connection
initialiseDB = open "covid.sqlite"

createTables :: Connection -> IO ()
createTables conn = do
        execute_ conn "CREATE TABLE IF NOT EXISTS countries (\
            \id INTEGER PRIMARY KEY AUTOINCREMENT,\
            \country VARCHAR(80) NOT NULL, \
            \continent VARCHAR(50) NOT NULL, \
            \population INT DEFAULT NULL \
            \)"
        execute_ conn "CREATE TABLE IF NOT EXISTS entries (\
            \date VARCHAR(40) NOT NULL, \
            \day VARCHAR(40) NOT NULL, \
            \month VARCHAR(40) NOT NULL, \
            \year VARCHAR(40) NOT NULL, \
            \cases INT DEFAULT NULL, \
            \deaths INT DEFAULT NULL, \
            \fk_country INTEGER\
            \)"

getOrCreateCountry :: Connection -> String -> String -> Maybe Int -> IO Country
getOrCreateCountry conn country continent population = do
    let selectQuery = "SELECT * FROM countries WHERE country=:country AND continent=:continent"
    results <- queryNamed conn selectQuery [":country" := country, ":continent" := continent]    
    if length results > 0 then
        return . head $ results
    else do
        let insertQuery = "INSERT INTO countries (country, continent, population) VALUES (?, ?, ?)"
        execute conn insertQuery (country, continent, population)
        getOrCreateCountry conn country continent population

createRecord :: Connection -> Record -> IO ()
createRecord conn record = do
    country <- getOrCreateCountry conn (country record) (continent record) (population record)
    let entry = Entry {
        date_ = date record,
        day_ = day record,
        month_ = month record,
        year_ = year record,
        cases_ = cases record,
        deaths_ = deaths record,
        fk_country = id_ country
    }
    execute conn "INSERT INTO entries VALUES (?,?,?,?,?,?,?)" entry

saveRecords :: Connection -> [Record] -> IO ()
saveRecords conn = mapM_ (createRecord conn)

queryCountryAllEntries :: Connection -> String -> IO [Record]
queryCountryAllEntries conn countryName = do
    putStrLn $ "Looking for " ++ countryName ++ " entries..."
    let sql = "SELECT date, day, month, year, cases, deaths, country, continent, population FROM entries inner join countries on entries.fk_country == countries.id WHERE country=?"
    query conn sql [countryName]

queryCountryTotalCases :: Connection -> String -> IO ()
queryCountryTotalCases conn countryName = do
    countryEntries <- queryCountryAllEntries conn countryName
    let total = sum (map cases countryEntries)
    print $ "Total " ++ countryName ++ " entries: " ++ show(total)
