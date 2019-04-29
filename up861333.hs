--
-- MATHFUN
-- UP861333
--
import Data.List
import Data.Ord
import Data.Function
import Text.Read
--
-- Types
--
-- Define Album type here

type Album = (String, String, Int, Int)

testData :: [Album]
testData = [("Greatest Hits","Queen",1981,6300000),("Gold: Greatest Hits","ABBA",1992,5400000),("Sgt. Pepper's Lonely Hearts Club Band", "The Beatles",1967,5340000),("21","Adele",2011,5110000),("(What's the Story) Morning Glory?","Oasis",1995,4940000),("Thriller","Michael Jackson",1982,4470000),("The Dark Side of the Moon","Pink Floyd",1973,4470000),("Brothers in Arms","Dire Straits",1985,4350000),("Bad","Michael Jackson",1987,4140000),("Rumours","Fleetwood Mac",1977,4090000),("Greatest Hits II","Queen",1991,3990000),("Back to Black","Amy Winehouse",2006,3940000),("The Immaculate Collection","Madonna",1990,3700000),("25","Adele",2015,3500000),("Stars","Simply Red",1991,3450000),("Come On Over","Shania Twain",1998,3430000),("x","Ed Sheeran",2014,3380000),("Legend","Bob Marley",1984,3380000),("Bat Out of Hell","Meat Loaf",1977,3370000),("Back to Bedlam","James Blunt",2004,3360000),("Urban Hymns","The Verve",1997,3340000),("Bridge over Troubled Water","Simon & Garfunkel",1970,3260000),("1","The Beatles",2000,3230000),("Spirit","Leona Lewis",2007,3170000),("Crazy Love","Michael Bublé",2009,3130000),("No Angel","Dido",2000,3090000),("White Ladder","David Gray",1998,3020000),("The Fame","Lady Gaga",2009,2990000),("Only by the Night","Kings of Leon",2008,2980000),("A Rush of Blood to the Head","Coldplay",2002,2960000),("Talk on Corners","The Corrs",1997,2960000),("Spice","Spice Girls",1996,2960000),("Life for Rent","Dido",2003,2900000),("Beautiful World","Take That",2006,2880000),("The Joshua Tree","U2",1987,2880000),("Hopes and Fears","Keane",2004,2860000),("The War of the Worlds","Jeff Wayne",1978,2800000),("X&Y","Coldplay",2005,2790000),("Jagged Little Pill","Alanis Morissette",1995,2780000),("Tubular Bells","Mike Oldfield",1973,2760000),("Scissor Sisters","Scissor Sisters",2004,2760000),("...But Seriously","Phil Collins",1989,2750000),("Tracy Chapman","Tracy Chapman",1988,2710000),("Parachutes","Coldplay",2000,2710000),("The Man Who","Travis",1999,2687500),("Greatest Hits","ABBA",1975,2606000),("I've Been Expecting You","Robbie Williams",1998,2586500),("Come Away with Me","Norah Jones",2002,2556650),("Graceland","Paul Simon",1986,2500000),("Ladies & Gentlemen: The Best of","George Michael",1998,2500000)]

--
--
--  Your functional code goes here
--
--

-- CODE FOR DEMO 8 --

lst (_,_,_,sales) = sales
sortByLast albums = reverse (sortBy(compare `on` lst) albums)

isQueriedAlbum :: Album -> String -> String -> Bool
isQueriedAlbum (name, artist, year, sales) searchName searchArtist
    | name == searchName && artist == searchArtist = True
    | otherwise = False

addSalesToAlbum :: [Album] -> String -> String -> Int -> [Album]
addSalesToAlbum albums "" _ _ = albums
addSalesToAlbum albums _ "" _ = albums
addSalesToAlbum [] _ _ _ = []
addSalesToAlbum (x:xs) name artist newSales
    | (x:xs) == [] = []
    | isQueriedAlbum x name artist == True = sortByLast(salesIncrease x newSales : xs)
    | otherwise = sortByLast(x : (addSalesToAlbum xs name artist newSales))

salesIncrease :: Album -> Int -> Album
salesIncrease (name, artist, year, sales) newSales = (name, artist, year, (sales + newSales))

-- CODE FOR DEMO 7 --

getAlbumSales :: Album -> Int
getAlbumSales (name, artist, year, sales) = sales

getNewIndex :: [Album] -> Int -> Int -> Int
getNewIndex albumdata newSales index
    | length albumdata > 0 && getAlbumSales(head albumdata) < newSales = index
    | length albumdata > 0 && getAlbumSales(head albumdata) > newSales = getNewIndex(drop 1 albumdata) newSales (index + 1)
    | length albumdata > 0 = index
    | otherwise = index

addToAlbums :: Int -> Album -> [Album] -> [Album]
addToAlbums i n (x:xs)
    | i == 0 = x:n:xs
    | i == -1 = n:xs
    | otherwise = x:addToAlbums (i-1) n xs


addNewAlbum :: [Album] -> Album -> String
addNewAlbum albumdata (name, artist, year, sales) = albumsToString(addToAlbums ((getNewIndex (init albumdata) sales 0)-1) (name, artist, year, sales) (init albumdata))

-- CODE FOR DEMO 6 --

count :: Ord a => [a] -> [(a, Int)]
count = map (\x -> (head x,length x)) . group .sort

getArtist :: Album -> String
getArtist (_, artist, _, _) = artist
artists = map getArtist

countArtists :: [Album] -> String
countArtists albumdata = albumsCountToString (count (artists testData))

format :: (String, Int) -> String
format (artistName, count) = addSpaces(artistName) ++ addSpaces(show count) ++ "\n"

albumsCountToString :: [(String, Int)] -> String
albumsCountToString albumdata
    | length albumdata > 0 = format (head albumdata) ++ albumsCountToString(drop 1 albumdata)
    | otherwise = ""

-- CODE FOR DEMO 5 --
getSales :: Album -> String -> Int
getSales (name, artist, year, sales) artistName
    | artistName == artist = sales
    | otherwise = 0

totalSales :: [Album] -> String -> Int
totalSales albumdata artist
    | length albumdata > 0 = getSales (head albumdata) artist + totalSales(drop 1 albumdata) artist
    | otherwise = 0

-- CODE FOR DEMO 4 --
hasPrefix :: Album -> String -> String
hasPrefix (name, artist, year, sales) prefix
    | prefix `isPrefixOf` name = formatSingleEntry (name, artist, year, sales)
    | otherwise = ""

albumPrefix :: [Album] -> String -> String
albumPrefix albumdata prefix
    | length albumdata > 0 = hasPrefix (head albumdata) prefix ++ albumPrefix(drop 1 albumdata) prefix
    | otherwise = ""

-- CODE FOR DEMO 3 --
dateCheck :: Album -> Int -> Int -> String
dateCheck (name, artist, year, sales) datemin datemax
    | year >= datemin && year <= datemax = formatSingleEntry (name, artist, year, sales)
    | otherwise = ""

albumsBetween :: [Album] -> Int -> Int -> String
albumsBetween albumdata datemin datemax
    | length albumdata > 0 = dateCheck (head albumdata) datemin datemax ++ albumsBetween (drop 1 albumdata) datemin datemax
    | otherwise = ""

-- CODE FOR DEMO 2 --
top10 :: [Album] -> [Album]
top10 albumdata = take 10 albumdata

-- CODE FOR DEMO 1 --
addSpaces :: String -> String
addSpaces text
    | length text < 40 = text ++ (concat $ replicate (40 - length text) " ")
    | otherwise = text

formatSingleEntry :: Album -> String
formatSingleEntry (name, artist, year, sales) = addSpaces(name) ++ addSpaces(artist) ++ addSpaces(show year) ++ addSpaces(show sales) ++ "\n"

albumsToString :: [Album] -> String
albumsToString albumdata
    | length albumdata > 0 = formatSingleEntry (head albumdata) ++ albumsToString(drop 1 albumdata)
    | otherwise = ""


-- Demo function to test basic functionality (without persistence - i.e.
-- testData doesn't change and nothing is saved/loaded to/from albums file).

demo :: Int -> IO ()
demo 1  = putStrLn (albumsToString testData)
demo 2  = putStrLn (albumsToString (top10 testData))
demo 3  = putStrLn (albumsBetween testData 2000 2008)
demo 4  = putStrLn (albumPrefix testData "Th")
demo 5  = putStrLn (show (totalSales testData "Queen"))
demo 6  = putStrLn (countArtists testData)
demo 7  = putStrLn (addNewAlbum testData ("Progress","Take That",2010,2700000))
demo 8  = putStrLn (albumsToString (addSalesToAlbum testData "21" "Adele" 400000))

--
--
-- Your user interface (and loading/saving) code goes here

main :: IO()
main = ui True

ui :: Bool -> IO ()
ui showInitDemo = do if showInitDemo == True
                        then do putStrLn "MATHFUN CW UP861333"
                                putStrLn ""
                                demo 1
                                putStrLn "Please select an option"
                                putStrLn ""
                                putStrLn "1. Show all"
                                putStrLn "2. Show Top 10"
                                putStrLn "3. Show all between dates"
                                putStrLn "4. Show all with prefix"
                                putStrLn "5. Show total sales of artist"
                                putStrLn "6. Show number of albums in top 50 per artist"
                                putStrLn "7. Remove worst album and add new entry"
                                putStrLn "8. Add sales to album"
                                option <- getLine
                                menuChoice (read option :: Int)
                       else do putStrLn "Please select an option"
                               putStrLn ""
                               putStrLn "1. Show all"
                               putStrLn "2. Show Top 10"
                               putStrLn "3. Show all between dates"
                               putStrLn "4. Show all with prefix"
                               putStrLn "5. Show total sales of artist"
                               putStrLn "6. Show number of albums in top 50 per artist"
                               putStrLn "7. Remove worst album and add new entry"
                               putStrLn "8. Add sales to album"
                               option <- getLine
                               menuChoice (read option :: Int)

menuChoice :: Int -> IO()
menuChoice 1 = do putStrLn(albumsToString testData)
                  ui False
menuChoice 2 = do putStrLn(albumsToString (top10 testData))
                  ui False
menuChoice 3 = do putStrLn "Enter start date:"
                  startDate <- getLine
                  putStrLn "Enter end date:"
                  endDate <- getLine
                  if isInt(startDate) == False || (read startDate :: Int) <= 0
                  then do putStrLn "start date value invalid"
                          menuChoice 3
                          else if isInt(endDate) == False || (read endDate :: Int) <= 0
                            then do putStrLn "end date value invalid"
                                    menuChoice 3
                                    else if (read startDate :: Int) > (read endDate :: Int)
                                      then do putStrLn "Start date can't be higher than the end date"
                                              menuChoice 3
                  else do putStrLn (albumsBetween testData (read startDate :: Int) (read endDate :: Int))
                          ui False
menuChoice 4 = do putStrLn "Enter a prefix eg.(th):"
                  prefix <- getLine
                  putStrLn (albumPrefix testData prefix)
                  ui False
menuChoice 5 = do putStrLn "Enter artist name"
                  artist <- getLine
                  putStrLn (show (totalSales testData artist))
                  ui False
menuChoice 6 = do putStrLn (countArtists testData)
                  ui False
menuChoice 7 = do putStrLn "Enter new album name"
                  name <- getLine
                  putStrLn "Enter new album artist"
                  artist <- getLine
                  putStrLn "Enter new album release year"
                  year <- getLine
                  putStrLn "Enter new album sales"
                  sales <- getLine
                  if isInt(year) == False || (read year :: Int) <= 0
                  then do putStrLn "year value invalid"
                          menuChoice 7
                          else if isInt(sales) == False || (read sales :: Int) < 0
                            then do putStrLn "sales value invalid"
                                    menuChoice 7
                  else do putStrLn (addNewAlbum testData (name,artist,(read year :: Int),(read sales :: Int)))
                          ui False
menuChoice 8 = do putStrLn "Enter new album name"
                  name <- getLine
                  putStrLn "Enter new album artist"
                  artist <- getLine
                  putStrLn "Enter new sales increase"
                  sales <- getLine
                  if isInt(sales) == False || (read sales :: Int) <= 0
                    then do putStrLn "sales increase value invalid"
                            menuChoice 8
                  else do putStrLn (albumsToString (addSalesToAlbum testData name artist (read sales :: Int)))
                          ui False

isInt :: String -> Bool
isInt value
    | (readMaybe value :: Maybe Int) == Nothing = False
    | otherwise = True
