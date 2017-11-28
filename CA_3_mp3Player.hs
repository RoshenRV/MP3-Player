-- CA3 MP3-Player Assignment
-- Roshen Varghese - D00188772

-- This assignment has been extensively worked together with Deivydas Vasiliauskas. For that reason the Data Structure, Method approaches used for each functionalities will guaranteed to look similar. 

{--
 MAIN FUNCTIONALITY  
 1. Add MP3 to MP3-Player:                      IMPLEMENTED
 2. Remove and MP3:                             IMPLEMENTED
 3. Search an MP3 Based (TITLE, GENRE, ARTIST): IMPLEMENTED
 4. Display all MP3s:                           IMPLEMENTED
 5. Show total space used:                      IMPLEMENTED
 6. Show total space available: 				IMPLEMENTED
 7. Play an MP3: 								IMPLEMENTED
 8. Display the most played Mp3: 				IMPLEMENTED
 9. Display the least played Mp3:				IMPLEMENTED
 10. Exit the Application: 						IMPLEMENTED

 EXTRA FEATURES
 1. Load the MP3s from a file: 					IMPLEMENTED
 2. Save the MP3s to file on exit: 				IMPLEMENTED
--}

{--
 GENERAL STRUCTURE OF THE APPLICATION
 - When Application is initiated & "main" is called, it displays a title then asks you whether or not you want to read mp3s from a file
 - Depending you option it either loads the file or not going straight to Main Menu from there.
 - Then on Exiting it save all the MP3s stored on the application to a file.
 
 
 DATA STRUCTURE
 - The MP3 Player used 2 Lists... First List contains Tuples of 5 holding all the MP3 Data, Second List contains a record of the memory used of by each Mp3
 - Each MP3 is stored as Tuple = (Title, Artist, Genre, PlayCount, Size)
 - When a new MP3 is added, the size of the mp3 is taken and taken away from the remaining storage space and then added to the mp3PlayerSizeList
	e.g mp3 A = 10mb... Intiatial Storage set to 2000Mb for the mp3 player, the mp3 size is taken away from the from 2000Mb and added to the 2nd List 
	resulting [2000,1990].. When the next mp3 is added, it will taken away from 1990Mb (which is the remaining space)
 --}






import System.IO
import Control.Monad
import Data.Char
import System.Exit
import Data.Binary



{--
	MAIN
	 - It only gets called once, once depending on the users input it will either read the mp3 from the file or not.
	 - If data not loaded from file, the user is then asked to enter in the storage size.
 --}
main = do
	appHeading
	putStr "\nDo you want to Load MP3s from file? \nType Yes or No: " 
	userInput <- getLine
	if (userInput == "Yes") then do
		readSongList <- readFile "mp3Player_Songs.dat" -- read all th MP3 data stores it in a list [(title, artist, genre, playCount, size)]
		readSongSize <- readFile "mp3Player_Memory.dat" -- read all the memory list and stores it in a list [sizeA, sizeB]

		let songList = read readSongList
		let sizeList = read readSongSize
		
		putStrLn "MP3s Loaded from File!" 
		mp3Player songList sizeList -- calls mp3Player passing in the songList, sizeList
		
	else do	
		putStrLn "\nWelcome"
		defineSize 
		
{-- This function is called if the users choosen not to load data from file, this function define the the size of the mp3 player then passes on.--}
defineSize = do
	putStr "\nEnter the size of the MP3 player in megabytes: \t"
	size <- getLine
	let playerMemory = read size
	if all isDigit size -- isDigit is part of monads which checks if the user's input is a Numeric value.
		then do mp3Player [] [playerMemory] -- calls the function mp3Player with an empty mp3 list and passes in the user defined memory size
	else do
		putStrLn "ERROR -> Not a legal size, must be a number!"
		defineSize	





{--
 MAIN MENU 
 - The function called mp3Player is called iteratively at the end of each functions, when it is called again, either the 2 list "mp3Store & mp3PlayerSizeList"  will have been modified or will be the same. For example, when the addMP3 function is initiated, then a new mp3 has been added. The mp3Player function will be called again with the new list.
 --}
mp3Player :: [(String, String, String, Int, Double)] -> [Double] -> IO ()
mp3Player mp3Store mp3PlayerSizeList = do
	putStrLn "MAIN MENU"
	putStrLn "________________________________________________\n" --Displays the menu with the possible options.
	putStrLn "1.  Add MP3"
	putStrLn "2.  Remove MP3"
	putStrLn "3.  Search for MP3"
	putStrLn "4.  Display all MP3's stored on device"
	putStrLn "5.  Display total space used on device"
	putStrLn "6.  Display total space available on device"
	putStrLn "7.  Play MP3"
	putStrLn "8.  Display most played MP3"
	putStrLn "9.  Display least played MP3"
	putStrLn "10. Exit"
	putStrLn "________________________________________________\n"
  	userInput <- getLine
  	if (userInput == "1") then do
	{-- The addMP3 is called with mp3Store & mp3PlayerSizeList passed along as parameter to be modified and passed back --}
  		addMP3 mp3Store mp3PlayerSizeList
  	else if (userInput == "2") then do
	{-- The removeMP3 is called with mp3Store & mp3PlayerSizeList passed along as parameter to be modified and passed back --}
  		removeMP3 mp3Store mp3PlayerSizeList
	else if (userInput == "3") then do
		-- The searchMP3 is called with mp3Store passed in as a parameter. 
		searchMP3 mp3Store 
		-- After the searchMP3 function has been implemented the mp3Player function is called again as there is no callback feature for the searchMP3
		mp3Player mp3Store mp3PlayerSizeList  
	else if (userInput == "4") then do 
		-- The displayAllMp3 is called with mp3Store passed in as a parameter. Because this function only displays all the mp3s on the player, it requires a playback function that is why I am calling mp3Player.
		displayAllMp3 mp3Store
		mp3Player mp3Store mp3PlayerSizeList	
	else if (userInput == "5") then do
		displayUsedSpace mp3PlayerSizeList
		mp3Player mp3Store mp3PlayerSizeList
	else if (userInput == "6") then do
		displayRemainSpace mp3PlayerSizeList
		mp3Player mp3Store mp3PlayerSizeList	
	else if (userInput == "7") then 
		 playAnMP3 mp3Store mp3PlayerSizeList
	else if (userInput == "8") then do
		mostPlayedMP3 mp3Store
		mp3Player mp3Store mp3PlayerSizeList
	else if (userInput == "9") then do
		leastPlayedMP3 mp3Store
		mp3Player mp3Store mp3PlayerSizeList
	else if (userInput == "10") then do 
		exitApp mp3Store mp3PlayerSizeList
  	else mp3Player mp3Store mp3PlayerSizeList
	



-- FUNCTIONALITY 
{-- 
	Add MP3 to the MP3 player 
	This function takes in 2 list mp3Store & mp3PlayerSizeList from the main
	Then each input is taken from the user in order of (title, artist, genre, playCount, size)
	#Play count is set automatically to zero in the intial adding of the new mp3.
	- Once the size is inputed by user, size of the MP3 is check to see if there is enough space on the mp3 player to accomadate the MP3. If there is enough space, the new info is stored in a tuple (title, artist, genre, playCount, size) with is list. Then that list is added to the current list of mp3Player
--}

addMP3 :: [(String, String, String, Int, Double)] -> [Double] -> IO ()
addMP3 mp3List sizeList = 
	do
		
		putStrLn "ADD MP3"
		putStrLn "________________________________________________\n"
		putStrLn "Please enter the Title:"
		title <- getLine -- Holds the Title
		putStrLn "Please enter the Artist:"
		artist <- getLine -- Holds the Artist
		putStrLn "Please enter the Genre:"
		genre  <- getLine -- Holds the Genre
		putStrLn "Please enter the Size:"
		size <- getLine -- Holds the Size
		let playCount = 0 -- The play count is automatically set to 0
		let mp3Size = read size -- This variable is hold the formatted user inputed size
		
		if(last sizeList - mp3Size < 0) then do
			 putStrLn "Error, the file is too large to be added to the player..." 
			 addMP3 mp3List sizeList
		else return()
		
		let mp3 =  [(title, artist, genre, playCount, mp3Size)]
		let mp3Store = mp3List ++ mp3
		let sizeCalculation = last sizeList - mp3Size
		let mp3PlayerSizeList = sizeList ++ [sizeCalculation]

		putStrLn "\n\tSuccessfuly added MP3"
		printer mp3 
		mp3Player mp3Store mp3PlayerSizeList
		-- Once the 2 list is formed, the mp3Player is called with the 2 modified list
		

 

{--
	REMOVE AN MP3
	
	To Remove a mp3 from the player, a MP3 is searched by Title by the user. If the search is not found then the removeMP3 function is restarted. If the search term is found, that found tuple is added to a list in the variable "tupleA". But the variable newMp3List will hold all the mp3s tuples except for the one that the users searched for. At the same time the size is extracted from the found tuple from the searched term. That that size is added to the remaining space of the MP3 player. Then mp3Player function is called with the newMp3List and newSizeList. 
		
--}
removeMP3 :: [(String, String, String, Int, Double)] -> [Double] -> IO ()
removeMP3 mp3List mp3PlayerSizeList = do
	putStrLn "Enter the title of the MP3 you wish to delete!"	
	searchTerm <- getLine
	let fSearchTerm = searchTerm
	let tupleA = [(title,b,c,d,e)| (title,b,c,d,e) <- mp3List, title == fSearchTerm]
	
	if (tupleA == []) then do
		putStrLn "No Match Found"
		removeMP3 mp3List mp3PlayerSizeList
	else return ()
	
	let newMp3List = [(title,b,c,d,e)| (title,b,c,d,e) <- mp3List, title /= fSearchTerm]
	let takenSize = getSize (head tupleA)
	let spaceAdding = last mp3PlayerSizeList  + takenSize
	let newSizeList = mp3PlayerSizeList ++ [spaceAdding]
	
	putStrLn "Successfully Removed"
	printer tupleA
	mp3Player newMp3List newSizeList

{-- 
	Search for MP3
	
	The user can search the MP3 by 3 categories  Title, Artist, Genre. Once the user chooses which category they want to search by, the user then enter the appropriate search term. Which then the search term is passed to into list comprehension to searched for. The list comprehension returns the list with the matching result of the search term. Then that result list is printed using a helper function. 
	
--}

searchMP3 :: [(String, String, String, Int, Double)] -> IO ()
searchMP3 mp3Store = do
	putStrLn "\n\n\t"
	putStrLn "SEARCH BY : TITLE, ARTIST, GENRE"
	putStrLn "________________________________________________\n"
	putStrLn "1. Title"
	putStrLn "2. Artist"
	putStrLn "3. Genre"
	putStrLn "________________________________________________\n"
	userInput <- getLine
	if (userInput == "1") then do
		putStrLn "Enter the title of the song"
		userTitle <- getLine
		let titleResult = [(title,b,c,d,e)| (title,b,c,d,e) <- mp3Store, title == userTitle]
		
		if(titleResult == []) then do 
		putStrLn "\n\nNo Match Found\n\n"
		else printer titleResult
	
	else if (userInput == "2") then do
		putStrLn "Enter the artist"
		userArtist <- getLine
		let artistResult = [(a,artist,c,d,e)| (a,artist,c,d,e) <- mp3Store, artist  == userArtist]
		
		if(artistResult == []) then do 
		putStrLn "\n\nNo Match Found\n\n"
		else printer artistResult
		
	else if (userInput == "3") then do
		putStrLn "Enter the genre"
		userGenre <- getLine
		let genreResult =  [(a,b,genre,d,e)| (a,b,genre,d,e) <- mp3Store, genre == userGenre]
		
		if(genreResult == []) then do 
		putStrLn "\n\nNo Match Found\n\n"
		else printer genreResult
		
	else do
		searchMP3 mp3Store


{--
	Display all Stored MP3s
	
	The list containing all the MP3s are passed down into displayAllMp3 function which then passed into a helper function to be printed in a better format.
--}
displayAllMp3 :: [(String, String, String, Int, Double)] -> IO ()
displayAllMp3 mp3Store = printer mp3Store

 

{--
	Display Total Space Used
	
	The list containing all the memory used by each MP3 is passed into displayUsedSpace function then the head of the list is taken which was the initial Space defined at the initial stage of teh application. Then the last of the taken and then taken away from the head of the list.
--}
displayUsedSpace :: (Num a, Show a) => [a] -> IO ()
displayUsedSpace memoryList = do
	let calSpaceLeft = head memoryList - last memoryList
	putStr "\nTotal Memory Used: "
	print calSpaceLeft
	
{--
	Display Remaining Space
	
	The list containing all the memory used by each MP3 is passed into displayRemainSpace function then the last of the taken and then displayed. The last of that list always hold the remainSpace after each MP3's space is taken up.
--}
displayRemainSpace :: (Num a, Show a) => [a] -> IO ()	
displayRemainSpace memoryList = do
	let remainSpace = last memoryList
	putStr "\nRemaining Memory: "
	print remainSpace 


playAnMP3 :: [(String, String, String, Int, Double)] -> [Double] -> IO ()
playAnMP3 mp3List sizeList = do
	putStrLn "\n\n\t"
	putStrLn "Play MP3"
	putStrLn "1. Enter the song position in the playlist"
	putStrLn "2. Enter the song by title"
	userOption <-getLine
	if (userOption == "1") then do
		putStrLn "Enter the index of Song: "
		indexNumber <- getLine
		let posNum = (read indexNumber ::Int)
		let songPlay = [(a,b,c,playCount+1,e)| (a,b,c,playCount,e) <- mp3List, (a,b,c,playCount,e) == mp3List !! (posNum - 1)]
		let playingSong = head(songPlay)
		putStrLn "\n\nNow Playing...."
		printer songPlay
		let replacedList = replaceNth (posNum) playingSong mp3List
		mp3Player replacedList sizeList

	else if (userOption == "2") then do
		putStrLn "Enter the Title of Song: "
		songT <- getLine
		let songTitle = songT
		let matchedResult = [(title,b,c,d,e)| (title,b,c,d,e) <- mp3List, title == songTitle]
		let cloneResult = [(title,b,c,d+1,e)| (title,b,c,d,e) <- mp3List, title == songTitle]
		let playingSong = head(matchedResult)
		let cloneTuple = head(cloneResult)
		let newTupleIndex = getTupleIndex playingSong mp3List 0
		putStrLn "\n\nNow Playing...."
		printer cloneResult
		let replacedList = replaceNth (newTupleIndex) cloneTuple mp3List
		mp3Player replacedList sizeList
	else
		playAnMP3 mp3List sizeList

    
getTupleIndex :: (Eq a, Num a1) => a -> [a] -> a1 -> a1
getTupleIndex _ [] count	= count
getTupleIndex x (y:ys) count
	| x == y	= count
	| otherwise = getTupleIndex x ys count+1
	
	
replaceNth :: (Eq a, Num a) => a -> a1 -> [a1] -> [a1]	
replaceNth n newVal (x:xs)
     | n == 0 = newVal:xs
     | otherwise = x:replaceNth (n-1) newVal xs


{--
	DISPLAY THE LEAST PLAYED MP3
	
	To Get the least played MP3. It uses the recurrsive minimum function to find the maximum number in a list. But here the criteria is that it match the playCount of the current tuple with rest of the playCount in the other tuples and once the MP3 with the lowest playCount is found then displays it using a printer function I created. 

--}
leastPlayedMP3 :: [(String, String, String, Int, Double)] -> IO ()
leastPlayedMP3 mp3List = printer([getLeastPlayed mp3List])

getLeastPlayed :: Ord playCount => [(title, artist, genre, playCount, size)] -> (title, artist, genre, playCount, size)
getLeastPlayed []     = error "No Songs in the List"
getLeastPlayed (x:xs) = minTail x xs
  where minTail currentMin [] = currentMin
        minTail (title, artist, genre, playCount, size) (p:ps)
          | playCount > (getPlayCount p) = minTail p ps
          | otherwise   = minTail (title, artist, genre, playCount, size) ps

{--
	DISPLAY THE MOST PLAYED MP3
	
	To Get the most played MP3. It uses the recurrsive maximum function to find the maximum number in a list. But here the criteria is that it match the playCount of the current tuple with rest of the playCount in the other tuples once the MP3 with the lowest playCount is found then displays it using a printer function I created. 

--}
mostPlayedMP3 :: [(String, String, String, Int, Double)] -> IO ()
mostPlayedMP3 mp3List = printer([getMostPlayed mp3List])


getMostPlayed :: Ord playCount => [(title, artist, genre, playCount, size)] -> (title, artist, genre, playCount, size)
getMostPlayed []     = error "No Songs in the List"
getMostPlayed (x:xs) = maxTail x xs
  where maxTail currentMax [] = currentMax
        maxTail (title, artist, genre, playCount, size) (p:ps)
          | playCount < (getPlayCount p) = maxTail p ps
          | otherwise   = maxTail (title, artist, genre, playCount, size) ps


-- 10
exitApp :: [(String, String, String, Int, Double)] -> [Double] -> IO ()
exitApp mp3Store mp3PlayerSizeList = do
	putStrLn "\n\n\t"
	putStrLn "EXIT THE APP"
	putStrLn "________________________________________________\n"
	putStrLn "Do you want to exit the App."
	putStrLn "Type 'Yes' to exit OR 'No' to return to the Main Menu"
	userChoice <- getLine
	if (userChoice `elem` ["Yes","Y","y","yes"]) then do 
		writeFile "mp3Player_Songs.dat" (show mp3Store)
		writeFile "mp3Player_Memory.dat" (show mp3PlayerSizeList)
		putStrLn "MP3 Songs has been written to File \nExiting"
	else if (userChoice `elem` ["No","N","n","no"]) 
		then do
		putStrLn "Redirecting..."	
		mp3Player mp3Store mp3PlayerSizeList
	else exitApp mp3Store mp3PlayerSizeList


-- Heading for the application, needed to escape every \ with another \ in order to render.
appHeading :: IO ()
appHeading = do 
	putStrLn "  __  __ _____ ____    _____  _                       "
	putStrLn " |  \\/  |  __ \\___ \\  |  __ \\| |                      "
	putStrLn " | \\  / | |__) |__) | | |__) | | __ _ _   _  ___ _ __ "
	putStrLn " | |\\/| |  ___/|__ <  |  ___/| |/ _` | | | |/ _ \\ '__|"
	putStrLn " | |  | | |    ___) | | |    | | (_| | |_| |  __/ |   "
	putStrLn " |_|  |_|_|   |____/  |_|    |_|\\__,_|\\__, |\\___|_|   "
	putStrLn "                                       __/ |          "
	putStrLn "                                      |___/           "

	
	

-- PRINTING FUNCTION
printer :: [(String, String, String, Int, Double)] -> IO ()
printer xs = putStrLn . unlines . map printer_helper $ xs

printer_helper :: (String, String, String, Int, Double) -> String
printer_helper (title, artist, genre, playCount, size) = 
	"\n" ++
	"Title     : " ++ title ++ "\n" ++ 
	"Artist    : " ++ artist ++ "\n" ++ 
	"Genre     : " ++ genre ++ "\n" ++ 
	"PlayCount : " ++ show playCount ++ "\n" ++ 
	"Size      : " ++ show size ++ "\n"

-- HELPER FUNCTIONS
getTitle :: (title, artist, genre, playCount, size) -> title
getTitle (title, artist, genre, playCount, size) = title

getArtist :: (title, artist, genre, playCount, size) -> artist
getArtist (title, artist, genre, playCount, size) = artist

getGenre :: (title, artist, genre, playCount, size) -> genre
getGenre (title, artist, genre, playCount, size) = genre

getPlayCount :: (title, artist, genre, playCount, size) -> playCount
getPlayCount (title, artist, genre, playCount, size) = playCount

getSize :: (title, artist, genre, playCount, size) -> size
getSize (title, artist, genre, playCount, size) = size
















