module LastHomework where

--Goodbye <3

import Data.List

type Username = String
type Password = String

data Privilege = Simple | Admin
  deriving (Eq, Show)
  
data Cookie = LoggedOut | LoggedIn Username Privilege
  deriving (Eq, Show)
  
data Entry = Entry Password Privilege [Username]
  deriving (Eq, Show)
  
type Database = [(Username, Entry)]

-- ___________________________________________________________________________________ --

richard, charlie, carol, david, kate :: (Username, Entry)
richard = ("Richard", Entry "password1" Admin  ["Kate"])
charlie = ("Charlie", Entry "password2" Simple ["Carol"])
carol   = ("Carol",   Entry "password3" Simple ["David", "Charlie"])
david   = ("David",   Entry "password4" Simple ["Carol"])
kate    = ("Kate",    Entry "password5" Simple ["Richard"])

testDB :: Database
testDB = [ richard, charlie, carol, david, kate ]

testDBWithoutCarol :: Database
testDBWithoutCarol =
  [ ("Richard", Entry "password1" Admin  ["Kate"])
  , ("Charlie", Entry "password2" Simple [])
  , ("David",   Entry "password4" Simple [])
  , ("Kate",    Entry "password5" Simple ["Richard"])
  ]
  
  
--      1.

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _       = True
 
 
fromJust :: Maybe a -> a
fromJust (Just x) = x
fromJust _        = error "error"


catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes ms = map fromJust $ filter isJust ms 


mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ [] = []
mapMaybe f ls = catMaybes $ map f ls


safeHead :: [a] -> Maybe a
safeHead []       = Nothing
safeHead (x : xs) = Just x


password :: Entry -> Password
password ( Entry pw _ _ ) = pw


privilege :: Entry -> Privilege
privilege (Entry _ pr _) = pr


friends :: Entry -> [Username]
friends (Entry _ _ frs) = frs 


mkCookie :: Username -> Password -> Entry -> Cookie
mkCookie un pw (Entry epw epr _) = if (pw == epw) then LoggedIn un epr else LoggedOut


login :: Username -> Password -> Database -> Cookie
login un pw dbs
    | (isJust $ lookup un dbs) && (pwcheck pw $ fromJust $ lookup un dbs) = mkCookie un pw $ fromJust $ lookup un dbs   
    | otherwise = LoggedOut
    where
    pwcheck pw (Entry epw _ _) = pw == epw

 
updateEntry :: Username -> (Username, Entry) -> Maybe (Username, Entry)
updateEntry un (dbun, (Entry pw pr friends)) = if (un /= dbun) then Just (dbun, (Entry pw pr (filter (\u -> u /= un) friends))) else Nothing

  
deleteUser :: Cookie -> Username -> Database -> Database
deleteUser _  _ [] = []
deleteUser (LoggedIn _ Admin) un db = mapMaybe (\entr -> updateEntry un entr ) db
deleteUser _                  _  db = db


getFriends :: Username -> Database -> [Username]
getFriends un db = if (isJust $ lookup un db) then friends $ fromJust $ lookup un db else []


getFriendsRefl :: Username -> Database -> [Username]
getFriendsRefl un db = if (isJust $ lookup un db) then un : (friends $ fromJust $ lookup un db) else []


fixPoint :: Eq a => (a -> a) -> a -> a
fixPoint fv arg = if (fv arg /= arg) then fixPoint fv (fv arg) else arg


sortUnique :: (Eq a, Ord a) => [a] -> [a]
sortUnique ls = nub $ sort ls


getSocialNetwork :: Username -> Database -> [Username]
getSocialNetwork _ [] = []
getSocialNetwork un db = sortUnique $ concat $ map (\x -> getFriendsRefl x db) (getFriendsRefl un db)