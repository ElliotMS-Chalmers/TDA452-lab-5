module Lab5 where
import Data.Char ( toLower )
import Text.Read ( readMaybe )
import System.IO ( stdout, hFlush )
import System.IO.Error ( tryIOError )

data QA = Question String QA QA | Person String deriving (Read, Show)

defaultTree = Question "Is this person from Europe?" 
                (Question "Is this person a scientist?" 
                    (Person "Marie Curie") 
                    (Person "Queen Lizzie")) 
                (Question "Is this person an actor?" 
                    (Person "Marylyn Monroe") 
                    (Person "Hillary Clinton")) 

main :: IO () 
main = do 
    tree <- loadTree
    newTree <- game tree
    saveTree newTree 

game :: QA -> IO QA 
game tree = do
    updatedTree <- play tree
    putStr "Do you want to play again? "
    hFlush stdout
    input <- getLine
    case map toLower input of 
        "yes" -> game updatedTree
        "no"  -> return updatedTree
        _     -> do badYesNo
                    game tree

loadTree :: IO QA
loadTree = do 
    result <- tryIOError (readFile "questions.qa")
    case result of
      Left _ -> return defaultTree
      Right content -> case readMaybe content of
        Just tree ->  return tree
        Nothing ->  return defaultTree

saveTree :: QA -> IO()
saveTree tree = writeFile "questions.qa" (show tree)

play :: QA -> IO QA
play (Question curr yes no) = do
    putStr (curr ++ " ")
    hFlush stdout
    input <- getLine 
    case map toLower input of 
        "yes" -> do 
            updatedYes <- play yes
            return (Question curr updatedYes no)
        "no"  -> do 
            updatedNo <- play no
            return (Question curr yes updatedNo)
        _     -> do
            badYesNo
            play (Question curr yes no)
play (Person p) = do 
    putStr ("My guess: Is it " ++ p ++ "? ")
    hFlush stdout
    input <- getLine 
    case map toLower input of 
        "yes" -> return (Person p)
        "no"  -> do
            putStr "Just curious: Who was your famous person? "
            hFlush stdout
            person <- getLine
            putStrLn ("Give me a question for which the answer for " ++ person ++ " is \"yes\" and the answer for " ++ p ++ " is \"no\".")
            hFlush stdout
            newQuestion <- getLine
            return (Question newQuestion (Person person) (Person p))
        _  -> do
            badYesNo
            play (Person p) 

badYesNo :: IO ()
badYesNo = putStrLn "Please answer with \"yes\" or \"no\"!"
