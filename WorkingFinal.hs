import System.IO
import "gtk3" Graphics.UI.Gtk hiding (response)
import "gtk3" Graphics.UI.Gtk.Builder
import "gtk3" Graphics.UI.Gtk.Buttons.Button
import Data.Char
import Control.Monad
import System.Exit
-------------------

--Methods---

-- Update next player
-- No Params
-- Returns IO (Writes to File)
setNextPlayer :: IO()
setNextPlayer = do
    str <- getCurPlayer
    if(str == "X")
        then writeFile "WorkingFinal.txt" "O"
    else writeFile "WorkingFinal.txt" "X"

-- Gets Current Player Name
-- No Params
-- Returns IO String (String of the current player turn (X or O))
getCurPlayer :: IO String
getCurPlayer = do
    handle <- openFile "WorkingFinal.txt" ReadMode
    contents <- hGetLine handle
    hClose handle
    return contents


-- Gets the Button label
-- Params: ButtonClass -> Btn
-- Returns IO String (String of the button's label)
btnGetLabel :: (ButtonClass o) => o -> IO String
btnGetLabel btn = do
        label <- buttonGetLabel btn
        return label

-- Does action on click of the button
-- Checks if the button here is clickable(it's initial value is '-')
-- and if it is, then we will set the label to the current player.
-- if Not we will do nothing.
-- Params: ButtonClass -> Btn
-- Returns IO() (Change of the button label results in IO())
btnDoAction :: (ButtonClass o) => o -> IO()
btnDoAction btn = do
    let cPlayer = "X"
    label <- btnGetLabel btn
    curPlayer <- getCurPlayer
    if label == "-"
        then 
            buttonSetLabel btn curPlayer
    else 
        return ()
-- Check if the action is legit + Setting the next player name
-- Calls the btnDoAction then setNextPlayer IF (the label is equal to '-'(EMPTY- NO PLAYER))
-- else returns empty IO()
btnActualAction :: (ButtonClass o) => o -> IO()
btnActualAction btn = do
    label <- btnGetLabel btn
    if(label == "-") 
        then
            do
                btnDoAction btn
                setNextPlayer
    else return ()

-- Fail try
-- Issue: IO String conversion to String within list comprehension
-- btnMWinner :: (ButtonClass o) => [o] -> IO Char
-- btnMWinner a = do
    -- let fstRow = length [(a !! i) | i <- [0,1,2], (btnGetLabel (a !! i)) == 'X']
--     return 'x'


-- Check if there's a winner within rows, columns and the two diagonals
-- Params: 9 Button Class -> 9 buttons
-- Returns: IO Char
--          'X' -> Winner is X
--          'O' -> Winner is O
--          'N' -> Winner is None - TIE
--          '-' -> No winner yet 
btnMyWinner :: (ButtonClass o) => o -> o -> o -> o -> o -> o -> o -> o -> o-> IO Char
btnMyWinner a b c d e f g h i = do
    lbl1 <- btnGetLabel a
    lbl2 <- btnGetLabel b
    lbl3 <- btnGetLabel c
    lbl4 <- btnGetLabel d
    lbl5 <- btnGetLabel e
    lbl6 <- btnGetLabel f
    lbl7 <- btnGetLabel g
    lbl8 <- btnGetLabel h
    lbl9 <- btnGetLabel i
    -- Rows
    if(lbl1 == lbl2  && lbl2 == lbl3 && lbl1 /= "-")
        then do
            if(lbl1 == "X")
                then return 'X'
            else return 'O'
    else if(lbl4 == lbl5 && lbl5 == lbl6 && lbl4 /= "-")
        then do
            if(lbl4 == "X")
                then return 'X'
            else return 'O'
    else if(lbl7 == lbl8 && lbl8 == lbl9 && lbl7 /= "-")
        then do
            if(lbl7 == "X")
                then return 'X'
            else return 'O'
    -- Columns
    else if(lbl1 == lbl4 && lbl4 == lbl7 && lbl1 /= "-")
        then do
            if(lbl1 == "X")
                then return 'X'
            else return 'O'
    else if(lbl2 == lbl5 && lbl5 == lbl8 && lbl2 /= "-")
        then do
            if(lbl2 == "X")
                then return 'X'
            else return 'O'
    else if(lbl3 == lbl6 && lbl6 == lbl9 && lbl9 /= "-")
        then do
            if(lbl3 == "X")
                then return 'X'
            else return 'O'
    --first diagonal /
    else if(lbl3 == lbl5 && lbl5 == lbl7 && lbl3 /= "-")
        then do
            if(lbl3 == "X")
                then return 'X'
            else return 'O'
    --second diagonal \
    else if(lbl1 == lbl5 && lbl5 == lbl9 && lbl1 /= "-")
        then do
            if(lbl1 == "X")
                then return 'X'
            else return 'O'
    else if(lbl1 /= "-" && lbl2 /= "-" && lbl3 /= "-" && lbl4 /= "-" && lbl5 /= "-" && lbl6 /= "-" && lbl7 /= "-" && lbl8 /= "-" && lbl9 /= "-")
        then return 'N'
    else return '-'


-- Displays Dialog Message for the state of the game(winner,tie)
-- Params:
-- Title: Title of the dialog
-- Message: Text Message in the dialog
-- Returns: IO()
dialogMessage title msg =  do 
    dialog <- messageDialogNew Nothing [] MessageInfo ButtonsClose msg
    set dialog [windowTitle := title]
    widgetShowAll dialog
    dialogRun dialog
    widgetDestroy dialog

-- Displays whether there's a winner or not.
-- Params: 9 Buttons
-- Returns IO() (Dialog + ExitSuccess)
btnIsWinner :: (ButtonClass o) => o -> o -> o -> o -> o -> o -> o -> o -> o-> IO()
btnIsWinner a b c d e f g h i = do
    winner <- btnMyWinner a b c d e f g h i
    if (winner == 'N')
        then do
            (dialogMessage "Looks like it's a tie!" ("We have no winner,\n It's a tie!"))
            exitSuccess
    else if(winner /= '-')
        then do
            putStr ("Winner is: ")
            print winner

            (dialogMessage "We have a winner!" ("Congratulations, player " ++ (show winner) ++ "\nYOU WON!"))
            exitSuccess
    else return ()
 
--TODO: Set disabled 
main :: IO ()
main = do
    -- Initialize the program with player X(With write to file(NO APPEND))
    writeFile "WorkingFinal.txt" "X"
    -- GUI Part (Window + onClicked signal)
    initGUI
    builder <- builderNew
    builderAddFromFile builder "WorkingFinal.glade"
    let onClicked obj = on obj buttonActivated
    mainWindow <- builderGetObject builder castToWindow "main_window"
    set mainWindow [windowDefaultWidth := 600, windowDefaultHeight := 500, containerBorderWidth := 2]

    -- Initialize Buttons--
    btn0 <- builderGetObject builder castToButton "btn0"
    btn1 <- builderGetObject builder castToButton "btn1"
    btn2 <- builderGetObject builder castToButton "btn2"
    btn3 <- builderGetObject builder castToButton "btn3"
    btn4 <- builderGetObject builder castToButton "btn4"
    btn5 <- builderGetObject builder castToButton "btn5"
    btn6 <- builderGetObject builder castToButton "btn6"
    btn7 <- builderGetObject builder castToButton "btn7"
    btn8 <- builderGetObject builder castToButton "btn8"

    -- Create a list of the buttons (no usage, part of a trial) 
    let listOfButtons = [btn0, btn1, btn2, btn3, btn4, btn5, btn6, btn7, btn8]
    
    -- Start Button signal (OnClicked) on our 9 buttons
    btn0 `onClicked` (do
    btnActualAction btn0
    btnIsWinner btn0 btn1 btn2 btn3 btn4 btn5 btn6 btn7 btn8
    -- if(whereWinner == 'XQ
    )

    btn1 `onClicked` (do
    btnActualAction btn1 
    btnIsWinner btn0 btn1 btn2 btn3 btn4 btn5 btn6 btn7 btn8
    )

    btn2 `onClicked` (do
    btnActualAction btn2 
    btnIsWinner btn0 btn1 btn2 btn3 btn4 btn5 btn6 btn7 btn8
    )

    btn3 `onClicked` (do
    btnActualAction btn3 
    btnIsWinner btn0 btn1 btn2 btn3 btn4 btn5 btn6 btn7 btn8
    )

    btn4 `onClicked` (do
    btnActualAction btn4 
    btnIsWinner btn0 btn1 btn2 btn3 btn4 btn5 btn6 btn7 btn8
    )

    btn5 `onClicked` (do
    btnActualAction btn5 
    btnIsWinner btn0 btn1 btn2 btn3 btn4 btn5 btn6 btn7 btn8
    )

    btn6 `onClicked` (do
    btnActualAction btn6 
    btnIsWinner btn0 btn1 btn2 btn3 btn4 btn5 btn6 btn7 btn8
    )

    btn7 `onClicked` (do
    btnActualAction btn7 
    btnIsWinner btn0 btn1 btn2 btn3 btn4 btn5 btn6 btn7 btn8
    )

    btn8 `onClicked` (do
    btnActualAction btn8 
    btnIsWinner btn0 btn1 btn2 btn3 btn4 btn5 btn6 btn7 btn8
    )
    
    -- End Button onClicked on the 9 buttons

    on mainWindow objectDestroy mainQuit
    widgetShowAll mainWindow
    mainGUI

