{-# LANGUAGE OverloadedStrings, OverloadedLabels, ScopedTypeVariables, LambdaCase #-}

module GUI where

import           Data.Text                     as T
import qualified Data.Map                      as Map
import qualified GI.Gtk                        as Gtk
import           Control.Monad

import           Files
import           Parser
import           Utils
import           FEM

import           Graphics.Rendering.Chart.Backend.Diagrams
import           Graphics.Rendering.Chart.Easy

gladeFile :: FilePath
gladeFile = "gui/fem.glade"

gui :: [String] -> IO ()
gui args = do
  _ <- Gtk.init Nothing
  putStrLn "creating builder"
  builder          <- Gtk.builderNewFromFile (pack gladeFile)

  Just window      <- getObj builder Gtk.Window "window"
  Just image       <- getObj builder Gtk.Image "plot"
  Just resetButton <- getObj builder Gtk.Button "reset"
  Just solveButton <- getObj builder Gtk.Button "solve"
  entries <- mapM (getObj builder Gtk.Entry) names >>= \entries -> return
    $ Map.fromList [ (n, en) | (n, Just en) <- Prelude.zip names entries ]

  values <- liftM
    (\case
      Right m -> m
      Left  _ -> Map.empty
    )
    (parseFile args)
  _ <- return $ Map.mapWithKey
    (\key text -> updateEntryText <$> (Map.lookup key entries) <*> Just text)
    values

  _ <- Gtk.afterButtonClicked resetButton $ onResetButtonClicked image entries

  _ <- Gtk.afterButtonClicked solveButton $ onSolveButtonClicked image entries

  _ <- Gtk.onWidgetDestroy window Gtk.mainQuit

  Gtk.widgetShowAll window
  Gtk.main
 where
  names = ["a(x)", "b(x)", "c(x)", "f(x)", "ur", "k", "l", "n"]
  getObj builder gtkConstr name = Gtk.builderGetObject builder name >>= \case
    Just obj -> Gtk.castTo gtkConstr obj
    Nothing  -> do
      putStrLn . unpack $ "Object named '" <> name <> "' could not be found."
      return Nothing

updateEntryText :: Gtk.Entry -> Text -> IO ()
updateEntryText entry text = do
  buffer <- Gtk.entryBufferNew (Just text) (fromIntegral $ T.length text)
  Gtk.entrySetBuffer entry buffer

onResetButtonClicked :: Gtk.Image -> Map.Map Text Gtk.Entry -> IO ()
onResetButtonClicked image entries = do
  Gtk.imageClear image
  _ <- mapM resetText entries
  return ()
 where
  resetText entry = do
    buffer <- Gtk.entryGetBuffer entry
    Gtk.entryBufferDeleteText buffer 0 (-1)

onSolveButtonClicked :: Gtk.Image -> Map.Map Text Gtk.Entry -> IO ()
onSolveButtonClicked image entries = do
  Gtk.imageClear image
  values <- mapM getText entries
  let Just (ec, n, fName) = checkInput "guiPlot" values
      fileName            = (T.unpack fName <> ".svg")
  toFile def fileName $ plot (line "u(x)" [solve ec n])
  Gtk.imageSetFromFile image (Just fileName)
  return ()
 where
  getText entry = do
    buffer <- Gtk.entryGetBuffer entry
    Gtk.entryBufferGetText buffer

checkInput :: Text -> Map.Map Text Text -> Maybe (EdgeCond Double, Int, Text)
checkInput fileName values = do
  a  <- checkFunc $ getFunc "a(x)"
  b  <- checkFunc $ getFunc "b(x)"
  c  <- checkFunc $ getFunc "c(x)"
  f  <- checkFunc $ getFunc "f(x)"
  n  <- checkNum $ getNum "n"
  k  <- checkNum $ getNum "k"
  l  <- checkNum $ getNum "l"
  ur <- checkNum $ getNum "ur"
  return (EC a b c f k l ur, n, T.takeWhile (/= '.') fileName)
 where
  getFunc key = parseRPN . parseToRPN <$> Map.lookup key values
  getNum key = reads <$> T.unpack <$> Map.lookup key values

checkNum :: (Num a, Read a) => Maybe ([(a, String)]) -> Maybe a
checkNum (Just [(n, "")]) = Just n
checkNum _                = Nothing

checkFunc :: Maybe (Either Text DFunc) -> Maybe DFunc
checkFunc (Just (Right f)) = return f
checkFunc (Just (Left  _)) = Nothing
checkFunc Nothing          = Nothing
