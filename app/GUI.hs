{-# LANGUAGE OverloadedStrings, OverloadedLabels, ScopedTypeVariables, LambdaCase #-}

module GUI where

import           Data.Text
import qualified Data.Map                      as Map
import qualified GI.Gtk                        as Gtk

gladeFile :: FilePath
gladeFile = "gui/fem.glade"

gui :: [String] -> IO ()
gui args = do
  _ <- Gtk.init Nothing
  putStrLn "creating builder"
  builder     <- Gtk.builderNewFromFile (pack gladeFile)

  Just window <- getObj builder Gtk.Window "window"
  Just plot   <- getObj builder Gtk.Image "plot"
  Just reset  <- getObj builder Gtk.Button "reset"
  Just solve  <- getObj builder Gtk.Button "solve"
  entries     <- mapM (getObj builder Gtk.Entry) names >>= \entries -> return
    $ Map.fromList [ (n, en) | (n, Just en) <- Prelude.zip names entries ]

  _ <- Gtk.afterButtonClicked reset $ onResetButtonClicked plot entries

  _ <- Gtk.afterButtonClicked solve $ onSolveButtonClicked plot entries

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

onResetButtonClicked :: Gtk.Image -> Map.Map Text Gtk.Entry -> IO ()
onResetButtonClicked plot entries = do
  Gtk.imageClear plot
  _ <- mapM resetText entries
  return ()
 where
  resetText entry = do
    buffer <- Gtk.entryGetBuffer entry
    Gtk.entryBufferDeleteText buffer 0 (-1)

onSolveButtonClicked :: Gtk.Image -> Map.Map Text Gtk.Entry -> IO ()
onSolveButtonClicked plot entries = do
  Gtk.imageClear plot
  values <- mapM getText entries

  return ()
 where
  getText entry = do
    buffer <- Gtk.entryGetBuffer entry
    Gtk.entryBufferGetText buffer
