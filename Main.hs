module Main (main) where

import MainWindow
import Graphics.UI.Gtk

main = do
    initGUI
    
    window <- mainWindowNew
    widgetShowAll window
    
    mainGUI