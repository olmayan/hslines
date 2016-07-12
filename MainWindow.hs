module MainWindow (mainWindowNew) where

import GameLogic

import Control.Monad
import Data.Array
import Data.IORef
import Data.List (intercalate)
import Graphics.Rendering.Cairo
import qualified Graphics.Rendering.Cairo as C
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Events (Event(Button, Crossing, Expose, Motion))
--import Graphics.UI.Gtk.Gdk.GC
import System.Random.Mersenne.Pure64

data AppData = AppData {
    g         :: PureMT,
    field     :: Field,
    sel       :: (Int, Int),
    mouseAt   :: (Int, Int),
    waypoints :: [(Int, Int)]
}

mainWindowNew = do

    g <- newPureMT
    let (fld, g') = throwBalls emptyField 5 g
    appData <- newIORef $ AppData {
        g         = g',
        field     = fld,
        sel       = (-1, -1),
        mouseAt   = (-1, -1),
        waypoints = []
    }

    window <- windowNew
    widgetSetSizeRequest window 450 450
    window `onDestroy` mainQuit

    canvas <- drawingAreaNew
    canvas `onExpose` \e@(Expose sent area region count) -> do
        dw <- widgetGetDrawWindow canvas
        (w, h, ms, s0, offsetx, offsety) <- widths canvas
        (AppData _ (Field fld) sel mouse_pt waypoints) <- readIORef appData
        windowSetTitle window $ "hslines [" ++ intercalate ", " (map show waypoints) ++ "]"
        renderWithDrawable dw $ do
            let rng = range ((0, 0), (side - 1, side - 1))
            forM rng $ \pt@(x, y) -> do
                C.rectangle (offsetx + fromIntegral x * s0)
                            (offsety + fromIntegral y * s0) s0 s0
                if pt == mouse_pt || onField sel && pt `elem` waypoints then do
                    setSourceRGB 0.5 0.5 0.5
                             else do
                    setSourceRGB 0.9 0.9 0.9
                fillPreserve
                setSourceRGB 0.0 0.0 0.0
                setLineWidth 1.0
                stroke
                let n = fld ! pt
                when (n /= 0) $ do
                    arc (offsetx + (fromIntegral x + 0.5) * s0)
                        (offsety + (fromIntegral y + 0.5) * s0)
                        (s0 * 0.4) 0.0 (2 * pi)
                    let (r, g, b) = [ (1.0, 0.0, 0.0)
                                    , (1.0, 1.0, 0.0)
                                    , (0.0, 1.0, 0.0)
                                    , (0.0, 1.0, 1.0)
                                    , (0.0, 0.0, 1.0)
                                    , (1.0, 0.0, 1.0) ] !! (n - 1)
                    setSourceRGB r g b
                    fillPreserve
                    if pt == sel then do
                        setLineWidth 3.0
                        setSourceRGB 1.0 0.5 0.0
                                 else do
                        setLineWidth 1.0
                        setSourceRGB 0.0 0.0 0.0
                    stroke
        return sent

    canvas `onButtonPress` \e@(Button sent _ _ x y m btn xr yr) -> do
        (w, h, ms, s0, offsetx, offsety) <- widths canvas
        ad@(AppData g field@(Field fld) sel _ _) <- readIORef appData
        --putStrLn $ "hslines [" ++ intercalate ", " (map show $ path field (0,0) (8,8)) ++ "]"
        let pt@(xc, yc) = (floor $ (x - offsetx) / s0, floor $ (y - offsety) / s0)
            waypoints = path field sel pt -- these will evaluate if demanded
            field' = Field (fld // [(sel, 0), (pt, fld ! sel)])
            (field'', g') = throwBalls field' (min 3 $ countFree field') g
        when (onField pt && fld ! pt /= 0) $ do
            writeIORef appData ad { sel = pt }
            widgetQueueDraw canvas
        when (onField pt && onField sel && fld ! sel /= 0 && (not . null) waypoints) $ do
            writeIORef appData ad { g = g',
                                    sel = (-1, -1),
                                    field = field''
                                  }
            widgetQueueDraw canvas
        --putStrLn $ show (xc, yc)
        return sent

    onMotionNotify canvas True $ \e@(Motion sent _ x y _ _ _ _) -> do
        (w, h, ms, s0, offsetx, offsety) <- widths canvas
        ad@(AppData _ field@(Field fld) sel old_pt _) <- readIORef appData
        let pt@(xc, yc) = (floor $ (x - offsetx) / s0, floor $ (y - offsety) / s0)
        when (pt /= old_pt) $ do
            writeIORef appData ad {
                mouseAt = pt,
                waypoints = if onField sel && onField pt && fld ! pt == 0
                             then path field sel pt
                             else []
            }
            widgetQueueDraw canvas
        return sent
    
    canvas `onLeaveNotify` \e@(Crossing sent _ x y _ _ _ _ _ _) -> do
        ad <- readIORef appData
        writeIORef appData ad { mouseAt = (-1, -1), waypoints = [] }
        widgetQueueDraw canvas
        return sent

    containerAdd window canvas

    return window

widths :: WidgetClass a => a -> IO (Double, Double, Double, Double, Double, Double)
widths widget = do
    (w, h) <- widgetGetSize widget
    let w' = fromIntegral w
        h' = fromIntegral h
        ms = min w' h'
        s0 = ms / fromIntegral side
        offsetx = (w' - ms) / 2
        offsety = (h' - ms) / 2
    return (w', h', ms, s0, offsetx, offsety)
