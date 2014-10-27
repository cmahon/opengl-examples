-- Source: https://github.com/bsl/GLFW-b-demo
-- Adapted to move a basic object around the screen in 2D

{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

--------------------------------------------------------------------------------

import           Control.Concurrent.STM    (TQueue, atomically, newTQueueIO,
                                            tryReadTQueue)
import           Control.Lens
import           Control.Monad             (unless, void, when)
import           Control.Monad.RWS.Strict  (RWST, asks, evalRWST, liftIO)
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW          as GLFW

import           Util

--------------------------------------------------------------------------------

data Env = Env
  { envEventsChan    :: TQueue Event
  , envWindow        :: !GLFW.Window
  }

type Pos = GL.Vector2 GL.GLdouble

data State = State
  { _stateWindowWidth     :: Int
  , _stateWindowHeight    :: Int
  , _statePlayer          :: Pos
  }

makeLenses ''State

type Demo = RWST Env () State IO

-----------------------------------------------------------------------------

playerSize :: GL.GLdouble
playerSize = 20

playerSpeed :: Int
playerSpeed = 10

--------------------------------------------------------------------------------

runDemo :: Env -> State -> IO ()
runDemo env state = 
  void $ evalRWST (adjustWindow >> run) env state

run :: Demo ()
run = do
  
  win <- asks envWindow
  
  liftIO GLFW.pollEvents
  
  processEvents
  
  l <- liftIO $ keyIsPressed win GLFW.Key'Left
  r <- liftIO $ keyIsPressed win GLFW.Key'Right
  u <- liftIO $ keyIsPressed win GLFW.Key'Up
  d <- liftIO $ keyIsPressed win GLFW.Key'Down
  
  let xd = (if l then (-1) else 0) + (if r then 1 else 0)
      yd = (if d then (-1) else 0) + (if u then 1 else 0)
  
  (GL.Vector2 xpos ypos) <- statePlayer <%= (\(GL.Vector2 x y) -> 
    GL.Vector2
     (x + (fromIntegral (xd * playerSpeed)))
     (y + (fromIntegral (yd * playerSpeed))))

  liftIO $ do
    GL.clear [GL.ColorBuffer]
    GL.color $ GL.Color4 0 0 0 (1 :: GL.GLfloat)
    GL.renderPrimitive GL.Quads $ do
      GL.vertex $ GL.Vertex2 (xpos - playerSize/2) (ypos - playerSize/2)
      GL.vertex $ GL.Vertex2 (xpos + playerSize/2) (ypos - playerSize/2)
      GL.vertex $ GL.Vertex2 (xpos + playerSize/2) (ypos + playerSize/2)
      GL.vertex $ GL.Vertex2 (xpos - playerSize/2) (ypos + playerSize/2)
    GL.color $ GL.Color4 1 1 1 (1 :: GL.GLfloat)
    GL.flush
    GLFW.swapBuffers win
  
  q <- liftIO $ GLFW.windowShouldClose win
  
  unless q run

-----------------------------------------------------------------------------

adjustWindow :: Demo ()
adjustWindow = do
  width <- use stateWindowWidth
  height <- use stateWindowHeight
  let pos   = GL.Position 0 0
      size  = GL.Size (fromIntegral width) (fromIntegral height)
  liftIO $ do
    GL.viewport   GL.$= (pos, size)
    GL.matrixMode GL.$= GL.Projection
    GL.loadIdentity

-----------------------------------------------------------------------------

processEvents :: Demo ()
processEvents = do
  tc <- asks envEventsChan
  me <- liftIO $ atomically $ tryReadTQueue tc
  case me of
    Just e -> do
      processEvent e
      processEvents
    Nothing -> return ()

processEvent :: Event -> Demo ()
processEvent ev = case ev of
  
  (EventError e s) -> do
      printEvent "error" [show e, show s]
      win <- asks envWindow
      liftIO $ GLFW.setWindowShouldClose win True

  (EventWindowPos _ x y) ->
      printEvent "window pos" [show x, show y]

  (EventWindowSize _ width height) ->
      printEvent "window size" [show width, show height]

  (EventWindowClose _) ->
      printEvent "window close" []

  (EventWindowRefresh _) ->
      printEvent "window refresh" []

  (EventWindowFocus _ fs) ->
      printEvent "window focus" [show fs]

  (EventWindowIconify _ is) ->
      printEvent "window iconify" [show is]

  (EventFramebufferSize _ width height) -> do
      printEvent "framebuffer size" [show width, show height]
      stateWindowWidth .= width
      stateWindowHeight .= height
      adjustWindow

  (EventMouseButton _ mb mbs mk) ->
      printEvent "mouse button" [show mb, show mbs, showModifierKeys mk]

  (EventCursorPos _ x y) -> do
      let x' = round x :: Int
          y' = round y :: Int
      printEvent "cursor pos" [show x', show y']

  (EventCursorEnter _ cs) ->
      printEvent "cursor enter" [show cs]

  (EventScroll _ x y) -> do
      let x' = round x :: Int
          y' = round y :: Int
      printEvent "scroll" [show x', show y']
      adjustWindow

  (EventKey win k scancode ks mk) -> do
      printEvent "key" [show k, show scancode, show ks, showModifierKeys mk]
      when (ks == GLFW.KeyState'Pressed) $ do
          when (k == GLFW.Key'Q || k == GLFW.Key'Escape) $
            liftIO $ GLFW.setWindowShouldClose win True
          when (k == GLFW.Key'Slash && GLFW.modifierKeysShift mk) $
            liftIO printInstructions
          when (k == GLFW.Key'I) $
            liftIO $ printInformation win

  (EventChar _ c) ->
      printEvent "char" [show c]

printEvent :: String -> [String] -> Demo ()
printEvent cbname fields =
  liftIO $ putStrLn $ cbname ++ ": " ++ unwords fields

-----------------------------------------------------------------------------

main :: IO ()
main = do
  
  let width  = 640
      height = 480

  eventsChan <- newTQueueIO :: IO (TQueue Event)

  withWindow width height "Demo" $ \win -> do
        
    GLFW.setErrorCallback               $ Just $ errorCallback           eventsChan
    GLFW.setWindowPosCallback       win $ Just $ windowPosCallback       eventsChan
    GLFW.setWindowSizeCallback      win $ Just $ windowSizeCallback      eventsChan
    GLFW.setWindowCloseCallback     win $ Just $ windowCloseCallback     eventsChan
    GLFW.setWindowRefreshCallback   win $ Just $ windowRefreshCallback   eventsChan
    GLFW.setWindowFocusCallback     win $ Just $ windowFocusCallback     eventsChan
    GLFW.setWindowIconifyCallback   win $ Just $ windowIconifyCallback   eventsChan
    GLFW.setFramebufferSizeCallback win $ Just $ framebufferSizeCallback eventsChan
    GLFW.setMouseButtonCallback     win $ Just $ mouseButtonCallback     eventsChan
    GLFW.setCursorPosCallback       win $ Just $ cursorPosCallback       eventsChan
    GLFW.setCursorEnterCallback     win $ Just $ cursorEnterCallback     eventsChan
    GLFW.setScrollCallback          win $ Just $ scrollCallback          eventsChan
    GLFW.setKeyCallback             win $ Just $ keyCallback             eventsChan
    GLFW.setCharCallback            win $ Just $ charCallback            eventsChan

    GL.lineSmooth GL.$= GL.Enabled
    GL.blend      GL.$= GL.Enabled
    GL.blendFunc  GL.$= (GL.SrcAlpha,GL.OneMinusSrcAlpha)
    GL.lineWidth  GL.$= 2.0
    GL.clearColor GL.$= GL.Color4 1 1 1 1
    GL.viewport GL.$= (GL.Position 0 0, GL.Size (fromIntegral width) (fromIntegral height))
    GL.ortho 0 (fromIntegral width) 0 (fromIntegral height) (-1) 1

    (fbWidth, fbHeight) <- GLFW.getFramebufferSize win

    let 
      env = Env
        { envEventsChan    = eventsChan
        , envWindow        = win
        }
      state = State
        { _stateWindowWidth     = fbWidth
        , _stateWindowHeight    = fbHeight
        , _statePlayer          = GL.Vector2
            (fromIntegral $ div fbWidth 4)
            (fromIntegral $ div fbHeight 4)
        }
        
    runDemo env state

    putStrLn "Done"

-----------------------------------------------------------------------------
-- Experiments

-- GLFW.swapInterval 1
-- GL.position (GL.Light 0) GL.$= GL.Vertex4 5 5 10 0
-- GL.light    (GL.Light 0) GL.$= GL.Enabled
-- GL.lighting   GL.$= GL.Enabled
-- GL.cullFace   GL.$= Just GL.Back
-- GL.depthFunc  GL.$= Just GL.Less
-- GL.clearColor GL.$= GL.Color4 0.05 0.05 0.05 1
-- GL.normalize  GL.$= GL.Enabled
