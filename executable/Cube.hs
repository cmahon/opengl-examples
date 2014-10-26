-- Source: https://github.com/bergey/haskell-OpenGL-examples
-- Adapted to add keyboard control of the camera position

{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Control.Monad
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.GLUtil as U
import qualified Graphics.GLUtil.Camera3D as U
import qualified Linear as L
import           System.Exit
import           System.IO

import           Util hiding (errorCallback,keyCallback)
import           Paths_opengl_examples

-----------------------------------------------------------------------------

initResources :: IO Resources
initResources = do
    GL.blend $= GL.Enabled
    GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
    v <- getDataFileName "cube.v.glsl"
    f <- getDataFileName "cube.f.glsl"
    print vertices
    Resources <$> U.simpleShaderProgram v f
              <*> U.fromSource GL.ArrayBuffer vertices
              <*> U.fromSource GL.ArrayBuffer colors
              <*> U.fromSource GL.ElementArrayBuffer elements

draw :: Resources -> GLFW.Window -> IO ()
draw r win = do
    GL.clearColor $= GL.Color4 1 1 1 1
    GL.depthFunc $= Just GL.Less
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]
    (width, height) <- GLFW.getFramebufferSize win
    GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral width) (fromIntegral height))

    --t <- maybe 0 id <$> GLFW.getTime -- time in seconds since program launch
    GL.currentProgram $= (Just . U.program . shaderProgram $ r)
    U.enableAttrib (shaderProgram r) "coord3d"
    U.enableAttrib (shaderProgram r) "v_color"
    GL.bindBuffer GL.ArrayBuffer $= Just (vertBuffer r)
    U.setAttrib (shaderProgram r) "coord3d"
        GL.ToFloat $ GL.VertexArrayDescriptor 3 GL.Float 0 U.offset0
    GL.bindBuffer GL.ArrayBuffer $= Just (colorBuffer r)
    U.setAttrib (shaderProgram r) "v_color"
        GL.ToFloat $ GL.VertexArrayDescriptor 3 GL.Float 0 U.offset0
    
    kl <- keyIsPressed win GLFW.Key'Left
    kr <- keyIsPressed win GLFW.Key'Right

    let t = (if kl then (-1) else 0) + (if kr then 1 else 0)

    U.asUniform (transformM width height t) $ U.getUniform (shaderProgram r) "mvp"
    GL.bindBuffer GL.ElementArrayBuffer $= Just (elementBuffer r)
    U.drawIndexedTris (fromIntegral $ length elements)
    -- GL.drawArrays GL.Triangles 0 3 -- 3 is the number of vertices
    -- GLUtil does not yet provide a function to disable attributes
    GL.vertexAttribArray (U.getAttrib (shaderProgram r) "coord3d") $= GL.Disabled
    GL.vertexAttribArray (U.getAttrib (shaderProgram r) "v_color") $= GL.Disabled

-- | Represents the shader program and its input buffers
data Resources = Resources { shaderProgram :: U.ShaderProgram
                           , vertBuffer :: GL.BufferObject
                           , colorBuffer :: GL.BufferObject
                           , elementBuffer :: GL.BufferObject
                           }

transformM :: Int -> Int -> Double -> L.M44 GL.GLfloat
transformM width height t = projection L.!*! view L.!*! model L.!*! anim where
  angle      = realToFrac t * pi/4
  anim       = L.mkTransformation (L.axisAngle (L.V3 0 1 0) angle) L.zero
  model      = L.mkTransformationMat L.eye3 $ L.V3 0 0 (-4)
  view       = U.camMatrix cam
  cam        = U.tilt (-30) . U.dolly (L.V3 0 2 (realToFrac t)) $ U.fpsCamera
  projection = U.projectionMatrix (pi/4) aspect 0.1 10
  aspect     = fromIntegral width / fromIntegral height

-- This does not result in the same face order as the C++ example.
-- The first 4 vertices correspond to the right (positive X) face.
vertices :: [L.V3 Float]
vertices = L.V3 <$> [1, -1] <*> [1, -1] <*> [1, -1]

colors :: [L.V3 Float]
colors = vertices -- color space visualization

-- Vertices for each triangle in CCW order
elements :: [L.V3 GL.GLuint]
elements = [ L.V3 2 1 0 -- right
           , L.V3 1 2 3
           , L.V3 0 1 4 -- top
           , L.V3 4 1 5
           , L.V3 4 5 6 -- left
           , L.V3 7 6 5
           , L.V3 2 6 3 -- bottom
           , L.V3 6 3 7
           , L.V3 0 4 2 -- front
           , L.V3 2 4 6
           , L.V3 5 1 7 -- back
           , L.V3 7 1 3
           ]

-- type ErrorCallback = Error -> String -> IO ()
errorCallback :: GLFW.ErrorCallback
errorCallback _ = hPutStrLn stderr

keyCallback :: GLFW.KeyCallback
keyCallback window key _ action _ = when (key == GLFW.Key'Escape && action == GLFW.KeyState'Pressed) $
  GLFW.setWindowShouldClose window True

initialize :: String -> IO GLFW.Window
initialize title = do
  GLFW.setErrorCallback (Just errorCallback)
  successfulInit <- GLFW.init
  -- if init failed, we exit the program
  if not successfulInit then exitFailure else do
      GLFW.windowHint $ GLFW.WindowHint'OpenGLDebugContext True
      GLFW.windowHint $ GLFW.WindowHint'DepthBits 16
      mw <- GLFW.createWindow 640 480 title Nothing Nothing
      case mw of
          Nothing -> GLFW.terminate >> exitFailure
          Just window -> do
              GLFW.makeContextCurrent mw
              GLFW.setKeyCallback window (Just keyCallback)
              return window

cleanup :: GLFW.Window -> IO ()
cleanup win = do
    GLFW.destroyWindow win
    GLFW.terminate
    exitSuccess

mainLoop :: IO () -> GLFW.Window -> IO ()
mainLoop draw w = do
    close <- GLFW.windowShouldClose w
    unless close $ do
                    draw
                    GLFW.swapBuffers w
                    GLFW.pollEvents
                    mainLoop draw w

-----------------------------------------------------------------------------

main :: IO ()
main = do
    win <- initialize "My First Cube"
    prog <- initResources
    mainLoop (draw prog win) win
    cleanup win
    