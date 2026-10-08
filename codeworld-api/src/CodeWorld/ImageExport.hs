module CodeWorld.ImageExport where


import Graphics.Blank (
  Canvas,
  DeviceContext,
  ImageData,
  getImageData,
  send,
  toDataURL,
  with,
  )
import CodeWorld.CanvasM (
  newImage,
  runCanvasM,
  saveRestore,
  withImage,
  )
import CodeWorld.Driver                 (drawFrame, setupScreenContext)
import CodeWorld.Picture                (Picture)
import Data.Text                        (Text)



renderPictureDataURL
  :: Int
  -> Int
  -> Picture
  -> DeviceContext
  -> IO Text
renderPictureDataURL = renderPictureData $ toDataURL ()


renderPicturePixels
  :: Int
  -> Int
  -> Picture
  -> DeviceContext
  -> IO ImageData
renderPicturePixels w h = renderPictureData
  (getImageData (0, 0, fromIntegral w, fromIntegral h))
  w
  h


renderPictureData
  :: Canvas a
  -> Int
  -> Int
  -> Picture
  -> DeviceContext
  -> IO a
renderPictureData format w h p ctx = do
  offscreen <- runCanvasM ctx $ newImage w h
  runCanvasM ctx $ withImage offscreen $ saveRestore $ do
    setupScreenContext w h
    drawFrame p
  send ctx $ with offscreen format

