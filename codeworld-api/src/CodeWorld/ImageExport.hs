module CodeWorld.ImageExport where


import Graphics.Blank (
  Canvas,
  DeviceContext,
  ImageData,
  clearCanvas,
  getImageData,
  send,
  toDataURL,
  width,
  height,
  )
import CodeWorld.CanvasM (
  runCanvasM,
  saveRestore,
  )
import CodeWorld.Driver                 (drawFrame, setupScreenContext)
import CodeWorld.Picture                (Picture)
import Data.Text                        (Text)



renderPictureDataURL
  :: Picture
  -> DeviceContext
  -> IO Text
renderPictureDataURL = renderPictureData $ const $ const $ toDataURL ()


renderPicturePixels
  :: Picture
  -> DeviceContext
  -> IO ImageData
renderPicturePixels = renderPictureData
  (\w h -> getImageData (0, 0, fromIntegral w, fromIntegral h))


renderPictureData
  :: (Int -> Int -> Canvas a)
  -> Picture
  -> DeviceContext
  -> IO a
renderPictureData format p ctx = do
  send ctx clearCanvas
  let w = width ctx
      h = height ctx
  runCanvasM ctx $ saveRestore $ do
    setupScreenContext w h
    drawFrame p
  send ctx $ format w h

