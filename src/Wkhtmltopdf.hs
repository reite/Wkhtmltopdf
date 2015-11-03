

module Wkhtmltopdf where

import System.Process
import GHC.IO.Handle
import Control.Monad
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL




data WkSettings = WKSettings {
    wkPath     :: FilePath,
    wkEncoding :: String
}

defWkSettings :: WkSettings
defWkSettings = WKSettings {
    wkPath = "wkhtmltopdf",
    wkEncoding = "utf8"
}




convertBytestring :: WkSettings -> BL.ByteString -> IO B.ByteString
convertBytestring wks bs = do
    (Just stdin, Just stdout, _, _) <- createProcess $
        (proc (wkPath wks) opts) { std_out = CreatePipe
                                 , std_in  = CreatePipe 
                                 }

    BL.hPutStr stdin bs >> hClose stdin
    B.hGetContents stdout
    where
        opts = ["--encoding", wkEncoding wks , "-", "-"]

-- Convert the given html and return the pdf as a strict bytestring.
convertHtml :: Html -> IO B.ByteString
convertHtml = convertBytestring defWkSettings . renderHtml

convertHtmlWith :: FilePath -> Html -> IO B.ByteString
convertHtmlWith p = convertBytestring (WKSettings p "utf-8") . renderHtml

