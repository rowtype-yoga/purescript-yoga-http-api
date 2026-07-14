-- EXPECT: Could not match type
module Test.CompileFail.UnsupportedStreamChunk where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Yoga.HTTP.API.Route.Encoding (Streaming, streaming)

bad :: Effect (Streaming Int)
bad = streaming
  { pull: pure (Just 1)
  , cancel: pure unit
  }
