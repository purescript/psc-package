module Language.PureScript.Package.Path (pathToTextUnsafe) where

import Turtle (FilePath, Text, toText)
import Prelude (either, error, id, (.))

pathToTextUnsafe :: FilePath -> Text
pathToTextUnsafe = either (error "FilePath.toText failed") id . toText
