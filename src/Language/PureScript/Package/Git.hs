{-# LANGUAGE OverloadedStrings #-}

-- | Functions for executing git.

module Language.PureScript.Package.Git (
  cloneShallow
, listRemoteTags
) where

import           Turtle
import           Prelude hiding (FilePath)

import Language.PureScript.Package.Path (pathToTextUnsafe)

cloneShallow
  :: MonadIO m
  => Text
  -- ^ repo
  -> Text
  -- ^ branch/tag
  -> Turtle.FilePath
  -- ^ target directory
  -> m ExitCode
cloneShallow from ref into =
  proc "git"
       [ "clone"
       , "-q"
       , "-c", "advice.detachedHead=false"
       , "--depth", "1"
       , "-b", ref
       , from
       , pathToTextUnsafe into
       ] empty .||. exit (ExitFailure 1)

listRemoteTags
  :: Text
  -- ^ repo
  -> Turtle.Shell Text
listRemoteTags from = let gitProc = inproc "git"
                                    [ "ls-remote"
                                    , "-q"
                                    , "-t"
                                    , from
                                    ] empty
                      in lineToText <$> gitProc
