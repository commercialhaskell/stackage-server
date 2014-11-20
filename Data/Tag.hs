-- | A wrapper around the 'Slug' interface.

module Data.Tag where

import Control.Monad.Catch
import Data.Slug
import Data.Text

-- | Make a tag.
mkTag :: MonadThrow m => Text -> m Slug
mkTag = mkSlugLen 1 20
