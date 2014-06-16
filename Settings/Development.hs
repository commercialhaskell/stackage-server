module Settings.Development where

import Prelude

development :: Bool
development =
#if DEVELOPMENT
  True
#else
  False
#endif

cabalFileLoader :: Bool
cabalFileLoader =
#if INGHCI
  False
#else
  True
#endif

production :: Bool
production = not development
