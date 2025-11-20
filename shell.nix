(import (
  fetchGit {
    url = "https://github.com/edolstra/flake-compat";
  }
) {
  src =  ./.;
}).shellNix
