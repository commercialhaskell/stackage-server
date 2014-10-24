stackage-server
===============

Server for stable, curated Haskell package sets

Code builds with the Stackage snapshot:

    remote-repo: stackage:http://www.stackage.org/stackage/aecbf72b568a63e86a971311fee5475f076043cc

Inside the config directory, there are two files ending in `-sample`. They
should be copied to remove the `-sample` suffix for the site to work. We do it
this way to avoid accidentally committing real database credentials into the
Git repository.
