stackage-server
===============

Server for stable, curated Haskell package sets

Code builds with the Stackage snapshot:

    remote-repo: stackage-35ecbe20461b5fe50bad1e5653f6660132861fe9:http://www.stackage.org/stackage/35ecbe20461b5fe50bad1e5653f6660132861fe9

Inside the config directory, there are two files ending in `-sample`. They
should be copied to remove the `-sample` suffix for the site to work. We do it
this way to avoid accidentally committing real database credentials into the
Git repository.
