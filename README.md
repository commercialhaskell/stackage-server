stackage-server
===============

Server for stable, curated Haskell package sets

Code builds with the Stackage snapshot:

    remote-repo: stackage-e36ddac9333f2197ada6883b52f4834ddc0d5e37:http://www.stackage.org/stackage/e36ddac9333f2197ada6883b52f4834ddc0d5e37

Inside the config directory, there are two files ending in `-sample`. They
should be copied to remove the `-sample` suffix for the site to work. We do it
this way to avoid accidentally committing real database credentials into the
Git repository.
