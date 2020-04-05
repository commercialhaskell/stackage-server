stackage-server
===============

![Runtime image](https://github.com/fpco/stackage-server/workflows/Runtime%20image/badge.svg)

Server for stable, curated Haskell package sets

This repo is part of the [Stackage project](https://github.com/fpco/stackage),
and the live server can be viewed at https://www.stackage.org.

## Building locally

Build locally by passing the `dev` flag to it:

``` shellsession
$ stack build . --flag stackage-server:dev
```

Now, initially you need to run the cron job to create and populate the database:

``` shellsession
$ export PGSTRING=postgresql://postgres:password@localhost:5432/stackage
$ stack exec stackage-server-cron
```

Note that you need to modify the PGSTRING environment variable according to your actual database configuration. Also, you need to create an empty database before running the cron job. Note that it takes quites some time for it to load your database.

After this, run the stackage server:

``` shellsession
$ export PGSTRING=postgresql://postgres:password@localhost:5432/stackage
$ stack exec stackage-server
```
