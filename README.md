stackage-server
===============

[![Build Status](https://travis-ci.org/fpco/stackage-server.svg?branch=master)](https://travis-ci.org/fpco/stackage-server)

Server for stable, curated Haskell package sets

This repo is part of the [Stackage project](https://github.com/fpco/stackage),
and the live server can be viewed at https://www.stackage.org.

Inside the config directory, there are two files ending in `-sample`. They
should be copied to remove the `-sample` suffix for the site to work. We do it
this way to avoid accidentally committing real database credentials into the
Git repository.

## How to upload a snapshot bundle to your own instance of stackage-server

You can upload snapshots to your own instance of stackage-server using
[stackage-curator](https://github.com/fpco/stackage-curator).
Here's how:

Start your instance (e.g. run `stackage-server Development`)

In a browser:

* Log into your instance
* Navigate to `http://your-stackage-server/profile`
* Take note of both your username and auth token

Edit your config/settings.yaml. Make sure your username is listed as an admin user, e.g.

```
admin-users:  
- danburton
```

Restart your stackage-server instance if you changed its config/settings.yaml

In a terminal:

* Set the `STACKAGE_AUTH_TOKEN` environment variable to your auth token
* run `stackage-curator upload your-snapshot.bundle --server-url http://your-stackage-server`

Sample bundle for trying this out: [stackage-nightly-2015-03-26.bundle](https://s3.amazonaws.com/download.fpcomplete.com/michael/stackage-nightly-2015-03-26.bundle)
