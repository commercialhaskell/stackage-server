# `stackage-server` load tests

This directory can be used to test an on-premises instance of `stackage-server` for quality of service metrics. Follow the instructions below to run it.

We assume you will be using `pyenv` to handle different Python environments in a safe manner. If you prefer installing packages globally (not recommended), jump straight to the line commented with `# install dependencies`. Before doing anything, set an environment variable `STACKAGE_SERVER_REPO` on your shell to this repo’s root folder. Then copy everything and run:

```shell
cd ~
curl -L https://raw.githubusercontent.com/pyenv/pyenv-installer/master/bin/pyenv-installer | bash

# add the following lines to your shell initialisation for permanent usage
export PYENV_VIRTUALENV_DISABLE_PROMPT=1
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"

# create an environment for tests
pyenv install 3.6.3
pyenv virtualenv 3.6.3 stackage-server-py-3.6.3
cd "${STACKAGE_SERVER_REPO}/etc/load-testing"
pyenv local stackage-server-py-3.6.3
pyenv activate stackage-server-py-3.6.3 # should happen automatically on previous line, but just to be sure

# install dependencies
pip install -r requirements.txt

# execute load tests
locust --host="http://yourlocalinstance.domain"
```

Then navigate to [127.0.0.1:8089](http://127.0.0.1:8089), set the number of clients to simulate, spawn rate, start it and wait a few minutes for the results to stabilise. You can then download or analyse directly on Locust’s UI.
