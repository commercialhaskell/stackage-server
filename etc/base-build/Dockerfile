FROM fpco/stack-build:lts-15.5

RUN stack update
COPY stack.yaml stackage-server.cabal /src/
RUN stack build --only-snapshot --stack-yaml /src/stack.yaml
