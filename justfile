[parallel]
test: stackage-server-nix
    stack test
    nix flake check

stackage-server-nix:
    cd nix && ./gen-packages.sh stackage-server

cachix-push:
    # inputs
    nix flake archive --json \
        | jq -r '.path,(.inputs|to_entries[].value.path)' \
        | cachix push stackage-infrastructure

    # runtime closure
    nix build --no-link --print-out-paths \
        | cachix push stackage-infrastructure

    # shell env
    nix develop --profile dev-profile -c true
    cachix push stackage-infrastructure dev-profile

run-ci:
    act --artifact-server-path=$PWD/act-artifacts pull_request --job stack-test
