#!/bin/sh
stack exec cardano-byron-proxy -- --configuration-file ../cardano-sl/lib/configuration.yaml --configuration-key mainnet_full --local-addr "(\"127.0.0.1\", \"7777\")" --topology ./topology.yaml
