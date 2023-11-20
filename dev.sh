#!/usr/bin/env bash

ghcid "--command=cabal repl Application" --test="runApp $ ApplicationConfig 8080 \"static/\" \"public/\""
