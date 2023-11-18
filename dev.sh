#!/usr/bin/env bash

BIN_LOCATION=$(cabal list-bin --enable-executable-dynamic\
                              --enable-shared\
                              --disable-optimization\
                              personal-website-exe);
BUILD_RUN="
  cabal build --enable-executable-dynamic\
              --enable-shared\
              --disable-optimization && {
    mv $BIN_LOCATION ./
    ./personal-website-exe -p 8080 -s static/ -f public/
  }";

watchexec -r -e hs -e cabal "$BUILD_RUN"
