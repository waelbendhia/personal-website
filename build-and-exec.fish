#!/usr/bin/env fish

cabal build
and begin
  set -g GIT_HASH $(git rev-parse HEAD)
  mv $(cabal list-bin personal-website-exe) ./
  ./personal-website-exe -p 8080 -s test-blogs/
end
