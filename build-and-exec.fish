#!/usr/bin/env fish

cabal build --disable-optimization
and begin
  mv $(cabal list-bin --disable-optimization personal-website-exe) ./
  ./personal-website-exe -p 8080 -s static/ -f public/
end
