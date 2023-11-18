#!/usr/bin/env fish

cabal build --enable-executable-dynamic\
            --enable-shared\
            --disable-optimization
and begin
  mv $(cabal list-bin --enable-executable-dynamic\
                      --enable-shared\
                      --disable-optimization\
                      personal-website-exe) ./
  ./personal-website-exe -p 8080 -s static/ -f public/
end
