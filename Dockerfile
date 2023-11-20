FROM haskell:9.2.8 as build
RUN mkdir /opt/build
WORKDIR /opt/build

RUN apt-get update &&\
    apt-get download libgmp10 postgresql
RUN mv libgmp*.deb libgmp.deb

RUN cabal update

COPY cabal.project personal-website.cabal ./
RUN cabal v2-build -v1 --only-dependencies all
COPY . .
RUN cabal build personal-website-exe
RUN cp $(cabal list-bin personal-website-exe) .

FROM debian:buster as app
RUN mkdir -p /opt/app
WORKDIR /opt/app

RUN apt-get update &&\
    apt-get install -y libpq-dev

COPY --from=build /opt/build/libgmp.deb /tmp
RUN dpkg -i /tmp/libgmp.deb && rm /tmp/libgmp.deb

COPY --from=build /opt/build/personal-website-exe ./personal-website-exe

RUN ls /opt/app
RUN chmod +x /opt/app/personal-website-exe

ENV PORT=8080
ENV STATIC_FOLDER=""
ENV PUBLIC_FOLDER=""

EXPOSE $PORT
CMD exec /opt/app/personal-website-exe --port $PORT --static-folder $STATIC_FOLDER --public-folder $PUBLIC_FOLDER
