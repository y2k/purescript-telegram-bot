FROM node:17.0.1-buster

RUN npm install -g --unsafe-perm purescript@0.14.5
RUN npm install -g --unsafe-perm spago@0.20.3

WORKDIR /app

COPY package.json yarn.lock /app/
RUN yarn

COPY spago.dhall packages.dhall ./
COPY src/ src/
COPY test/*.purs test/*.js test/
COPY test/resources/*.xml test/resources/

RUN spago test && spago bundle-app -x

FROM node:17.0.1-alpine3.14

WORKDIR /app

COPY --from=0 /app/package.json /app/yarn.lock ./
RUN yarn --production

COPY --from=0 /app/index.js .
COPY --from=0 /app/index.js.map .

ENV export NTBA_FIX_319=1

ENTRYPOINT ["node", "--enable-source-maps", "index.js"]
