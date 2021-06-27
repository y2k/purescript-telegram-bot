FROM node:14.5.0-stretch

RUN npm install -g --unsafe-perm purescript@0.13.8
RUN npm install -g --unsafe-perm spago@0.15.3

WORKDIR /app

COPY package.json yarn.lock /app/
RUN yarn

COPY spago.dhall packages.dhall ./
COPY src/*.purs src/*.js src/
COPY test/*.purs test/*.js test/
COPY test/resources/*.xml test/resources/

RUN spago test && spago bundle-app

FROM node:14.5.0-alpine3.11

WORKDIR /app

COPY --from=0 /app/package.json /app/yarn.lock ./
RUN yarn --production

COPY --from=0 /app/index.js .

ENV export NTBA_FIX_319=1

ENTRYPOINT ["node", "index.js"]
