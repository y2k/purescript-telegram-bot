FROM node:14.5.0-stretch

WORKDIR /app
COPY . /app

RUN npm install -g --unsafe-perm purescript@0.13.8
RUN npm install -g --unsafe-perm spago@0.15.3
RUN spago test && spago bundle-app

FROM node:14.5.0-alpine3.11

WORKDIR /app
COPY --from=0 /app/index.js .
COPY --from=0 /app/package.json .
RUN yarn --production

ENTRYPOINT ["node", "index.js"]
