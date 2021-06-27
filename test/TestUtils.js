"use strict"

const fs = require('fs')

exports.newQueue = () => { return { log: [] } }
exports.reset = q => () => { q.log = [] }
exports.push = x => q => () => { q.log.push(x) }
exports.toArray = q => () => [...q.log]

exports.unsafeReadTextFile = (path) => () => {
  if (!fs.existsSync(path)) console.error("Can't find file: " + path)
  return fs.readFileSync(path, "utf8")
}
exports.stringHashCode = function (s) {
  return s.split("").reduce(function (a, b) { a = ((a << 5) - a) + b.charCodeAt(0); return a & a }, 0);
}

exports.unsafeToJson = x => () => JSON.stringify(x)
exports.unsafeParseJson = json => () => JSON.parse(json)
