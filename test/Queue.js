"use strict"

const fs = require('fs')

exports.newQueue = () => { return { log: [] } }
exports.push = x => q => () => { q.log.push(x) }
exports.toArray = q => () => [...q.log]
exports.unsafeReadTextFile = (filename) => () => fs.readFileSync(filename, "utf8")
