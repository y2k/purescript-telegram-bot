"use strict"

exports.newQueue = () => { return { log: [] } }
exports.push = x => q => () => { q.log.push(x) }
exports.toArray = q => () => [...q.log]
