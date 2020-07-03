"use strict";

const TelegramBot = require('node-telegram-bot-api');

exports.getApkKey = function () {
  return process.env.GIPHY_API_KEY
}

exports.startRepl = function (f) {
  return function () {
    const bot = new TelegramBot(process.env.TELEGRAM_TOKEN, { polling: true });
    bot.on('message', (msg) => {
      const chatId = msg.chat.id;
      let eff = f(msg)();
      eff.then(function (result) {
        bot.sendMessage(chatId, result);
      })
    });
  };
};
