"use strict";

const TelegramBot = require('node-telegram-bot-api');

exports.test = function (handleRequest) {
  return function () {
    return 42;
  }
};

const bot = new TelegramBot(process.env.TELEGRAM_TOKEN, { polling: true });
bot.on('message', (msg) => {
  console.log(msg);

  const chatId = msg.chat.id;
  bot.sendMessage(chatId, 'Received your message');
});
