"use strict";

const TelegramBot = require('node-telegram-bot-api');

exports.getApiKey = () => process.env.GIPHY_API_KEY
exports.sendVideo = bot => chatId => video => () => bot.sendVideo(chatId, video);
exports.sendMessage = bot => chatId => text => () => bot.sendMessage(chatId, text);
exports.startBotRepl = f => () => {
  const bot = new TelegramBot(process.env.TELEGRAM_TOKEN, { polling: true });
  bot.on('message', msg => {
    console.log(msg)
    f({ bot: bot, chat: msg.chat.id, text: msg.text || "" })()
  });
};
