"use strict";

const TelegramBot = require('node-telegram-bot-api');

exports.getApiKey = () => process.env.GIPHY_API_KEY
exports.sendVideo = bot => chatId => replyMsg => video => caption => () =>
  bot.sendVideo(chatId, video, { caption: caption, reply_to_message_id: replyMsg });
exports.sendMessage = bot => chatId => text => () => bot.sendMessage(chatId, text);
exports.startBotRepl = f => () => {
  const bot = new TelegramBot(process.env.TELEGRAM_TOKEN, { polling: true });
  bot.on('message', msg => {
    console.log(msg)
    try {
      f({
        bot: bot,
        chat: msg.chat.id,
        text: msg.text || "",
        id: msg.message_id,
        regUserName: msg.new_chat_member && msg.new_chat_member.first_name,
      })()
    } catch (e) {
      console.log(e)
    }
  });
};
