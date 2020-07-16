"use strict";

const TelegramBot = require('node-telegram-bot-api');

exports.editMessageMedia = bot => chatId => msgId => url => buttons => () => {
  console.log("editMessageMedia(chatId = " + chatId + ", msgId = " + msgId + ", url = " + url + ")")
  bot.editMessageMedia(
    {
      media: url,
      type: "video"
    },
    {
      chat_id: chatId,
      message_id: msgId,
      reply_markup: { inline_keyboard: [buttons] }
    })
}
exports.getApiKey = () => process.env.GIPHY_API_KEY
exports.deleteMessage = bot => chatId => msgId => () => bot.deleteMessage(chatId, msgId)
exports.sendVideo = bot => chatId => replyMsg => video => caption => buttons => () =>
  bot.sendVideo(chatId, video, {
    caption: caption,
    reply_to_message_id: replyMsg,
    reply_markup: { inline_keyboard: [buttons] }
  });
exports.sendMessage = bot => chatId => text => () => bot.sendMessage(chatId, text);
exports.startBotRepl = f => () => {
  const bot = new TelegramBot(process.env.TELEGRAM_TOKEN, { polling: true });
  bot.on('callback_query', msg => {
    console.log(msg)
    try {
      f({
        bot: bot,
        from: msg.from,
        message: msg.message,
        chat: msg.chat,
        data: msg.data,
        text: "",
        id: 0,
        regUserName: "",
      })()
    } catch (e) {
      console.log(e)
    }
  })
  bot.on('message', msg => {
    console.log(msg)
    try {
      f({
        bot: bot,
        from: msg.from,
        message: null,
        chat: msg.chat,
        text: msg.text || "",
        id: msg.message_id,
        regUserName: msg.new_chat_member && msg.new_chat_member.username,
      })()
    } catch (e) {
      console.log(e)
    }
  });
};
