"use strict";

const TelegramBot = require('node-telegram-bot-api');

exports.unsafeParseJson = json => () => JSON.parse(json)
exports.editMessageReplyMarkup = bot => chatId => msgId => buttons => () => {
  bot.editMessageReplyMarkup({
    inline_keyboard: [buttons]
  }, {
    chat_id: chatId,
    message_id: msgId,
  })
}
exports.editMessageMedia = bot => chatId => msgId => url => buttons => () => {
  console.log("editMessageMedia(chatId = " + chatId + ", msgId = " + msgId + ", url = " + url + ")")
  bot.editMessageMedia({
    media: url,
    type: "video"
  }, {
    chat_id: chatId,
    message_id: msgId,
    reply_markup: { inline_keyboard: [buttons] }
  })
}
exports.getApiKey = () => process.env.GIPHY_API_KEY
exports.deleteMessage = bot => x => () => bot.deleteMessage(x.chatId, x.messageId)
exports.sendVideo = bot => chatId => replyMsg => video => caption => buttons => () =>
  bot.sendVideo(chatId, video, {
    caption: caption,
    reply_to_message_id: replyMsg,
    reply_markup: { inline_keyboard: [buttons] }
  });
exports.sendMessage = x => bot => () => bot.sendMessage(x.chatId, x.text);
exports.startBotRepl = f => () => {
  const bot = new TelegramBot(process.env.TELEGRAM_TOKEN, { polling: true });
  function update(msg) {
    console.log(msg)
    try {
      f(Object.assign({ bot: bot }, msg))()
    } catch (e) {
      console.log(e)
    }
  }
  bot.on('callback_query', update)
  bot.on('message', update);
};
