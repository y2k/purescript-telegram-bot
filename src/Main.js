"use strict";

const TelegramBot = require('node-telegram-bot-api');

exports.makeVar = x => () => ({ value: x })
exports.setVar = ref => x => () => { ref.value = x }
exports.getVar = x => () => x.value

exports.unsafeToJson = x => () => JSON.stringify(x)
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
/**
 * @param {TelegramBot} bot
 */
exports.sendMessage2 = bot => x => () => bot.sendMessage(
  x.chatId,
  x.text,
  {
    disable_notification: true,
    reply_to_message_id: x.reply_message_id
  });
exports.createBot = () => new TelegramBot(process.env.TELEGRAM_TOKEN, { polling: true });
exports.startBotRepl = bot => f => () => {
  function update(msg) {
    try {
      console.log(msg)
      f(msg)()
    } catch (e) {
      console.log(e)
    }
  }
  bot.on('callback_query', update)
  bot.on('message', update);
};
