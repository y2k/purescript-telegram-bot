"use strict";

const TelegramBot = require('node-telegram-bot-api');

exports.makeVar = x => () => ({ value: x })
exports.setVar = ref => x => () => { ref.value = x }
exports.getVar = x => () => x.value

exports.unsafeToJson = x => () => JSON.stringify(x)
exports.unsafeParseJson = json => () => JSON.parse(json)
/**
 *  @param {TelegramBot} bot
 */
exports.editMessageReplyMarkup = bot => chatId => msgId => buttons => () => {
  bot.editMessageReplyMarkup({
    inline_keyboard: [buttons]
  }, {
    chat_id: chatId,
    message_id: msgId,
  })
}
/**
 *  @param {TelegramBot} bot
 */
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
/**
 *  @param {TelegramBot} bot
 */
exports.deleteMessage = bot => x => () => bot.deleteMessage(x.chatId, x.messageId)
/**
 *  @param {TelegramBot} bot
 */
exports.sendVideo = bot => chatId => replyMsg => video => caption => buttons => () =>
  bot.sendVideo(chatId, video, {
    caption: caption,
    reply_to_message_id: replyMsg,
    parse_mode: 'Markdown',
    reply_markup: { inline_keyboard: [buttons] }
  });
/**
 * @param {TelegramBot} bot
 */
exports.sendMessage = bot => x => () => bot.sendMessage(
  x.chatId,
  x.text,
  {
    disable_notification: true,
    reply_to_message_id: x.reply_message_id
  });
exports.createBot = () => new TelegramBot(process.env.TELEGRAM_TOKEN, { polling: true });
/**
 *  @param {TelegramBot} bot
 */
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
