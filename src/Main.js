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
exports.editMessageReplyMarkup = bot => params => () => {
  bot.editMessageReplyMarkup({
    inline_keyboard: [params.keyboard]
  }, {
    chat_id: params.chat_id,
    message_id: params.message_id,
  })
}
/**
 *  @param {TelegramBot} bot
 */
exports.editMessageMedia = bot => params => () => {
  bot.editMessageMedia({
    media: params.url,
    type: "video"
  }, {
    chat_id: params.chat_id,
    message_id: params.message_id,
    reply_markup: { inline_keyboard: [params.keyboard] }
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
exports.sendVideo = bot => params => () =>
  bot.sendVideo(params.chat_id, params.url, {
    caption: params.caption,
    reply_to_message_id: params.reply_to_message_id,
    parse_mode: 'Markdown',
    reply_markup: { inline_keyboard: [params.keyboard] },
    disable_notification: false
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
      f(msg)()
    } catch (e) {
      console.log(e)
    }
  }
  bot.on('callback_query', update)
  bot.on('message', update);
};
