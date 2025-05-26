from telegram import Update, ReplyKeyboardMarkup, ReplyKeyboardRemove
from telegram.ext import (
    ApplicationBuilder, CommandHandler, MessageHandler, filters,
    ConversationHandler, ContextTypes
)
from telegram.constants import ParseMode

# состояния для заказа поездки
FROM_CITY, TO_CITY, RIDE_DATE, RIDE_NAME, RIDE_PHONE = range(5)

# состояния для заказа звонка
CALL_NAME, CALL_PHONE = range(10, 12)

# команда start
async def start(update: Update, context: ContextTypes.DEFAULT_TYPE):
    keyboard = [
        ["🚐 Заказать поездку"],
        ["☎️ Заказать звонок"],
        ["🌐 Открыть веб-сайт"]
    ]
    reply_markup = ReplyKeyboardMarkup(keyboard, resize_keyboard=True)

    await update.message.reply_text(
        "👋 Добро пожаловать в \n\nВыберите нужное действие:",
        reply_markup=reply_markup
    )


# заказ поездки
async def ride_start(update: Update, context: ContextTypes.DEFAULT_TYPE):
    reply_markup = ReplyKeyboardMarkup([["❌ Отмена"]], resize_keyboard=True)
    await update.message.reply_text("📍 Откуда поедем?", reply_markup=reply_markup)
    return FROM_CITY

async def from_city(update: Update, context: ContextTypes.DEFAULT_TYPE):
    context.user_data["from_city"] = update.message.text
    reply_markup = ReplyKeyboardMarkup([["❌ Отмена"]], resize_keyboard=True)
    await update.message.reply_text("🏁 Куда направляемся?", reply_markup=reply_markup)
    return TO_CITY

async def to_city(update: Update, context: ContextTypes.DEFAULT_TYPE):
    context.user_data["to_city"] = update.message.text
    reply_markup = ReplyKeyboardMarkup([["❌ Отмена"]], resize_keyboard=True)
    await update.message.reply_text("📅 Когда планируете поездку? (например, 21 апреля в 10:00)", reply_markup=reply_markup)
    return RIDE_DATE

async def ride_date(update: Update, context: ContextTypes.DEFAULT_TYPE):
    context.user_data["ride_date"] = update.message.text
    reply_markup = ReplyKeyboardMarkup([["❌ Отмена"]], resize_keyboard=True)
    await update.message.reply_text("👤 Укажите, пожалуйста, ваше имя:", reply_markup=reply_markup)
    return RIDE_NAME

async def ride_name(update: Update, context: ContextTypes.DEFAULT_TYPE):
    context.user_data["ride_name"] = update.message.text
    reply_markup = ReplyKeyboardMarkup([["❌ Отмена"]], resize_keyboard=True)
    await update.message.reply_text("📞 Введите ваш номер телефона:", reply_markup=reply_markup)
    return RIDE_PHONE

TARGET_CHAT_ID = "-"

async def ride_phone(update: Update, context: ContextTypes.DEFAULT_TYPE):
    context.user_data["ride_phone"] = update.message.text
    data = context.user_data

    # отправка данных заказчика
    await context.bot.send_message(
        chat_id=TARGET_CHAT_ID,
        text=(
            f"🚐 Новый заказ поездки:\n\n"
            f"🗺 {data['from_city']} → {data['to_city']}\n"
            f"🗓 {data['ride_date']}\n"
            f"👤 {data['ride_name']}\n"
            f"📞 {data['ride_phone']}"
        )
    )

    # подтверждение для клиента
    summary = (
        f"✅ Спасибо за заказ!\n\n"
        f"🗺 {data['from_city']} → {data['to_city']}\n"
        f"🗓 {data['ride_date']}\n"
        f"👤 {data['ride_name']}\n"
        f"📞 {data['ride_phone']}\n\n"
        f"Наш менеджер свяжется с вами. Спасибо!"
    )
    await update.message.reply_text(summary, reply_markup=ReplyKeyboardRemove())
    return ConversationHandler.END



# Заказ звонка
async def call_start(update: Update, context: ContextTypes.DEFAULT_TYPE):
    reply_markup = ReplyKeyboardMarkup([["❌ Отмена"]], resize_keyboard=True)
    await update.message.reply_text("☎ Заказ звонка.\n\nКак вас зовут?", reply_markup=reply_markup)
    return CALL_NAME

async def call_name(update: Update, context: ContextTypes.DEFAULT_TYPE):
    context.user_data["call_name"] = update.message.text
    reply_markup = ReplyKeyboardMarkup([["❌ Отмена"]], resize_keyboard=True)
    await update.message.reply_text("📞 Введите ваш номер телефона для обратного звонка:", reply_markup=reply_markup)
    return CALL_PHONE


async def call_phone(update: Update, context: ContextTypes.DEFAULT_TYPE):
    context.user_data["call_phone"] = update.message.text
    data = context.user_data

    await context.bot.send_message(
        chat_id=TARGET_CHAT_ID,
        text=(
            f"☎️ Новый запрос звонка:\n\n"
            f"👤 Имя: {data['call_name']}\n"
            f"📞 Телефон: {data['call_phone']}"
        )
    )

    summary = (
        f"✅ Звонок заказан!\n\n"
        f"👤 Имя: {data['call_name']}\n"
        f"📞 Телефон: {data['call_phone']}\n\n"
        f"Мы скоро вам перезвоним."
    )
    await update.message.reply_text(summary, reply_markup=ReplyKeyboardRemove())
    return ConversationHandler.END

# Отмена
async def cancel(update: Update, context: ContextTypes.DEFAULT_TYPE):
    await update.message.reply_text("❌ Операция отменена.", reply_markup=ReplyKeyboardRemove())
    return ConversationHandler.END

async def open_website(update: Update, context: ContextTypes.DEFAULT_TYPE):
    await update.message.reply_text(
        "🌐 Перейдите на наш сайт: [-](-)",
        parse_mode=ParseMode.MARKDOWN
    )
cancel_handler = MessageHandler(filters.TEXT & filters.Regex("^❌ Отмена$"), cancel)

async def debug_chat_id(update: Update, context: ContextTypes.DEFAULT_TYPE):
    await update.message.reply_text(f"Chat ID: `{update.effective_chat.id}`", parse_mode="Markdown")

# запуск
if __name__ == "__main__":
    app = ApplicationBuilder().token("-").build()

    # диалог заказа поездки
    ride_conv_handler = ConversationHandler(
        entry_points=[
            CommandHandler("ride", ride_start),
            MessageHandler(filters.TEXT & filters.Regex("^🚐 Заказать поездку$"), ride_start)
        ],
        states={
            FROM_CITY: [cancel_handler,MessageHandler(filters.TEXT & ~filters.COMMAND, from_city)],
            TO_CITY: [cancel_handler,MessageHandler(filters.TEXT & ~filters.COMMAND, to_city)],
            RIDE_DATE: [cancel_handler,MessageHandler(filters.TEXT & ~filters.COMMAND, ride_date)],
            RIDE_NAME: [cancel_handler,MessageHandler(filters.TEXT & ~filters.COMMAND, ride_name)],
            RIDE_PHONE: [cancel_handler,MessageHandler(filters.TEXT & ~filters.COMMAND, ride_phone)],
        },
        fallbacks=[
            CommandHandler("cancel", cancel),
            MessageHandler(filters.TEXT & filters.Regex("^❌ Отмена$"), cancel)
        ]

    )

    # диалог заказа звонка
    call_conv_handler = ConversationHandler(
        entry_points=[
            CommandHandler("call", call_start),
            MessageHandler(filters.TEXT & filters.Regex("^☎️ Заказать звонок$"), call_start)
        ],
        states={
            CALL_NAME: [cancel_handler,MessageHandler(filters.TEXT & ~filters.COMMAND, call_name)],
            CALL_PHONE: [cancel_handler,MessageHandler(filters.TEXT & ~filters.COMMAND, call_phone)],
        },
        fallbacks=[
            CommandHandler("cancel", cancel),
            MessageHandler(filters.TEXT & filters.Regex("^❌ Отмена$"), cancel)
        ],
    )

    # регистрация обработчиков
    app.add_handler(CommandHandler("start", start))
    app.add_handler(ride_conv_handler)
    app.add_handler(call_conv_handler)
    app.add_handler(MessageHandler(filters.TEXT & filters.Regex("^🌐 Открыть веб-сайт$"), open_website))
    app.add_handler(CommandHandler("getid", debug_chat_id))
    print("Бот запущен...")
    app.run_polling()

