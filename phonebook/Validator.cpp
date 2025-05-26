#include "Validator.h"
#include <chrono>
#include <ctime>

// Метод для проверки имени на корректность
bool Validator::ValidName(const QString& name) {
    static QRegularExpression nameRegex(R"(^[A-ZА-ЯЁ]+[A-Za-zА-Яа-яЁё0-9\- ]*[A-Za-zА-Яа-яЁё0-9]$)");
    return nameRegex.match(name).hasMatch();
}

// Метод для проверки телефона на корректность
bool Validator::ValidPhoneNumber(const QString& phone) {
    static QRegularExpression phoneRegex(R"((\+7|8)\(\d{3}\)\d{3}-\d{2}-\d{2}|(\+7|8)\(?\d{3}\)?\d{7})");
    return phoneRegex.match(phone).hasMatch();
}

// Метод для проверки всего списка телефонов на корректность
QStringList Validator::ValidPhoneNumbers(const QStringList& phoneNumbers) {
    QStringList validNumbers;

    for (const QString& phone : phoneNumbers) {
        QString trimmedPhone = phone.trimmed(); // Убираем пробелы
        if (ValidPhoneNumber(trimmedPhone)) {
            validNumbers.append(trimmedPhone); // Если номер валиден, добавляем его
        }
    }
    return validNumbers; // Возвращаем список валидных номеров
}

// Метод для проверки электронной почты на корректность
bool Validator::ValidEmail(const QString& email) {
    static QRegularExpression emailRegex(R"(^[A-Za-z0-9][A-Za-z0-9.]*@[A-Za-z0-9.]+\.[A-Za-z]{2,}$)");
    return emailRegex.match(email).hasMatch();
}

// Метод для проверки даты рождения на корректность
bool Validator::ValidDate(const QString& date) {
    // Регулярное выражение для формата ДД.ММ.ГГГГ
    static QRegularExpression dateRegex(R"(^\d{2}\.\d{2}\.\d{4}$)");
    if (!dateRegex.match(date).hasMatch()) {
        return false;
    }

    // Разбираем строку на день, месяц и год
    QStringList dateParts = date.split('.');
    int day = dateParts[0].toInt();
    int month = dateParts[1].toInt();
    int year = dateParts[2].toInt();

    // Проверка на допустимые значения дня и месяца
    if (month < 1 || month > 12 || day < 1 || day > 31) {
        return false;
    }

    // Проверка февраля в зависимости от високосного года
    bool isLeap = (year % 4 == 0 && year % 100 != 0) || (year % 400 == 0);
    if ((month == 2 && day > (isLeap ? 29 : 28)) ||
        ((month == 4 || month == 6 || month == 9 || month == 11) && day > 30)) {
        return false;
    }

    // Проверка для месяцев с 30 днями
    if ((month == 4 || month == 6 || month == 9 || month == 11) && day > 30) {
        return false;
    }

    // Получаем текущую дату
    auto now = std::chrono::system_clock::now();
    std::time_t now_time = std::chrono::system_clock::to_time_t(now);
    std::tm* currentDate = std::localtime(&now_time);

    // Сравниваем введённую дату с текущей
    if (year > currentDate->tm_year + 1900 ||
        (year == currentDate->tm_year + 1900 && month > currentDate->tm_mon + 1) ||
        (year == currentDate->tm_year + 1900 && month == currentDate->tm_mon + 1 && day > currentDate->tm_mday)) {
        return false;
    }

    return true;
}

// Проверка адреса на корректность
bool Validator::ValidAddress(const QString& address) {
    static QRegularExpression addressRegex(R"(^[A-ZА-ЯЁ0-9]+[A-Za-zА-Яа-яЁё0-9\- ]*\s\d+([\dA-Za-zА-Яа-яЁё]+\d+)?(-\d+)?$)");
    return addressRegex.match(address).hasMatch();
}

// Устранение пробелов
QString Validator::trim(const QString& str) {
    return str.trimmed();
}

// Устранение пробелов в списке телефонов
QStringList Validator::trimPhone(const QStringList& phoneNumbers) {
    QStringList trimmedPhoneNumbers = phoneNumbers;

    // Удаляем пробелы с начала и конца каждого номера
    for (int i = 0; i < trimmedPhoneNumbers.size(); ++i) {
        trimmedPhoneNumbers[i] = trimmedPhoneNumbers[i].trimmed();
    }

    return trimmedPhoneNumbers;
}

// Устранение проблеов в почте
QString Validator::EmailTrimmer(const QString& email) {
    QString trimmedEmail = email.trimmed();  // Обрезаем пробелы вокруг всей строки
    int atPos = trimmedEmail.indexOf('@');   // Находим позицию символа '@'

    if (atPos != -1) {
        QString localPart = trimmedEmail.left(atPos).trimmed();       // Локальная часть до '@'
        QString domainPart = trimmedEmail.mid(atPos + 1).trimmed();   // Доменная часть после '@'
        return localPart + "@" + domainPart;  // Собираем нормализованный email
    }

    return trimmedEmail;  // Если '@' не найден, возвращаем строку как есть
}
