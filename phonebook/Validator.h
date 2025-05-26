#ifndef VALIDATOR_H
#define VALIDATOR_H

#include <QString>
#include <QRegularExpression>
#include <QDate>

class Validator {

public:

    // Статистические методы проверки данных на корректность
    static bool ValidName(const QString& name);
    static bool ValidPhoneNumber(const QString& phone);
    static QStringList ValidPhoneNumbers(const QStringList& phoneNumbers);
    static bool ValidEmail(const QString& email);
    static bool ValidDate(const QString& date);
    static bool ValidAddress(const QString& address);

    // Удаление лишних пробелов
    static QString trim(const QString& str);
    static QStringList trimPhone(const QStringList& phoneNumbers);
    static QString EmailTrimmer(const QString& email);
};

#endif // VALIDATOR_H
