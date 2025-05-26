#include "Contacts.h"
#include "Database.h"
#include <QMessageBox>
#include <QSqlError>

// Метод инициализации бд
bool Database::initializeDatabase() {
    // Создание и инициализация бд
    QSqlDatabase db = QSqlDatabase::addDatabase("QSQLITE");
    db.setDatabaseName("contacts.db");

    // Открытие соединения
    if (!db.open()) {
        QMessageBox::critical(nullptr, "Ошибка", "Не удалось открыть базу данных!");
        return false; // Return false if the database can't be opened
    }

    // Создание таблицы
    QSqlQuery query; // Переменная для запроса

    // Выполнение запроса
    query.exec(R"(
        CREATE TABLE IF NOT EXISTS contacts ( // Создание 1 раз, если не существует
            id INTEGER PRIMARY KEY AUTOINCREMENT, // Уникальный id записей
            lastName TEXT,
            firstName TEXT,
            middleName TEXT,
            address TEXT,
            birthDate TEXT,
            email TEXT UNIQUE,
            phoneNumbers TEXT
        )
    )");

    return true;
}

// Метод сохранения контакта в бд
bool Database::saveContactToDatabase(const Contact& contact) {
    QSqlQuery query;

    // Запрос на сохранение контакта
    query.prepare(R"(
        INSERT INTO contacts (lastName, firstName, middleName, address, birthDate, email, phoneNumbers)
        VALUES (:lastName, :firstName, :middleName, :address, :birthDate, :email, :phoneNumbers)
        ON CONFLICT(email) DO UPDATE SET
            lastName = excluded.lastName,
            firstName = excluded.firstName,
            middleName = excluded.middleName,
            address = excluded.address,
            birthDate = excluded.birthDate,
            phoneNumbers = excluded.phoneNumbers
    )");

    // Привязка значений для вставки
    query.bindValue(":lastName", contact.lastName);
    query.bindValue(":firstName", contact.firstName);
    query.bindValue(":middleName", contact.middleName);
    query.bindValue(":address", contact.address);
    query.bindValue(":birthDate", contact.birthDate);
    query.bindValue(":email", contact.email);
    query.bindValue(":phoneNumbers", contact.phoneNumbers.join(", "));

    if (!query.exec()) {
        QMessageBox::critical(nullptr, "Ошибка", "Не удалось сохранить контакт: " + query.lastError().text());
        return false;
    }
    return true;
}

// Метод для сохранения таблицы в бд
void Database::saveToDatabase(const QVector<Contact> &contacts) {
    if (!initializeDatabase()) {

        // Если базы данных нет, выходим
        QMessageBox::critical(nullptr, "Ошибка", "Не удалось открыть базу данных.");
        return;
    }

    // Очищаем таблицу перед сохранением
    if (!clearDatabase()) {
        QMessageBox::critical(nullptr, "Ошибка", "Не удалось очистить базу данных.");
        return;
    }

    // Сохраняем каждый контакт
    bool allSaved = true;
    for (const auto &contact : contacts) {
        if (!saveContactToDatabase(contact)) {
            allSaved = false;
        }
    }

    if (allSaved) {
        QMessageBox::information(nullptr, "Успех", "Контакты сохранены в базу данных.");
    } else {
        QMessageBox::warning(nullptr, "Ошибка", "Некоторые контакты не были сохранены.");
    }
}

// Метод для загрузки контактов из бд
void Database::loadFromDatabase(QVector<Contact> &contacts) {
    if (!initializeDatabase()) {

        // Если базы данных нет, выходим
        QMessageBox::critical(nullptr, "Ошибка", "Не удалось открыть базу данных.");
        return;
    }
    contacts.clear(); // Очищаем вектор перед загрузкой новых данных

    // Выбираем все данные
    QSqlQuery query("SELECT lastName, firstName, middleName, address, birthDate, email, phoneNumbers FROM contacts");

    if (!query.exec()) {
        QMessageBox::critical(nullptr, "Ошибка", "Не удалось выполнить запрос: " + query.lastError().text());
        return;
    }

    // Обрабатываем результаты запроса
    while (query.next()) {
        Contact contact;

        // Заполняем contact значениеями из бд
        contact.lastName = query.value(0).toString();
        contact.firstName = query.value(1).toString();
        contact.middleName = query.value(2).toString();
        contact.address = query.value(3).toString();
        contact.birthDate = query.value(4).toString();
        contact.email = query.value(5).toString();
        contact.phoneNumbers = query.value(6).toString().split(", ");
        contacts.push_back(contact);
    }

    QMessageBox::information(nullptr, "Успех", "Контакты загружены из базы данных.");
}

// Метод для очистки бд
bool Database::clearDatabase() {
    QSqlQuery query;
    if (!query.exec("DELETE FROM contacts")) {
        qDebug() << "Ошибка очистки базы данных:" << query.lastError().text();
        return false;
    }
    return true;
}
