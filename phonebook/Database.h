#ifndef DATABASE_H
#define DATABASE_H

#include <QSqlDatabase>
#include <QSqlQuery>
#include <QString>
#include <QMessageBox>
#include "Contacts.h"

class Database {

public:
    Database();
    ~Database();

    // Статические методы для управления бд
    static bool initializeDatabase();
    static bool saveContactToDatabase(const Contact& contact);
    static void saveToDatabase(const QVector<Contact> &contacts);
    static void loadFromDatabase(QVector<Contact> &contacts);
    static bool clearDatabase();

private:
    QSqlDatabase db; // Соединение с бд
};

#endif // DATABASE_H
