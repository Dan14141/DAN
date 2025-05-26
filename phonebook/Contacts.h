#ifndef CONTACTS_H
#define CONTACTS_H

#include <QVector>
#include <QString>
#include <QTableWidget>
#include <QMessageBox>

// Структура, представляющая контакт
struct Contact {
    QString lastName;
    QString firstName;
    QString middleName;
    QString address;
    QString birthDate;
    QString email;
    QVector<QString> phoneNumbers;
};

class Contacts {

public:
    explicit Contacts(QTableWidget *tableWidget);

// Методы для управления контактами, вызываются по сигналу
public slots:
    void addContact();
    void saveContacts();
    void loadContacts();
    void deleteContact();
    void displayContacts();
    void editContact();
    void sortContacts();
    void searchContacts();

private:
    QTableWidget *tableWidget; // Указатель на виджет таблицы
    QVector<Contact> contacts;
};

#endif // CONTACTS_H



