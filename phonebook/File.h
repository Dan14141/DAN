#ifndef FILE_H
#define FILE_H

#include <QFile>
#include <QTextStream>
#include <QMessageBox>
#include "Contacts.h"

class File {

public:
    // Статические методы для работы с файлом
    static void saveToFile(const QVector<Contact> &contacts);
    static void loadFromFile(QVector<Contact> &contacts);

};
#endif // FILE_H
