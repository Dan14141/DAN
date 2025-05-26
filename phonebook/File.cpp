#include "File.h"

// Метод для сохранения контактов в файл
void File::saveToFile(const QVector<Contact> &contacts) {
    QFile file("contacts.txt");
    if (file.open(QIODevice::WriteOnly | QIODevice::Text)) {
        QTextStream out(&file);

        // Записываем каждый контакт в файл
        for (const auto& contact : contacts) {
            out << contact.lastName << "|"
                << contact.firstName << "|"
                << contact.middleName << "|"
                << contact.address << "|"
                << contact.birthDate << "|"
                << contact.email << "|"
                << contact.phoneNumbers.join(";") << "\n";
        }

        file.close();
        QMessageBox::information(nullptr, "Успех", "Контакты сохранены в файл.");
    } else {
        QMessageBox::critical(nullptr, "Ошибка", "Не удалось открыть файл для записи.");
    }
}

// Метод для загрузки контактов из файла
void File::loadFromFile(QVector<Contact> &contacts) {
    QFile file("contacts.txt");
    if (!file.open(QIODevice::ReadOnly | QIODevice::Text)) {
        QMessageBox::warning(nullptr, "Ошибка", "Не удалось открыть файл.");
        return;
    }

    QTextStream in(&file);
    contacts.clear();
    while (!in.atEnd()) {
        QString line = in.readLine();
        QStringList fields = line.split('|');
        if (fields.size() >= 7) {
            Contact contact;
            contact.lastName = fields[0];
            contact.firstName = fields[1];
            contact.middleName = fields[2];
            contact.address = fields[3];
            contact.birthDate = fields[4];
            contact.email = fields[5];
            contact.phoneNumbers = fields[6].split(";");
            contacts.append(contact);
        }
    }

    file.close();
    QMessageBox::information(nullptr, "Успех", "Контакты загружены из файла.");
}


