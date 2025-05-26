#include <QInputDialog>
#include <QHeaderView>
#include <QSqlError>
#include "Contacts.h"
#include "Validator.h"
#include "Dialog.h"
#include "File.h"
#include "Database.h"
// Инициализация tableWidget
Contacts::Contacts(QTableWidget *tableWidget)
    : tableWidget(tableWidget) {}

// Метод для добавления нового контакта
void Contacts::addContact()
{
    // Создаем диалоговое окно для добавления контакта
    Dialog dialog;
    while (true) {
        // Показываем диалог и ждем, пока пользователь введет данные
        if (dialog.exec() != QDialog::Accepted) {
            return;  // Если пользователь отменил действие, то выходим из функции
        }

        QString lastName = dialog.getLastName();
        lastName = Validator::trim(lastName);
        if (!Validator::ValidName(lastName)) {
            QMessageBox::warning(nullptr, "Ошибка", "Некорректная фамилия");
            continue;
        }

        // Получаем введенные данные и проверяем корректность
        QString firstName = dialog.getFirstName();
        firstName = Validator::trim(firstName);
        if (!Validator::ValidName(firstName)) {
            QMessageBox::warning(nullptr, "Ошибка", "Некорректное имя");
            continue;
        }

        QString middleName = dialog.getMiddleName();
        middleName = Validator::trim(middleName);
        if (!Validator::ValidName(middleName)) {
            QMessageBox::warning(nullptr, "Ошибка", "Некорректное отчество");
            continue;
        }

        QString address = dialog.getAddress();
        address = Validator::trim(address);
        if (!Validator::ValidAddress(address)) {
            QMessageBox::warning(nullptr, "Ошибка", "Некорректный адрес");
            continue;
        }

        QString birthDate = dialog.getBirthDate();
        //birthDate = birthDate.trimmed();  // Убираем лишние пробеks
        if (!Validator::ValidDate(birthDate)) {
            QMessageBox::warning(nullptr, "Ошибка", "Некорректная дата рождения");
            continue;
        }

        QString email = dialog.getEmail();
        email = Validator::EmailTrimmer(email);
        if (!Validator::ValidEmail(email)) {
            QMessageBox::warning(nullptr, "Ошибка", "Некорректная электронная почта");
            continue;
        }

        QStringList phoneNumbers = dialog.getPhoneNumbers();
        phoneNumbers = Validator::trimPhone(phoneNumbers);
        QStringList validNumbers = Validator::ValidPhoneNumbers(phoneNumbers);
        if (validNumbers.isEmpty()) {
            QMessageBox::warning(nullptr, "Ошибка", "Некорректные телефоны");
            continue;
        }

        // Создание контакта
        Contact contact{firstName, lastName, middleName, address, birthDate, email, phoneNumbers};
        contacts.append(contact);

        // Добавляем контакт в таблицу
        int row = tableWidget->rowCount();
        tableWidget->insertRow(row);

        // Заполнение ячеек таблицы
        tableWidget->setItem(row, 0, new QTableWidgetItem(contact.lastName));
        tableWidget->setItem(row, 1, new QTableWidgetItem(contact.firstName));
        tableWidget->setItem(row, 2, new QTableWidgetItem(contact.middleName));
        tableWidget->setItem(row, 3, new QTableWidgetItem(contact.address));
        tableWidget->setItem(row, 4, new QTableWidgetItem(contact.birthDate));
        tableWidget->setItem(row, 5, new QTableWidgetItem(contact.email));
        tableWidget->setItem(row, 6, new QTableWidgetItem(contact.phoneNumbers.join(", ")));

        QMessageBox::information(nullptr, "Успех", "Контакт добавлен.");
    }
}

// Метод для сохранения контактов
void Contacts::saveContacts() {
    bool ok;
    QStringList options = {"Файл", "База данных"};
    QString choice = QInputDialog::getItem(nullptr, "Выберите способ сохранения", "Куда сохранить контакты?", options, 0, false, &ok);

    if (!ok || choice.isEmpty()) {
        return;
    }

    if (choice == "Файл") {
        File::saveToFile(contacts);
    }

    else if (choice == "База данных") {
        Database::saveToDatabase(contacts);
    }
}

// Метод для загрузки контактов
void Contacts::loadContacts() {
    bool ok;
    QStringList options = {"Файл", "База данных"};
    QString choice = QInputDialog::getItem(nullptr, "Выберите источник загрузки", "Откуда загрузить контакты?", options, 0, false, &ok);

    if (!ok || choice.isEmpty()) {
        return;
    }

    contacts.clear(); // Очищаем текущий список контактов перед загрузкой новых данных

    if (choice == "Файл") {
        File::loadFromFile(contacts);
    }
    else if (choice == "База данных") {
        Database::loadFromDatabase(contacts);
    }
}


// Метод для удаления контакта
void Contacts::deleteContact() {
    QString email = QInputDialog::getText(nullptr, "Удалить контакт", "Введите email контакта для удаления:");

    if (email.isEmpty()) {
        QMessageBox::warning(nullptr, "Ошибка", "Email не может быть пустым.");
        return;
    }

    // Удаление из списка контактов
    auto it = std::find_if(contacts.begin(), contacts.end(), [&](const Contact& contact) {
        return contact.email == email;
    });

    if (it != contacts.end()) {
        contacts.erase(it);
        QMessageBox::information(nullptr, "Успех", "Контакт успешно удалён.");

    } else {
        QMessageBox::warning(nullptr, "Ошибка", "Контакт с таким email не найден.");
    }
}

// Метод для показа контактов
void Contacts::displayContacts() {
    // Очистка таблицы перед обновлением данных
    tableWidget->clearContents();
    tableWidget->setRowCount(contacts.size());

    // Заполнение таблицы данными
    for (int row = 0; row < contacts.size(); ++row) {
        const Contact &contact = contacts[row];
        tableWidget->setItem(row, 0, new QTableWidgetItem(contact.lastName));
        tableWidget->setItem(row, 1, new QTableWidgetItem(contact.firstName));
        tableWidget->setItem(row, 2, new QTableWidgetItem(contact.middleName));
        tableWidget->setItem(row, 3, new QTableWidgetItem(contact.address));
        tableWidget->setItem(row, 4, new QTableWidgetItem(contact.birthDate));
        tableWidget->setItem(row, 5, new QTableWidgetItem(contact.email));
        tableWidget->setItem(row, 6, new QTableWidgetItem(contact.phoneNumbers.join(", ")));
    }

    // Обновление заголовков (если вдруг они не установлены)
    QStringList headers = {"Фамилия", "Имя", "Отчество", "Адрес", "Дата рождения", "Email", "Телефоны"};
    tableWidget->setHorizontalHeaderLabels(headers);
    tableWidget->horizontalHeader()->setSectionResizeMode(QHeaderView::Stretch);
}

// Метод для редактирования контакта
void Contacts::editContact() {
    // Запрашиваем email для поиска контакта
    QString email = Validator::EmailTrimmer(QInputDialog::getText(nullptr, "Редактировать контакт", "Введите email контакта для редактирования:"));

    if (email.isEmpty()) {
        QMessageBox::warning(nullptr, "Ошибка", "Email не может быть пустым.");
        return;
    }

    // Поиск контакта по email
    auto it = std::find_if(contacts.begin(), contacts.end(), [&](const Contact& contact) {
        return contact.email == email;
    });

    if (it == contacts.end()) {
        QMessageBox::warning(nullptr, "Ошибка", "Контакт с таким email не найден.");
        return;
    }

    Contact& contact = *it;

    // Выбор поля для редактирования
    bool ok;
    QStringList options = {"Фамилия", "Имя", "Отчество", "Адрес", "Дата рождения", "Email", "Телефоны"};
    QString selectedOption = QInputDialog::getItem(nullptr, "Выберите поле для редактирования", "Что вы хотите изменить?", options, 0, false, &ok);
    if (!ok || selectedOption.isEmpty()) {
        return;
    }

    // Редактирование выбранного поля с удалением пробелов и валидацией
    if (selectedOption == "Имя") {
        QString newFirstName = Validator::trim(QInputDialog::getText(nullptr, "Редактировать имя", "Введите новое имя:", QLineEdit::Normal, contact.firstName, &ok));
        if (ok && !newFirstName.isEmpty() && Validator::ValidName(newFirstName)) {
            contact.firstName = newFirstName;
        } else {
            QMessageBox::warning(nullptr, "Ошибка", "Неверное имя!");
            return;
        }
    }
    else if (selectedOption == "Фамилия") {
        QString newLastName = Validator::trim(QInputDialog::getText(nullptr, "Редактировать фамилию", "Введите новую фамилию:", QLineEdit::Normal, contact.lastName, &ok));
        if (ok && !newLastName.isEmpty() && Validator::ValidName(newLastName)) {
            contact.lastName = newLastName;
        } else {
            QMessageBox::warning(nullptr, "Ошибка", "Неверная фамилия!");
            return;
        }
    }
    else if (selectedOption == "Отчество") {
        QString newMiddleName = Validator::trim(QInputDialog::getText(nullptr, "Редактировать отчество", "Введите новое отчество:", QLineEdit::Normal, contact.middleName, &ok));
        if (ok && Validator::ValidName(newMiddleName)) {
            contact.middleName = newMiddleName;
        } else {
            QMessageBox::warning(nullptr, "Ошибка", "Неверное отчество!");
            return;
        }
    }
    else if (selectedOption == "Адрес") {
        QString newAddress = QInputDialog::getText(nullptr, "Редактировать адрес", "Введите новый адрес:", QLineEdit::Normal, contact.address, &ok).trimmed();
        if (ok && !newAddress.isEmpty()) {
            contact.address = newAddress;
        } else {
            QMessageBox::warning(nullptr, "Ошибка", "Адрес не может быть пустым!");
            return;
        }
    }
    else if (selectedOption == "Дата рождения") {
        QString newBirthDate = QInputDialog::getText(nullptr, "Редактировать дату рождения", "Введите новую дату рождения (ГГГГ-ММ-ДД):", QLineEdit::Normal, contact.birthDate, &ok).trimmed();
        if (ok && Validator::ValidDate(newBirthDate)) {
            contact.birthDate = newBirthDate;
        } else {
            QMessageBox::warning(nullptr, "Ошибка", "Некорректная дата рождения!");
            return;
        }
    }
    else if (selectedOption == "Email") {
        QString newEmail = Validator::EmailTrimmer(QInputDialog::getText(nullptr, "Редактировать email", "Введите новый email:", QLineEdit::Normal, contact.email, &ok));
        if (ok && Validator::ValidEmail(newEmail)) {
            contact.email = newEmail;
        } else {
            QMessageBox::warning(nullptr, "Ошибка", "Некорректный email!");
            return;
        }
    }
    else if (selectedOption == "Телефоны") {
        QString newPhones = QInputDialog::getText(nullptr, "Редактировать телефоны", "Введите телефоны через запятую:", QLineEdit::Normal, contact.phoneNumbers.join(", "), &ok);
        if (ok) {
            QStringList phoneList = newPhones.split(',', Qt::SkipEmptyParts);
            bool allValid = std::all_of(phoneList.begin(), phoneList.end(), [](const QString& phone) {
                return Validator::ValidPhoneNumber(phone.trimmed());
            });
            if (allValid) {
                contact.phoneNumbers = phoneList;
            } else {
                QMessageBox::warning(nullptr, "Ошибка", "Некоторые номера телефонов некорректны!");
                return;
            }
        }
    }
    QMessageBox::information(nullptr, "Успех", "Контакт успешно обновлён!");
}

void Contacts::sortContacts() {
    // Диалоговое окно для выбора поля сортировки
    bool ok;
    QStringList options = {"Фамилия", "Имя", "Отчество","Адрес", "Дата рождения", "Email", "Телефоны"};
    QString selectedOption = QInputDialog::getItem(nullptr, "Выберите поле для сортировки", "По какому полю сортировать?", options, 0, false, &ok);

    if (!ok || selectedOption.isEmpty()) {
        return;
    }

    // Компаратор для сортировки
    auto comparator = [selectedOption](const Contact& a, const Contact& b) {
        if (selectedOption == "Имя") {
            return a.firstName < b.firstName;
        } else if (selectedOption == "Фамилия") {
            return a.lastName < b.lastName;
        } else if (selectedOption == "Отчество") {
            return a.middleName < b.middleName;
        } else if (selectedOption == "Адрес") {
            return a.address < b.address;
        } else if (selectedOption == "Дата рождения") {
            // Преобразование даты в формат ГГГГММДД
            auto parseDate = [](const QString& date) -> QString {
                QStringList parts = date.split(".");
                if (parts.size() == 3) { // Проверка на корректность формата
                    return parts[2] + parts[1] + parts[0]; // ГГГГ + ММ + ДД
                }
                return "";
            };
            return parseDate(a.birthDate) < parseDate(b.birthDate);
        } else if (selectedOption == "Email") {
            return a.email < b.email;
        }
        else if (selectedOption == "Телефоны") {
            return a.phoneNumbers.first() < b.phoneNumbers.first();
        }
        return false; // На случай, если выбор был пустым
    };

    // Сортировка
    std::sort(contacts.begin(), contacts.end(), comparator);

    // Отображение отсортированных контактов
    QMessageBox::information(nullptr, "Успех", "Контакты отсортированы!");
}


// Метод для поиска
void Contacts::searchContacts() {
    bool ok;
    QString query = QInputDialog::getText(nullptr, "Поиск контакта", "Введите значение для поиска:", QLineEdit::Normal, "", &ok);

    if (!ok || query.isEmpty()) {
        return;
    }

    // Сброс цвета всех строк перед новым поиском
    for (int row = 0; row < tableWidget->rowCount(); ++row) {
        for (int col = 0; col < tableWidget->columnCount(); ++col) {
            tableWidget->item(row, col)->setBackground(Qt::white);
        }
    }

    bool found = false;
    for (int row = 0; row < tableWidget->rowCount(); ++row) {
        for (int col = 0; col < tableWidget->columnCount(); ++col) {
            QTableWidgetItem *item = tableWidget->item(row, col);
            if (item && item->text().contains(query, Qt::CaseInsensitive)) {
                item->setBackground(Qt::yellow);  // Подсветка найденного элемента
                found = true;
            }
        }
    }

    if (!found) {
        QMessageBox::information(nullptr, "Результат поиска", "Контакт не найден.");
    } else {
        QMessageBox::information(nullptr, "Результат поиска", "Контакты найдены и подсвечены.");
    }
}
