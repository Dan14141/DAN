#include "Dialog.h"
#include <QVBoxLayout>
#include <QPushButton>
#include <QFormLayout>
#include <QLineEdit>
#include <QLabel>
#include <QMessageBox>

    // Инициалзация диалогового окна
    Dialog::Dialog(QWidget *parent) :
    QDialog(parent)
{
    // Инициализация элементов
    lastNameEdit = new QLineEdit(this);
    firstNameEdit = new QLineEdit(this);
    middleNameEdit = new QLineEdit(this);
    addressEdit = new QLineEdit(this);
    birthDateEdit = new QLineEdit(this);
    emailEdit = new QLineEdit(this);
    phoneEdit = new QLineEdit(this);

    // Создание кнопки, соединение сигнала и слота
    addButton = new QPushButton("Добавить контакт", this);
    connect(addButton, &QPushButton::clicked, this, &QDialog::accept);

    // Расположение элементов
    QFormLayout *formLayout = new QFormLayout;
    formLayout->addRow("Фамилия:", lastNameEdit);
    formLayout->addRow("Имя:", firstNameEdit);
    formLayout->addRow("Отчество:", middleNameEdit);
    formLayout->addRow("Адрес:", addressEdit);
    formLayout->addRow("Дата рождения:", birthDateEdit);
    formLayout->addRow("Email:", emailEdit);
    formLayout->addRow("Телефоны (через запятую):", phoneEdit);

    QVBoxLayout *mainLayout = new QVBoxLayout;
    mainLayout->addLayout(formLayout);
    mainLayout->addWidget(addButton);

    setLayout(mainLayout);
    setWindowTitle("Добавить контакт");
    setFixedSize(300, 300);
}


Dialog::~Dialog(){}


QString Dialog::getLastName() const
{
    return lastNameEdit->text();
}

QString Dialog::getFirstName() const
{
    return firstNameEdit->text();
}

QString Dialog::getMiddleName() const
{
    return middleNameEdit->text();
}

QString Dialog::getAddress() const
{
    return addressEdit->text();
}

QString Dialog::getBirthDate() const
{
    return birthDateEdit->text();
}

QString Dialog::getEmail() const
{
    return emailEdit->text();
}

QStringList Dialog::getPhoneNumbers() const
{
    return phoneEdit->text().split(",", Qt::SkipEmptyParts); // пропуск пробелов и лишних запятых
}
