#include <QMessageBox>
#include <QHeaderView>
#include <QTableWidget>
#include "Contacts.h"
#include "mainwindow.h"

MainWindow::MainWindow(QWidget *parent)
    : QMainWindow(parent), ui(new Ui::MainWindow) {
    ui->setupUi(this);

    // Создание виджетов
    tableWidget = new QTableWidget(this);
    tableWidget->setRowCount(0);
    tableWidget->setColumnCount(7);
    tableWidget->setHorizontalHeaderLabels({"Фамилия", "Имя", "Отчество", "Адрес", "Дата рождения", "Email", "Телефоны"});
    tableWidget->horizontalHeader()->setSectionResizeMode(QHeaderView::Stretch);

    contactsManager = new Contacts(tableWidget);
    // Создание кнопок
    ui->addButton = new QPushButton("Добавить", this);
    ui->saveButton = new QPushButton("Сохранить", this);
    ui->loadButton = new QPushButton("Загрузить", this);
    ui->deleteButton = new QPushButton("Удалить", this);
    ui->displayButton = new QPushButton("Показать", this);
    ui->editButton = new QPushButton("Редактировать", this);
    ui->sortButton = new QPushButton("Сортировать", this);
    ui->searchButton = new QPushButton("Поиск", this);

    // Layout для кнопок
    QHBoxLayout *buttonLayout = new QHBoxLayout;
    buttonLayout->addWidget(ui->addButton);
    buttonLayout->addWidget(ui->saveButton);
    buttonLayout->addWidget(ui->loadButton);
    buttonLayout->addWidget(ui->deleteButton);
    buttonLayout->addWidget(ui->displayButton);
    buttonLayout->addWidget(ui->editButton);
    buttonLayout->addWidget(ui->sortButton);
    buttonLayout->addWidget(ui->searchButton);

    // Основной Layout
    QVBoxLayout *mainLayout = new QVBoxLayout;
    mainLayout->addWidget(tableWidget);
    mainLayout->addLayout(buttonLayout);

    QWidget *centralWidget = new QWidget(this);
    centralWidget->setLayout(mainLayout);
    setCentralWidget(centralWidget);

    // Подключение сигналов к слотам
    connect(ui->addButton, &QPushButton::clicked, this, [this]() {contactsManager->addContact();});
    connect(ui->saveButton, &QPushButton::clicked,this, [this](){contactsManager->saveContacts();});
    connect(ui->loadButton, &QPushButton::clicked,this, [this](){contactsManager->loadContacts();});
    connect(ui->deleteButton, &QPushButton::clicked,this, [this]() {contactsManager->deleteContact();});
    connect(ui->displayButton, &QPushButton::clicked,this, [this](){contactsManager->displayContacts();});
    connect(ui->editButton, &QPushButton::clicked,this, [this]() {contactsManager->editContact();});
    connect(ui->sortButton, &QPushButton::clicked,this, [this](){contactsManager->sortContacts();});
    connect(ui->searchButton, &QPushButton::clicked,this, [this](){contactsManager->searchContacts();});
}


MainWindow::~MainWindow() {
    delete ui;
    delete contactsManager;
}







