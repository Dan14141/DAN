#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QtWidgets/QMainWindow>
#include <QtWidgets/QLineEdit>
#include <QtWidgets/QTextEdit>
#include <QtWidgets/QPushButton>
#include <QtWidgets/QVBoxLayout>
#include <QtWidgets/QHBoxLayout>
#include <QtWidgets/QLabel>
#include <QtWidgets/QWidget>
#include <QVector>
#include <QString>
#include <QFile>
#include <QTextStream>
#include <QTableWidget>
#include "Contacts.h"
QT_BEGIN_NAMESPACE

class Ui_MainWindow {

public:
    QWidget *centralWidget;
    QVBoxLayout *mainLayout;

    // Кнопки
    QPushButton *addButton;
    QPushButton *saveButton;
    QPushButton *loadButton;
    QPushButton *deleteButton;
    QPushButton *displayButton;
    QPushButton *editButton;
    QPushButton *sortButton;
    QPushButton *searchButton;

    // Поле для отображения контактов
    QTextEdit *textEdit;

    void setupUi(QMainWindow *MainWindow)
    {
        // Инициализация главного окна
        MainWindow->setWindowTitle(QString::fromUtf8("Телефонный справочник"));
        centralWidget = new QWidget(MainWindow);
        mainLayout = new QVBoxLayout(centralWidget);

        // Текстовое поле для отображения данных
        textEdit = new QTextEdit();
        textEdit->setReadOnly(true);
        mainLayout->addWidget(textEdit);

        // Завершение настройки интерфейса
        MainWindow->setCentralWidget(centralWidget);
    }
};

namespace Ui {
class MainWindow : public Ui_MainWindow {};
}

QT_END_NAMESPACE

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    MainWindow(QWidget *parent = nullptr);
    ~MainWindow();
private slots:

private:
    QVector<Contact> contacts;
    Contacts *contactsManager;
    Ui::MainWindow *ui;
    QTableWidget *tableWidget;
    void clearInputFields();
    void setupDatabase();   // Метод для настройки базы данных
    void setupTableWidget(); // Метод для настройки таблицы
};

#endif // MAINWINDOW_H





