#ifndef DIALOG_H
#define DIALOG_H

#include <QDialog>
#include <QLineEdit>
#include <QPushButton>
#include <QFormLayout>
#include <QLabel>
#include <QVBoxLayout>
#include <QStringList>

class Dialog : public QDialog{

    Q_OBJECT

public:
    explicit Dialog(QWidget *parent = nullptr);
    ~Dialog();

    // Методы получения данных
    QString getLastName() const;
    QString getFirstName() const;
    QString getMiddleName() const;
    QString getAddress() const;
    QString getBirthDate() const;
    QString getEmail() const;
    QStringList getPhoneNumbers() const;

private:

    // Поля для ввода данных
    QLineEdit *lastNameEdit;
    QLineEdit *firstNameEdit;
    QLineEdit *middleNameEdit;
    QLineEdit *addressEdit;
    QLineEdit *birthDateEdit;
    QLineEdit *emailEdit;
    QLineEdit *phoneEdit;
    QPushButton *addButton;
};

#endif // DIALOG_H

