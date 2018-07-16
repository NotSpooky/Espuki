// Copied from https://wiki.qt.io/Clickable_QLabel

#include "clickablelabel.h"
#include <QDialogButtonBox>
#include <QTextEdit>
#include <QDebug>
#include <iostream>

ClickableLabel::ClickableLabel(QWidget* parent, Qt::WindowFlags f)
    : QLabel(parent) {
    setText("");
    setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
    connect(this, &ClickableLabel::clicked, [&](QMouseEvent * event) {
        auto widget = new QFrame();
        ui.setupUi(widget);
        ui.textEdit->setText(this->text);
        connect(ui.buttonBox, &QDialogButtonBox::accepted, [this, widget]() {
            setText(ui.textEdit->toPlainText());
            widget->close();
        });
        widget->show();
    });

}

ClickableLabel::~ClickableLabel() {}

void ClickableLabel::setText(QString text) {
    this->text = text;
    if (text == "") {
        QLabel::setText("<b><font color = \"blue\">Empty node</color></b>");
    } else {
        QLabel::setText(text);
    }
}

#include <QMouseEvent>
void ClickableLabel::mousePressEvent(QMouseEvent* event) {
    emit clicked(event);
}
