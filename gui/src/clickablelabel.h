// Copied from https://wiki.qt.io/Clickable_QLabel

#ifndef CLICKABLELABEL_H
#define CLICKABLELABEL_H

#include <QLabel>
#include <QWidget>
#include <Qt>
#include <QString>
#include "ui_nodeeditor.h"

class ClickableLabel : public QLabel {
    Q_OBJECT

public:
    explicit ClickableLabel(QWidget* parent = Q_NULLPTR, Qt::WindowFlags f = Qt::WindowFlags());
    ~ClickableLabel();
    QString text = "";
    Ui::frame ui;
    void setText(QString text);

signals:
    void clicked(QMouseEvent* event);

protected:
    void mousePressEvent(QMouseEvent* event);

};

#endif // CLICKABLELABEL_H
