#ifndef CLICKABLELABEL_H
#define CLICKABLELABEL_H

#include <QLabel>
#include <QWidget>
#include <Qt>
#include <QString>
#include "ui_nodeeditor.h"

struct Node : public QWidget {
    explicit Node();
    void mousePressEvent(QMouseEvent *event);

    QLabel label;
    QString text = "";
    void setText(QString text);
    QHBoxLayout * inputs;
};


#endif // CLICKABLELABEL_H
