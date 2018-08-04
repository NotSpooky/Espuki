#ifndef NODE_H
#define NODE_H

#include <QLabel>
#include <QWidget>
#include <Qt>
#include <QString>
#include <QGraphicsItem>
#include "ui_nodeeditor.h"
#include "clickablelabel.h"

struct Node : public QWidget {
    explicit Node();
    //void mousePressEvent(QMouseEvent *event);

    ClickableLabel label;
    QString text = "";
    QHBoxLayout * inputs = nullptr;
    QHBoxLayout * outputs = nullptr;
    void setText(QString text);
    QGraphicsItem * itemInScene = nullptr;
};


#endif // NODE_H
