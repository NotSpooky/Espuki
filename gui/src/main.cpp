// TODO: Check if setMask is a better option for the click events.

#include "mainwindow.h"
#include <QApplication>
#include <QGraphicsView>
#include <QTextEdit>
#include <QGraphicsProxyWidget>
#include <QGraphicsSceneMouseEvent>
#include <QHBoxLayout>
#include <QVBoxLayout>
#include <iostream>
#include "node.h"


#include "mainlisp.h"
int main(int argc, char *argv[])
{
    QApplication a(argc, argv);
    MainWindow w;
    //scene -> addRect(150, 0, 90, 90);
    /*
    auto frame = new QWidget();
    // Make the frame transparent but not its children.
    horizontalL.setSpacing(10);
    horizontalL.addStretch();
    frame->setLayout(&horizontalL);
    TextNode edit;
    edit.setText("A");
    TextNode edit2;
    assert(w.scene);
    auto textElement = w.scene -> addWidget(frame);
    edit2.setText("Elemento 2");
    horizontalL.addWidget(&edit);
    horizontalL.addWidget(&edit2);
    */

    //auto nodeText = "";
    w.view->fitInView(w.view->sceneRect(), Qt::KeepAspectRatio);
    w.scene->onClick = [&](QGraphicsSceneMouseEvent* event) {
        if (event->button() == Qt::LeftButton) {
            auto pos = event->scenePos();
            auto node = new Node();
            auto added = w.scene->addWidget(node);
            node->itemInScene = added;
            auto size = node->size();
            // Prevent activation of other widgets in this position.
            // If this is problematic, can try with sceneEventFilter.
            added->setPanelModality(QGraphicsItem::SceneModal);
            added->setGeometry(QRectF(pos.x(),pos.y(),size.width(),size.height()));
        } else if (event->button() == Qt::RightButton) {
            // Lol
        }
    };
    //mainLisp();

    return a.exec();
}
