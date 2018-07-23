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

void drawLine (QGraphicsScene &scene, QPointF initialPoint, QPointF endPoint) {
    auto line = scene.addLine(initialPoint.x(),initialPoint.y(),endPoint.x(), endPoint.y());
    // Draw in background so that text items appear on top.
    // It seems there's something else for background items.
    line->setZValue(-1);
}

enum Slot {top, bottom};

// Gets the position on the scene of the top/bottom of an item.
// firstAncestor is the first parent that is on the GraphicsScene.
auto pos (Slot slot, QTextEdit & text, QGraphicsItem & firstAncestorItem, QWidget & firstAncestor) {
    auto size = text.size();
    return firstAncestorItem.mapFromScene(
        text.mapTo(
            &firstAncestor, QPoint(
                size.width() / 2
                , slot == top ? 0 : size.height()
            )
        )
    );
}


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
        auto pos = event->scenePos();
        auto node = new Node();
        auto added = w.scene->addWidget(node);
        auto size = node->size();
        // Prevent activation of other widgets in this position.
        // If this is problematic, can try with sceneEventFilter.
        added->setPanelModality(QGraphicsItem::SceneModal);
        added->setGeometry(QRectF(pos.x(),pos.y(),size.width(),size.height()));
    };
    //mainLisp();

    return a.exec();
}
