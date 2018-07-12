#include "mainwindow.h"
#include <QApplication>
#include <QGraphicsView>
#include <QTextEdit>
#include <QGraphicsProxyWidget>
#include <QHBoxLayout>
#include <QVBoxLayout>
#include <iostream>

void drawLine (QGraphicsScene &scene, QPointF initialPoint, QPointF endPoint) {
    auto line = scene.addLine(initialPoint.x(),initialPoint.y(),endPoint.x(), endPoint.y());
    // Draw in background so that text items appear on top.
    line->setZValue(-1);
}

enum Slot {input, output};
// firstAncestor is the parent that is on the graphicsScene
auto pos (Slot slot, QTextEdit & text, QGraphicsItem & firstAncestorItem, QWidget & firstAncestor) {
    auto size = text.size();
    return firstAncestorItem.mapFromScene(
        text.mapTo(
            &firstAncestor, QPoint(
                size.width() / 2
                , slot == input ? 0 : size.height()
            )
        )
    );
}

struct TextNode : public QTextEdit {
    double horizontalSize = 200.0;
    void fitToText () {
        resize(horizontalSize, document()->size().height() + 20);
        auto par = parentWidget();
        if (par) {
            par->adjustSize();
        }
    }
    TextNode () : QTextEdit() {
        setMinimumSize(QSize(horizontalSize, 30));
        setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
        fitToText();
        connect(this, &QTextEdit::textChanged, this, &TextNode::fitToText);
    }
};

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);
    MainWindow w;
    //scene -> addRect(150, 0, 90, 90);
    auto frame = new QWidget();
    frame->setObjectName("frame");
    // Make the frame transparent but not its children.
    frame->setStyleSheet("#frame { background-color: rgba(0,0,0,0) }");
    QVBoxLayout horizontalL;
    horizontalL.setSpacing(10);
    horizontalL.addStretch();
    frame->setLayout(&horizontalL);
    TextNode edit;
    edit.setText("A");
    TextNode edit2;
    auto textElement = w.scene -> addWidget(frame);
    edit2.setText("Elemento 2");
    horizontalL.addWidget(&edit);
    horizontalL.addWidget(&edit2);

    edit.connect(&edit, &QTextEdit::textChanged, [&]() {
        drawLine(
            *w.scene
            , pos(output, edit, *textElement, *frame)
            , pos(input, edit2, *textElement, *frame)
        );
    });
    assert(w.scene);
    w.view->fitInView(w.view->sceneRect(), Qt::KeepAspectRatio);
    return a.exec();
}
