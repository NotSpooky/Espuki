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
#include "clickablelabel.h"

void drawLine (QGraphicsScene &scene, QPointF initialPoint, QPointF endPoint) {
    auto line = scene.addLine(initialPoint.x(),initialPoint.y(),endPoint.x(), endPoint.y());
    // Draw in background so that text items appear on top.
    // It seems there's something else for background items.
    line->setZValue(-1);
}

enum Slot {input, output};

// Gets the position on the scene of the input/output of text.
// firstAncestor is the parent that is on the graphicsScene.
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

struct TextNode : public QLabel {
    double horizontalSize = 200.0;
    void fitToText () {
        /*
        resize(horizontalSize, document()->size().height() + 20);
        auto par = parentWidget();
        if (par) {
            par->adjustSize();
        }*/
    }
    TextNode () : QLabel() {
        setMinimumSize(QSize(horizontalSize, 30));
        setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
        fitToText();
        //connect(this, &QTextEdit::textChanged, this, &TextNode::fitToText);
    }
};

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);
    MainWindow w;
    //scene -> addRect(150, 0, 90, 90);
    /*
    auto frame = new QWidget();
    frame->setObjectName("frame");
    // Make the frame transparent but not its children.
    frame->setStyleSheet("#frame { background-color: rgba(0,0,0,0) }");
    horizontalL.setSpacing(10);
    horizontalL.addStretch();
    frame->setLayout(&horizontalL);
    */
    /*
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
    w.scene->onClick = [&](QGraphicsSceneMouseEvent* event){
        auto pos = event->scenePos();
        auto frame = new QWidget();
        auto lay = new QHBoxLayout(); // Used so that the label can resize.
        //lay->addStretch();
        frame -> setLayout(lay);
        auto emptyNode = new ClickableLabel();
        lay->addWidget(emptyNode);
        auto added = w.scene->addWidget(frame);
        auto size = emptyNode-> size();
        // Prevent activation of other widgets in this position.
        // If this is problematic, can try with sceneEventFilter.
        added->setPanelModality(QGraphicsItem::SceneModal);
        added->setGeometry(QRectF(pos.x(),pos.y(),size.width(),size.height()));
    };

    return a.exec();
}
