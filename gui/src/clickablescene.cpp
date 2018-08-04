#include "clickablescene.h"

ClickableScene::ClickableScene () : QGraphicsScene()
{
    ConnectionEnds::scene = this;
}
#include <QGraphicsSceneMouseEvent>

void ClickableScene::mousePressEvent(QGraphicsSceneMouseEvent *event)
{
    auto subItems = this->items(event->scenePos());
    if (onClick && subItems.count() == 0) {
        // Clicking empty space.
        onClick(event);
    } else {
        QGraphicsScene::mousePressEvent(event);
    }
}

#include <QWidget>
// Gets the position on the scene of the top/bottom of an item.
// firstAncestor is the first parent that is on the GraphicsScene.
QPointF findPos (Slot slot, QWidget & text, QGraphicsItem & firstAncestorItem, QWidget & firstAncestor) {
    auto size = text.size();
    float height = 0;
    switch (slot) {
    case top:
        height = 0;
        break;
    case _center:
        height = size.height() / 2;
        break;
    case bottom:
        height = size.height();
        break;
    default:
        assert(false);
        break;
    }
    return firstAncestorItem.mapToScene(
        text.mapTo(&firstAncestor, QPoint (
           static_cast<int>(size.width() / 2)
           , static_cast<int> (height)
        ))
    );
    /*
    return firstAncestorItem.mapFromScene(
        text.mapTo(
            &firstAncestor, QPoint(
                static_cast<int>(size.width() / 2)
                , static_cast<int> (height)
            )
        )
    );*/
}

bool ConnectionEnds::beginSet = false;
bool ConnectionEnds::endSet = false;
QPointF ConnectionEnds::beginPoint = QPointF();
QPointF ConnectionEnds::endPoint = QPointF();
ClickableScene * ConnectionEnds::scene = nullptr;

void ConnectionEnds::addPoint(QPointF point, PointSide side) {
    assert (scene);
    if (side == begin) {
        beginPoint = point;
        beginSet = true;
        printf("Added begin\n\n");
    } else {
        endPoint = point;
        endSet = true;
        printf("Added end\n\n");
    }
    if (beginSet && endSet) {
        scene->drawLine(beginPoint, endPoint);
        beginSet = false;
        endSet = false;
    }
}
