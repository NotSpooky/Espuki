#include "clickablescene.h"

ClickableScene::ClickableScene () : QGraphicsScene()
{
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
