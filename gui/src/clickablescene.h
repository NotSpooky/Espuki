#ifndef CLICKABLESCENE_H
#define CLICKABLESCENE_H

#include <functional>
#include <QGraphicsScene>

struct ClickableScene : public QGraphicsScene
{
    ClickableScene();
    void mousePressEvent(QGraphicsSceneMouseEvent * event);
    std::function<void(QGraphicsSceneMouseEvent *)> onClick = nullptr;
};

#endif // CLICKABLESCENE_H
