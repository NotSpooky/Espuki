#ifndef CLICKABLESCENE_H
#define CLICKABLESCENE_H

#include <functional>
#include <QGraphicsScene>
#include <QGraphicsLineItem>

struct ClickableScene : public QGraphicsScene
{
    ClickableScene();
    void mousePressEvent(QGraphicsSceneMouseEvent * event);
    std::function<void(QGraphicsSceneMouseEvent *)> onClick = nullptr;
    void drawLine (QPointF initialPoint, QPointF endPoint) {
        auto line = addLine(initialPoint.x(),initialPoint.y(),endPoint.x(), endPoint.y());
        // Draw in background so that text items appear on top.
        // It seems there's something else for background items.
        line->setZValue(-1);
    }
};

enum PointSide {begin, end};
struct ConnectionEnds {
    static void addPoint (QPointF point, PointSide side);
    static ClickableScene * scene;

    private:
    static bool beginSet;
    static bool endSet;
    static QPointF beginPoint, endPoint;
};

enum Slot {top, bottom, _center};

QPointF findPos (Slot slot, QWidget &wid, QGraphicsItem &firstAncestorItem, QWidget &firstAncestor);

#endif // CLICKABLESCENE_H
