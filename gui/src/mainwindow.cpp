#include "mainwindow.h"
#include "ui_mainwindow.h"


MainWindow::MainWindow(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::MainWindow)
{
    ui->setupUi(this);
    this->show();
    scene = new QGraphicsScene();
    ui->graphicsView->setScene(scene);
    view = ui->graphicsView;
    view->fitInView(view->sceneRect());
}

MainWindow::~MainWindow()
{
    delete ui;
}
