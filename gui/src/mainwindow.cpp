#include "mainwindow.h"
#include "ui_mainwindow.h"
#include "clickablescene.h"

MainWindow::MainWindow(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::MainWindow)
{
    ui->setupUi(this);
    this->show();
    scene = new ClickableScene();
    ui->graphicsView->setScene(scene);
    view = ui->graphicsView;
    //view->fitInView(scene->sceneRect());
}

MainWindow::~MainWindow()
{
    delete ui;
}
