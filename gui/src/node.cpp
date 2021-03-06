#include "node.h"
#include <QDialogButtonBox>
#include <QTextEdit>
#include <QDebug>
#include <vector>
#include "clickablescene.h"
#include <QMouseEvent>

struct EditorInput {
    QString name = ""; // The text inside.
    // Updated after user accepts.
    // If null, then a new one must be created.
    QLabel * guiElement;
    explicit EditorInput(QLabel * guiElement) {
        this->guiElement = guiElement;
    }
};

#include <QHBoxLayout>
#include <QLineEdit>
struct NodeEditor {
    Ui::frame ui;
    // Used to store the new state of the inputs so that they can be applied when
    // the user clicks accept.
    std::vector<EditorInput> inputTracker;
    QFrame * baseWidget;

    explicit NodeEditor(Node * node) {
        this->inputTracker = std::vector<EditorInput>(); // Maybe not needed.
        this->baseWidget = new QFrame();
        this->ui.setupUi(baseWidget);
        this->ui.textEdit->setText(node->text);

        // On creation, add the input labels as input text edits.
        //printf("Amount of children: %d\n\n", node->inputs->children().length());
        // Cannot use node->inputs->children() because the subnodes aren't children of the layout but of the widget.
        for (unsigned inputPos = 0; inputPos < node->inputs->count(); inputPos++) {
            auto childLabel = qobject_cast<ClickableLabel*> (node->inputs->itemAt(inputPos)->widget());
            if (childLabel != nullptr) {
                // And also keep track of them so that changes can be applied on accept.
                auto inputLabel = new QLineEdit();
                this->inputTracker.push_back(EditorInput(childLabel));
                inputLabel->connect(inputLabel, &QLineEdit::textChanged, [inputLabel, inputPos, this] () {
                    assert(inputTracker.size() > inputPos);
                    this->inputTracker[inputPos].name = inputLabel->text();
                });
                inputLabel->setText(childLabel->text());
                this->ui.inputsLayout->addWidget(inputLabel);
            }
        }

        // When the add input button is clicked, an editor for that input is created.
        baseWidget->connect(this->ui.addInput, &QPushButton::clicked, [this] () {
            auto inputPos = this->ui.inputsLayout->count() - 1;
            auto newInput = new QLineEdit ();
            this->inputTracker.push_back(EditorInput(nullptr));
            newInput->connect(newInput, &QLineEdit::textChanged, [newInput, inputPos, this] () {
                assert(inputTracker.size() > inputPos);
                this->inputTracker[inputPos].name = newInput->text();
            });
            newInput->setText(QString::fromStdString("_" + std::to_string(inputPos + 1)));
            this->ui.inputsLayout->addWidget(newInput);
            // It is also added to the tracking vector.
        });

        // On accept, use the data in the tracking vector to update the inputs in the main interface.
        baseWidget->connect(this->ui.buttonBox, &QDialogButtonBox::accepted, [this, node]() {
            node->setText(this->ui.textEdit->toPlainText());
            foreach (auto newInputData, this->inputTracker) {
                auto text = newInputData.name;
                if(!newInputData.guiElement) {
                    auto newInputLabel = new ClickableLabel();
                    newInputLabel->connect(newInputLabel, &ClickableLabel::clicked, [newInputLabel, node] () {
                        ConnectionEnds::addPoint(findPos(top, *newInputLabel, *(node->itemInScene), *node), PointSide::end);
                    });
                    newInputLabel->setStyleSheet("QFrame {background-color: #ccff99}");
                    node->inputs->addWidget(newInputLabel);
                    newInputData.guiElement = newInputLabel;
                }
                newInputData.guiElement->setText(text);
            }
            this->baseWidget->close();
        });
        baseWidget->show();
    }
};
ConnectionEnds connectionEnds;
Node::Node(): QWidget(nullptr) {

    // Useful for setting the style sheet without affecting children.
    this->setObjectName("frame");
    this->setStyleSheet("#frame { background-color: rgba(0,0,0,0) }");
    this->inputs = new QHBoxLayout();
    this->outputs = new QHBoxLayout();
    //inputsLayout->addWidget(exampleInput2);
    auto lay = new QVBoxLayout(); // Used so that the label can resize.
    lay->setSizeConstraint(QLayout::SetNoConstraint);
    label.setParent(this);
    label.setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
    label.connect(&label, &ClickableLabel::clicked, [this] (QMouseEvent * event) {
        if (event->button() == Qt::LeftButton) {
            NodeEditor * editor = new NodeEditor(this);
        }
    });
    setText("");
    this->setLayout(lay);
    lay->addLayout(inputs);
    lay->addWidget(&(this->label));
    auto exampleOutput = new ClickableLabel();
    exampleOutput->setText("output");
    exampleOutput->setStyleSheet("QFrame {background-color: #ffcc99}");
    exampleOutput->connect(exampleOutput, &ClickableLabel::clicked, [this, exampleOutput]() {
        ConnectionEnds::addPoint(findPos(bottom, *exampleOutput, *itemInScene, *this), PointSide::begin);
    });
    lay->addLayout(outputs);
    outputs->addWidget(exampleOutput);
}




#include <QPushButton>
#include <QMouseEvent>

/*
void Node::mousePressEvent(QMouseEvent *event) {
    if (event->button() == Qt::LeftButton) {
        NodeEditor * editor = new NodeEditor(this);
    }
}
*/

void Node::setText(QString text) {
    this->text = text;
    if (text == "") {
        this->label.setText("<b><font color = \"blue\">Empty node</color></b>");
    } else {
        this->label.setText(text);
    }
}
