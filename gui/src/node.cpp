#include "node.h"
#include <QDialogButtonBox>
#include <QTextEdit>
#include <QDebug>
#include <vector>

Node::Node(): QWidget(nullptr) {

    // Useful for setting the style sheet without affecting children.
    this->setObjectName("frame");
    this->setStyleSheet("#frame { background-color: rgba(0,0,0,0) }");
    this->inputs = new QHBoxLayout();
    //exampleInput->setStyleSheet("QFrame {background-color: green}");
    //inputsLayout->addWidget(exampleInput2);
    auto lay = new QVBoxLayout(); // Used so that the label can resize.
    lay->setSizeConstraint(QLayout::SetNoConstraint);
    label.setParent(this);
    label.setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
    setText("");
    this->setLayout(lay);
    lay->addLayout(inputs);
    lay->addWidget(&(this->label));
}


struct EditorInput {
    QString name = "Test text"; // The text inside.
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
        foreach (auto input, node->inputs->children()){
            auto childLabel = qobject_cast<QLabel*> (input);
            if (childLabel != nullptr) {
                // And also keep track of them so that changes can be applied on accept.
                this->inputTracker.push_back(EditorInput(childLabel));
                auto inputLabel = new QLabel();
                inputLabel->setText(childLabel->text());
                this->ui.inputsLayout->addWidget(inputLabel);
            }
        }

        // When the add input button is clicked, an editor for that input is created.
        baseWidget->connect(this->ui.addInput, &QPushButton::clicked, [this] () {
            auto inputPos = this->ui.inputsLayout->count();
            auto newInput = new QLineEdit ();
            newInput->setText(QString::fromStdString("_" + std::to_string(inputPos)));
            this->ui.inputsLayout->addWidget(newInput);
            // It is also added to the tracking vector.
            this->inputTracker.push_back(EditorInput(nullptr));
        });

        // On accept, use the data in the traking vector to update the inputs in the main interface.
        baseWidget->connect(this->ui.buttonBox, &QDialogButtonBox::accepted, [this, node]() {
            node->setText(this->ui.textEdit->toPlainText());
            foreach (auto newInputData, this->inputTracker) {
                auto text = newInputData.name;
                if(!newInputData.guiElement) {
                    auto newInputLabel = new QLabel();
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

#include <QPushButton>

void Node::mousePressEvent(QMouseEvent *event) {
    NodeEditor * editor = new NodeEditor(this);
}

void Node::setText(QString text) {
    this->text = text;
    if (text == "") {
        this->label.setText("<b><font color = \"blue\">Empty node</color></b>");
    } else {
        this->label.setText(text);
    }
}
