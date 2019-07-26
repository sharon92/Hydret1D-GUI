# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'lageplan.ui'
#
# Created by: PyQt5 UI code generator 5.12.1
#
# WARNING! All changes made in this file will be lost!

from PyQt5 import QtCore, QtGui, QtWidgets


class Ui_lp_dock(object):
    def setupUi(self, lp_dock):
        lp_dock.setObjectName("lp_dock")
        lp_dock.resize(981, 739)
        font = QtGui.QFont()
        font.setFamily("Segoe UI")
        lp_dock.setFont(font)
        icon = QtGui.QIcon()
        icon.addPixmap(QtGui.QPixmap("../icons/hydret1D.ico"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        lp_dock.setWindowIcon(icon)
        lp_dock.setAutoFillBackground(True)
        self.dockWidgetContents = QtWidgets.QWidget()
        self.dockWidgetContents.setObjectName("dockWidgetContents")
        self.gridLayout = QtWidgets.QGridLayout(self.dockWidgetContents)
        self.gridLayout.setObjectName("gridLayout")
        self.nodeView = PlotWidget(self.dockWidgetContents)
        self.nodeView.setMinimumSize(QtCore.QSize(200, 200))
        self.nodeView.setObjectName("nodeView")
        self.gridLayout.addWidget(self.nodeView, 0, 0, 2, 1)
        self.lp_adddata = QtWidgets.QPushButton(self.dockWidgetContents)
        self.lp_adddata.setObjectName("lp_adddata")
        self.gridLayout.addWidget(self.lp_adddata, 0, 1, 1, 1)
        self.lp_removedata = QtWidgets.QPushButton(self.dockWidgetContents)
        self.lp_removedata.setObjectName("lp_removedata")
        self.gridLayout.addWidget(self.lp_removedata, 0, 2, 1, 1)
        self.lp_listWidget = QtWidgets.QListWidget(self.dockWidgetContents)
        self.lp_listWidget.setDragEnabled(True)
        self.lp_listWidget.setDefaultDropAction(QtCore.Qt.TargetMoveAction)
        self.lp_listWidget.setAlternatingRowColors(True)
        self.lp_listWidget.setMovement(QtWidgets.QListView.Snap)
        self.lp_listWidget.setViewMode(QtWidgets.QListView.ListMode)
        self.lp_listWidget.setObjectName("lp_listWidget")
        self.gridLayout.addWidget(self.lp_listWidget, 1, 1, 1, 2)
        lp_dock.setWidget(self.dockWidgetContents)

        self.retranslateUi(lp_dock)
        QtCore.QMetaObject.connectSlotsByName(lp_dock)

    def retranslateUi(self, lp_dock):
        _translate = QtCore.QCoreApplication.translate
        lp_dock.setWindowTitle(_translate("lp_dock", "Lageplan"))
        self.lp_adddata.setText(_translate("lp_dock", "Dateset hinzufügen"))
        self.lp_removedata.setText(_translate("lp_dock", "Dataset löschen"))
        self.lp_listWidget.setSortingEnabled(True)


from pyqtgraph import PlotWidget


if __name__ == "__main__":
    import sys
    app = QtWidgets.QApplication(sys.argv)
    lp_dock = QtWidgets.QDockWidget()
    ui = Ui_lp_dock()
    ui.setupUi(lp_dock)
    lp_dock.show()
    sys.exit(app.exec_())
