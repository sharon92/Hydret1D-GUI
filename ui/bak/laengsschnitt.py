# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'laengsschnitt.ui'
#
# Created by: PyQt5 UI code generator 5.12.1
#
# WARNING! All changes made in this file will be lost!

from PyQt5 import QtCore, QtGui, QtWidgets


class Ui_ls_dock(object):
    def setupUi(self, ls_dock):
        ls_dock.setObjectName("ls_dock")
        ls_dock.resize(792, 642)
        self.dockWidgetContents = QtWidgets.QWidget()
        self.dockWidgetContents.setObjectName("dockWidgetContents")
        self.gridLayout = QtWidgets.QGridLayout(self.dockWidgetContents)
        self.gridLayout.setObjectName("gridLayout")
        self.groupBox_7 = QtWidgets.QGroupBox(self.dockWidgetContents)
        self.groupBox_7.setTitle("")
        self.groupBox_7.setObjectName("groupBox_7")
        self.gridLayout_13 = QtWidgets.QGridLayout(self.groupBox_7)
        self.gridLayout_13.setObjectName("gridLayout_13")
        self.label_93 = QtWidgets.QLabel(self.groupBox_7)
        self.label_93.setObjectName("label_93")
        self.gridLayout_13.addWidget(self.label_93, 0, 0, 1, 1)
        self.lang_ID = QtWidgets.QComboBox(self.groupBox_7)
        self.lang_ID.setObjectName("lang_ID")
        self.gridLayout_13.addWidget(self.lang_ID, 0, 1, 1, 1)
        self.langView = PlotWidget(self.groupBox_7)
        self.langView.setObjectName("langView")
        self.gridLayout_13.addWidget(self.langView, 2, 0, 1, 2)
        self.gridLayout.addWidget(self.groupBox_7, 0, 0, 1, 1)
        ls_dock.setWidget(self.dockWidgetContents)

        self.retranslateUi(ls_dock)
        QtCore.QMetaObject.connectSlotsByName(ls_dock)

    def retranslateUi(self, ls_dock):
        _translate = QtCore.QCoreApplication.translate
        ls_dock.setWindowTitle(_translate("ls_dock", "Längsschnitt"))
        self.label_93.setText(_translate("ls_dock", "Gewässer ID"))


from pyqtgraph import PlotWidget


if __name__ == "__main__":
    import sys
    app = QtWidgets.QApplication(sys.argv)
    ls_dock = QtWidgets.QDockWidget()
    ui = Ui_ls_dock()
    ui.setupUi(ls_dock)
    ls_dock.show()
    sys.exit(app.exec_())
