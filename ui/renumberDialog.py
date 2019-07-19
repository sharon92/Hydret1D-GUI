# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'renumberDialog.ui'
#
# Created by: PyQt5 UI code generator 5.12.1
#
# WARNING! All changes made in this file will be lost!

from PyQt5 import QtCore, QtGui, QtWidgets


class Ui_renDialog(object):
    def setupUi(self, renDialog):
        renDialog.setObjectName("renDialog")
        renDialog.resize(312, 556)
        font = QtGui.QFont()
        font.setFamily("Segoe UI")
        renDialog.setFont(font)
        self.gridLayout = QtWidgets.QGridLayout(renDialog)
        self.gridLayout.setObjectName("gridLayout")
        self.cancel = QtWidgets.QPushButton(renDialog)
        self.cancel.setObjectName("cancel")
        self.gridLayout.addWidget(self.cancel, 3, 2, 1, 1)
        self.sheet = QtWidgets.QTableWidget(renDialog)
        self.sheet.setAlternatingRowColors(True)
        self.sheet.setObjectName("sheet")
        self.sheet.setColumnCount(2)
        self.sheet.setRowCount(0)
        item = QtWidgets.QTableWidgetItem()
        self.sheet.setHorizontalHeaderItem(0, item)
        item = QtWidgets.QTableWidgetItem()
        self.sheet.setHorizontalHeaderItem(1, item)
        self.sheet.horizontalHeader().setStretchLastSection(True)
        self.gridLayout.addWidget(self.sheet, 4, 1, 1, 2)
        self.ok = QtWidgets.QPushButton(renDialog)
        self.ok.setObjectName("ok")
        self.gridLayout.addWidget(self.ok, 3, 1, 1, 1)
        self.label = QtWidgets.QLabel(renDialog)
        self.label.setObjectName("label")
        self.gridLayout.addWidget(self.label, 1, 1, 1, 1)
        self.overwrite = QtWidgets.QCheckBox(renDialog)
        self.overwrite.setObjectName("overwrite")
        self.gridLayout.addWidget(self.overwrite, 0, 1, 1, 1)
        self.dataname = QtWidgets.QLineEdit(renDialog)
        self.dataname.setObjectName("dataname")
        self.gridLayout.addWidget(self.dataname, 1, 2, 1, 1)

        self.retranslateUi(renDialog)
        QtCore.QMetaObject.connectSlotsByName(renDialog)

    def retranslateUi(self, renDialog):
        _translate = QtCore.QCoreApplication.translate
        renDialog.setWindowTitle(_translate("renDialog", "Renumber "))
        self.cancel.setText(_translate("renDialog", "cancel"))
        item = self.sheet.horizontalHeaderItem(0)
        item.setText(_translate("renDialog", "Alt"))
        item = self.sheet.horizontalHeaderItem(1)
        item.setText(_translate("renDialog", "Neu"))
        self.ok.setText(_translate("renDialog", "update, save and reload"))
        self.label.setText(_translate("renDialog", "New Filename:"))
        self.overwrite.setText(_translate("renDialog", "Overwrite Files"))
        self.dataname.setToolTip(_translate("renDialog", "<html><head/><body><p>Datei Name für Start, Pro, und Hyd Format</p></body></html>"))




if __name__ == "__main__":
    import sys
    app = QtWidgets.QApplication(sys.argv)
    renDialog = QtWidgets.QDialog()
    ui = Ui_renDialog()
    ui.setupUi(renDialog)
    renDialog.show()
    sys.exit(app.exec_())
