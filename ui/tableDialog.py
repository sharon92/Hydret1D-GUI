# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'tableDialog.ui'
#
# Created by: PyQt5 UI code generator 5.12.1
#
# WARNING! All changes made in this file will be lost!

from PyQt5 import QtCore, QtGui, QtWidgets


class Ui_tableDialog(object):
    def setupUi(self, tableDialog):
        tableDialog.setObjectName("tableDialog")
        tableDialog.resize(1035, 717)
        font = QtGui.QFont()
        font.setFamily("Segoe UI")
        tableDialog.setFont(font)
        self.gridLayout = QtWidgets.QGridLayout(tableDialog)
        self.gridLayout.setObjectName("gridLayout")
        self.add = QtWidgets.QPushButton(tableDialog)
        self.add.setObjectName("add")
        self.gridLayout.addWidget(self.add, 0, 10, 1, 1)
        self.paste = QtWidgets.QPushButton(tableDialog)
        self.paste.setObjectName("paste")
        self.gridLayout.addWidget(self.paste, 0, 12, 1, 1)
        self.ok = QtWidgets.QPushButton(tableDialog)
        self.ok.setObjectName("ok")
        self.gridLayout.addWidget(self.ok, 5, 11, 1, 1)
        spacerItem = QtWidgets.QSpacerItem(40, 20, QtWidgets.QSizePolicy.Expanding, QtWidgets.QSizePolicy.Minimum)
        self.gridLayout.addItem(spacerItem, 0, 6, 1, 1)
        self.startpath = QtWidgets.QLineEdit(tableDialog)
        self.startpath.setMinimumSize(QtCore.QSize(400, 0))
        self.startpath.setObjectName("startpath")
        self.gridLayout.addWidget(self.startpath, 0, 2, 1, 4)
        self.lineEdit = QtWidgets.QLineEdit(tableDialog)
        self.lineEdit.setObjectName("lineEdit")
        self.gridLayout.addWidget(self.lineEdit, 3, 2, 1, 12)
        self.label = QtWidgets.QLabel(tableDialog)
        font = QtGui.QFont()
        font.setFamily("Segoe UI")
        font.setBold(True)
        font.setWeight(75)
        self.label.setFont(font)
        self.label.setObjectName("label")
        self.gridLayout.addWidget(self.label, 0, 1, 1, 1)
        self.sheet = QtWidgets.QTableWidget(tableDialog)
        self.sheet.setAlternatingRowColors(True)
        self.sheet.setObjectName("sheet")
        self.sheet.setColumnCount(0)
        self.sheet.setRowCount(0)
        self.gridLayout.addWidget(self.sheet, 4, 1, 1, 13)
        self.remove = QtWidgets.QPushButton(tableDialog)
        self.remove.setObjectName("remove")
        self.gridLayout.addWidget(self.remove, 0, 9, 1, 1)
        self.cancel = QtWidgets.QPushButton(tableDialog)
        self.cancel.setObjectName("cancel")
        self.gridLayout.addWidget(self.cancel, 5, 12, 1, 1)
        self.copy = QtWidgets.QPushButton(tableDialog)
        self.copy.setObjectName("copy")
        self.gridLayout.addWidget(self.copy, 0, 11, 1, 1)
        self.label_2 = QtWidgets.QLabel(tableDialog)
        font = QtGui.QFont()
        font.setFamily("Segoe UI")
        font.setPointSize(8)
        font.setBold(True)
        font.setWeight(75)
        self.label_2.setFont(font)
        self.label_2.setObjectName("label_2")
        self.gridLayout.addWidget(self.label_2, 3, 1, 1, 1)
        self.undo = QtWidgets.QPushButton(tableDialog)
        self.undo.setEnabled(False)
        self.undo.setObjectName("undo")
        self.gridLayout.addWidget(self.undo, 0, 7, 1, 1)
        self.redo = QtWidgets.QPushButton(tableDialog)
        self.redo.setEnabled(False)
        self.redo.setObjectName("redo")
        self.gridLayout.addWidget(self.redo, 0, 8, 1, 1)

        self.retranslateUi(tableDialog)
        QtCore.QMetaObject.connectSlotsByName(tableDialog)

    def retranslateUi(self, tableDialog):
        _translate = QtCore.QCoreApplication.translate
        tableDialog.setWindowTitle(_translate("tableDialog", "Spreadsheet"))
        self.add.setText(_translate("tableDialog", "+"))
        self.paste.setText(_translate("tableDialog", "paste"))
        self.ok.setText(_translate("tableDialog", "update"))
        self.label.setText(_translate("tableDialog", "Start-Datei"))
        self.remove.setText(_translate("tableDialog", "-"))
        self.cancel.setText(_translate("tableDialog", "cancel"))
        self.copy.setText(_translate("tableDialog", "copy"))
        self.label_2.setText(_translate("tableDialog", "Formel:"))
        self.undo.setText(_translate("tableDialog", "undo"))
        self.redo.setText(_translate("tableDialog", "redo"))




if __name__ == "__main__":
    import sys
    app = QtWidgets.QApplication(sys.argv)
    tableDialog = QtWidgets.QDialog()
    ui = Ui_tableDialog()
    ui.setupUi(tableDialog)
    tableDialog.show()
    sys.exit(app.exec_())
