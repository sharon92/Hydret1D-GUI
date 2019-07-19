# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'runDialog.ui'
#
# Created by: PyQt5 UI code generator 5.12.1
#
# WARNING! All changes made in this file will be lost!

from PyQt5 import QtCore, QtGui, QtWidgets


class Ui_runDialog(object):
    def setupUi(self, runDialog):
        runDialog.setObjectName("runDialog")
        runDialog.resize(710, 516)
        font = QtGui.QFont()
        font.setFamily("Segoe UI")
        runDialog.setFont(font)
        self.verticalLayout = QtWidgets.QVBoxLayout(runDialog)
        self.verticalLayout.setObjectName("verticalLayout")
        self.groupBox = QtWidgets.QGroupBox(runDialog)
        self.groupBox.setTitle("")
        self.groupBox.setObjectName("groupBox")
        self.gridLayout_2 = QtWidgets.QGridLayout(self.groupBox)
        self.gridLayout_2.setObjectName("gridLayout_2")
        self.dataname = QtWidgets.QLineEdit(self.groupBox)
        self.dataname.setObjectName("dataname")
        self.gridLayout_2.addWidget(self.dataname, 2, 0, 1, 1)
        self.label = QtWidgets.QLabel(self.groupBox)
        self.label.setMaximumSize(QtCore.QSize(80, 16777215))
        font = QtGui.QFont()
        font.setFamily("Segoe UI")
        font.setPointSize(10)
        font.setBold(True)
        font.setWeight(75)
        self.label.setFont(font)
        self.label.setObjectName("label")
        self.gridLayout_2.addWidget(self.label, 3, 0, 1, 1)
        self.runbut = QtWidgets.QPushButton(self.groupBox)
        self.runbut.setObjectName("runbut")
        self.gridLayout_2.addWidget(self.runbut, 4, 1, 1, 1)
        self.cancel = QtWidgets.QPushButton(self.groupBox)
        self.cancel.setObjectName("cancel")
        self.gridLayout_2.addWidget(self.cancel, 6, 1, 1, 1)
        self.ok = QtWidgets.QPushButton(self.groupBox)
        self.ok.setObjectName("ok")
        self.gridLayout_2.addWidget(self.ok, 5, 1, 1, 1)
        self.overwrite = QtWidgets.QCheckBox(self.groupBox)
        self.overwrite.setMaximumSize(QtCore.QSize(70, 16777215))
        self.overwrite.setChecked(True)
        self.overwrite.setObjectName("overwrite")
        self.gridLayout_2.addWidget(self.overwrite, 2, 1, 1, 1)
        spacerItem = QtWidgets.QSpacerItem(20, 40, QtWidgets.QSizePolicy.Minimum, QtWidgets.QSizePolicy.Expanding)
        self.gridLayout_2.addItem(spacerItem, 7, 1, 1, 1)
        self.p_name = QtWidgets.QLabel(self.groupBox)
        self.p_name.setMaximumSize(QtCore.QSize(16777215, 15))
        self.p_name.setText("")
        self.p_name.setObjectName("p_name")
        self.gridLayout_2.addWidget(self.p_name, 1, 0, 1, 1)
        self.label_2 = QtWidgets.QLabel(self.groupBox)
        self.label_2.setMaximumSize(QtCore.QSize(90, 16777215))
        font = QtGui.QFont()
        font.setPointSize(10)
        font.setBold(True)
        font.setWeight(75)
        self.label_2.setFont(font)
        self.label_2.setObjectName("label_2")
        self.gridLayout_2.addWidget(self.label_2, 0, 0, 1, 1)
        self.movie = QtWidgets.QLabel(self.groupBox)
        self.movie.setMinimumSize(QtCore.QSize(120, 120))
        self.movie.setMaximumSize(QtCore.QSize(120, 120))
        self.movie.setText("")
        self.movie.setObjectName("movie")
        self.gridLayout_2.addWidget(self.movie, 8, 1, 1, 1)
        self.plainTextEdit = QtWidgets.QPlainTextEdit(self.groupBox)
        self.plainTextEdit.setObjectName("plainTextEdit")
        self.gridLayout_2.addWidget(self.plainTextEdit, 4, 0, 5, 1)
        self.verticalLayout.addWidget(self.groupBox)

        self.retranslateUi(runDialog)
        QtCore.QMetaObject.connectSlotsByName(runDialog)

    def retranslateUi(self, runDialog):
        _translate = QtCore.QCoreApplication.translate
        runDialog.setWindowTitle(_translate("runDialog", "Run Hydret06"))
        self.label.setText(_translate("runDialog", "Monitor:"))
        self.runbut.setText(_translate("runDialog", "Run"))
        self.cancel.setText(_translate("runDialog", "Close"))
        self.ok.setText(_translate("runDialog", "Update and Close"))
        self.overwrite.setText(_translate("runDialog", "Backup"))
        self.label_2.setText(_translate("runDialog", "Backup Folder:"))
        self.plainTextEdit.setPlaceholderText(_translate("runDialog", "Press Run to start Hydret06..."))




if __name__ == "__main__":
    import sys
    app = QtWidgets.QApplication(sys.argv)
    runDialog = QtWidgets.QDialog()
    ui = Ui_runDialog()
    ui.setupUi(runDialog)
    runDialog.show()
    sys.exit(app.exec_())
