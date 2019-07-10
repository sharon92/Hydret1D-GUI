# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'raudialog.ui'
#
# Created by: PyQt5 UI code generator 5.12.1
#
# WARNING! All changes made in this file will be lost!

from PyQt5 import QtCore, QtGui, QtWidgets


class Ui_raudialog(object):
    def setupUi(self, raudialog):
        raudialog.setObjectName("raudialog")
        raudialog.resize(737, 695)
        self.gridLayout = QtWidgets.QGridLayout(raudialog)
        self.gridLayout.setObjectName("gridLayout")
        self.label_5 = QtWidgets.QLabel(raudialog)
        self.label_5.setMaximumSize(QtCore.QSize(16777215, 10))
        font = QtGui.QFont()
        font.setBold(True)
        font.setWeight(75)
        self.label_5.setFont(font)
        self.label_5.setObjectName("label_5")
        self.gridLayout.addWidget(self.label_5, 0, 0, 1, 2)
        self.count = QtWidgets.QLCDNumber(raudialog)
        self.count.setMinimumSize(QtCore.QSize(0, 70))
        self.count.setMaximumSize(QtCore.QSize(16777215, 80))
        self.count.setObjectName("count")
        self.gridLayout.addWidget(self.count, 1, 4, 2, 2)
        self.nodes_table = QtWidgets.QTableWidget(raudialog)
        self.nodes_table.setEditTriggers(QtWidgets.QAbstractItemView.NoEditTriggers)
        self.nodes_table.setProperty("showDropIndicator", False)
        self.nodes_table.setDragDropOverwriteMode(False)
        self.nodes_table.setAlternatingRowColors(True)
        self.nodes_table.setSelectionBehavior(QtWidgets.QAbstractItemView.SelectRows)
        self.nodes_table.setColumnCount(4)
        self.nodes_table.setObjectName("nodes_table")
        self.nodes_table.setRowCount(0)
        item = QtWidgets.QTableWidgetItem()
        self.nodes_table.setHorizontalHeaderItem(0, item)
        item = QtWidgets.QTableWidgetItem()
        self.nodes_table.setHorizontalHeaderItem(1, item)
        item = QtWidgets.QTableWidgetItem()
        self.nodes_table.setHorizontalHeaderItem(2, item)
        item = QtWidgets.QTableWidgetItem()
        self.nodes_table.setHorizontalHeaderItem(3, item)
        self.nodes_table.horizontalHeader().setStretchLastSection(True)
        self.nodes_table.verticalHeader().setVisible(False)
        self.gridLayout.addWidget(self.nodes_table, 1, 0, 14, 3)
        self.cancel = QtWidgets.QPushButton(raudialog)
        self.cancel.setAutoDefault(False)
        self.cancel.setObjectName("cancel")
        self.gridLayout.addWidget(self.cancel, 14, 5, 1, 1)
        self.ok = QtWidgets.QPushButton(raudialog)
        self.ok.setAutoDefault(False)
        self.ok.setObjectName("ok")
        self.gridLayout.addWidget(self.ok, 14, 4, 1, 1)
        self._schaltercheck = QtWidgets.QCheckBox(raudialog)
        self._schaltercheck.setObjectName("_schaltercheck")
        self.gridLayout.addWidget(self._schaltercheck, 3, 5, 1, 1)
        self._swert = QtWidgets.QLineEdit(raudialog)
        self._swert.setEnabled(False)
        self._swert.setMinimumSize(QtCore.QSize(0, 0))
        self._swert.setMaximumSize(QtCore.QSize(120, 16777215))
        self._swert.setObjectName("_swert")
        self.gridLayout.addWidget(self._swert, 4, 5, 1, 1)
        self.label_4 = QtWidgets.QLabel(raudialog)
        self.label_4.setObjectName("label_4")
        self.gridLayout.addWidget(self.label_4, 4, 4, 1, 1)
        self.label_3 = QtWidgets.QLabel(raudialog)
        self.label_3.setObjectName("label_3")
        self.gridLayout.addWidget(self.label_3, 5, 4, 1, 1)
        self._smodus = QtWidgets.QLineEdit(raudialog)
        self._smodus.setEnabled(False)
        self._smodus.setMaximumSize(QtCore.QSize(120, 16777215))
        self._smodus.setObjectName("_smodus")
        self.gridLayout.addWidget(self._smodus, 5, 5, 1, 1)
        self.label = QtWidgets.QLabel(raudialog)
        self.label.setObjectName("label")
        self.gridLayout.addWidget(self.label, 6, 4, 1, 1)
        self._modus = QtWidgets.QLineEdit(raudialog)
        self._modus.setEnabled(False)
        self._modus.setMaximumSize(QtCore.QSize(120, 16777215))
        self._modus.setObjectName("_modus")
        self.gridLayout.addWidget(self._modus, 6, 5, 1, 1)
        self.groupBox = QtWidgets.QGroupBox(raudialog)
        self.groupBox.setObjectName("groupBox")
        self.gridLayout_2 = QtWidgets.QGridLayout(self.groupBox)
        self.gridLayout_2.setObjectName("gridLayout_2")
        self.strickler = QtWidgets.QLineEdit(self.groupBox)
        self.strickler.setMaximumSize(QtCore.QSize(120, 16777215))
        self.strickler.setObjectName("strickler")
        self.gridLayout_2.addWidget(self.strickler, 1, 2, 1, 1)
        self.label_11 = QtWidgets.QLabel(self.groupBox)
        self.label_11.setObjectName("label_11")
        self.gridLayout_2.addWidget(self.label_11, 0, 1, 1, 1)
        self.label_2 = QtWidgets.QLabel(self.groupBox)
        self.label_2.setMaximumSize(QtCore.QSize(180, 16777215))
        self.label_2.setObjectName("label_2")
        self.gridLayout_2.addWidget(self.label_2, 1, 1, 1, 1)
        self.samplerau = QtWidgets.QTableWidget(self.groupBox)
        self.samplerau.setObjectName("samplerau")
        self.samplerau.setColumnCount(2)
        self.samplerau.setRowCount(0)
        item = QtWidgets.QTableWidgetItem()
        self.samplerau.setHorizontalHeaderItem(0, item)
        item = QtWidgets.QTableWidgetItem()
        self.samplerau.setHorizontalHeaderItem(1, item)
        self.samplerau.horizontalHeader().setStretchLastSection(True)
        self.gridLayout_2.addWidget(self.samplerau, 3, 1, 2, 2)
        self._lmodus = QtWidgets.QLineEdit(self.groupBox)
        self._lmodus.setEnabled(False)
        self._lmodus.setMaximumSize(QtCore.QSize(120, 16777215))
        self._lmodus.setObjectName("_lmodus")
        self.gridLayout_2.addWidget(self._lmodus, 0, 2, 1, 1)
        self.add = QtWidgets.QPushButton(self.groupBox)
        self.add.setObjectName("add")
        self.gridLayout_2.addWidget(self.add, 2, 2, 1, 1)
        self.del_ = QtWidgets.QPushButton(self.groupBox)
        self.del_.setObjectName("del_")
        self.gridLayout_2.addWidget(self.del_, 2, 1, 1, 1)
        self.gridLayout.addWidget(self.groupBox, 7, 4, 7, 2)

        self.retranslateUi(raudialog)
        QtCore.QMetaObject.connectSlotsByName(raudialog)

    def retranslateUi(self, raudialog):
        _translate = QtCore.QCoreApplication.translate
        raudialog.setWindowTitle(_translate("raudialog", "Create Lamellen Profiles"))
        self.label_5.setText(_translate("raudialog", "Choose Nodes:"))
        self.nodes_table.setSortingEnabled(True)
        item = self.nodes_table.horizontalHeaderItem(0)
        item.setText(_translate("raudialog", "Nodes"))
        item = self.nodes_table.horizontalHeaderItem(1)
        item.setText(_translate("raudialog", "Station"))
        item = self.nodes_table.horizontalHeaderItem(2)
        item.setText(_translate("raudialog", "ID"))
        item = self.nodes_table.horizontalHeaderItem(3)
        item.setText(_translate("raudialog", "Schnitt"))
        self.cancel.setText(_translate("raudialog", "Cancel"))
        self.ok.setText(_translate("raudialog", "Create"))
        self._schaltercheck.setText(_translate("raudialog", "Schalt Profil"))
        self._swert.setText(_translate("raudialog", "120"))
        self.label_4.setText(_translate("raudialog", "Anfangs Wert"))
        self.label_3.setText(_translate("raudialog", "Schaltprofil Modus"))
        self._smodus.setText(_translate("raudialog", "ZS"))
        self.label.setText(_translate("raudialog", "QuerschnittsProfil Modus"))
        self._modus.setText(_translate("raudialog", "RS"))
        self.groupBox.setTitle(_translate("raudialog", "Rauheitsprofil"))
        self.strickler.setText(_translate("raudialog", "35"))
        self.label_11.setText(_translate("raudialog", "Rauheitsprofil Modus"))
        self.label_2.setText(_translate("raudialog", "Anfangs Strickler Wert"))
        item = self.samplerau.horizontalHeaderItem(0)
        item.setText(_translate("raudialog", "X"))
        item = self.samplerau.horizontalHeaderItem(1)
        item.setText(_translate("raudialog", "Strickler"))
        self._lmodus.setText(_translate("raudialog", "H2"))
        self.add.setText(_translate("raudialog", "+"))
        self.del_.setText(_translate("raudialog", "-"))




if __name__ == "__main__":
    import sys
    app = QtWidgets.QApplication(sys.argv)
    raudialog = QtWidgets.QDialog()
    ui = Ui_raudialog()
    ui.setupUi(raudialog)
    raudialog.show()
    sys.exit(app.exec_())
