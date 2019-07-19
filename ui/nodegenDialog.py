# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'nodegenDialog.ui'
#
# Created by: PyQt5 UI code generator 5.12.1
#
# WARNING! All changes made in this file will be lost!

from PyQt5 import QtCore, QtGui, QtWidgets


class Ui_nodegenDialog(object):
    def setupUi(self, nodegenDialog):
        nodegenDialog.setObjectName("nodegenDialog")
        nodegenDialog.resize(707, 584)
        font = QtGui.QFont()
        font.setFamily("Segoe UI")
        nodegenDialog.setFont(font)
        self.verticalLayout = QtWidgets.QVBoxLayout(nodegenDialog)
        self.verticalLayout.setObjectName("verticalLayout")
        self.label_5 = QtWidgets.QLabel(nodegenDialog)
        font = QtGui.QFont()
        font.setFamily("Segoe UI")
        font.setBold(True)
        font.setWeight(75)
        self.label_5.setFont(font)
        self.label_5.setObjectName("label_5")
        self.verticalLayout.addWidget(self.label_5)
        self.groupBox_2 = QtWidgets.QGroupBox(nodegenDialog)
        self.groupBox_2.setTitle("")
        self.groupBox_2.setObjectName("groupBox_2")
        self.gridLayout_3 = QtWidgets.QGridLayout(self.groupBox_2)
        self.gridLayout_3.setObjectName("gridLayout_3")
        self.label = QtWidgets.QLabel(self.groupBox_2)
        self.label.setObjectName("label")
        self.gridLayout_3.addWidget(self.label, 0, 0, 1, 1)
        self.label_2 = QtWidgets.QLabel(self.groupBox_2)
        self.label_2.setObjectName("label_2")
        self.gridLayout_3.addWidget(self.label_2, 0, 3, 1, 1)
        self.schnitt_show = QtWidgets.QLineEdit(self.groupBox_2)
        self.schnitt_show.setEnabled(False)
        self.schnitt_show.setMinimumSize(QtCore.QSize(150, 0))
        self.schnitt_show.setObjectName("schnitt_show")
        self.gridLayout_3.addWidget(self.schnitt_show, 1, 1, 1, 1)
        self.id = QtWidgets.QLineEdit(self.groupBox_2)
        self.id.setEnabled(False)
        self.id.setMaximumSize(QtCore.QSize(100, 16777215))
        self.id.setObjectName("id")
        self.gridLayout_3.addWidget(self.id, 1, 4, 1, 1)
        self.node_show = QtWidgets.QLineEdit(self.groupBox_2)
        self.node_show.setEnabled(False)
        self.node_show.setMaximumSize(QtCore.QSize(100, 16777215))
        self.node_show.setObjectName("node_show")
        self.gridLayout_3.addWidget(self.node_show, 0, 4, 1, 1)
        self.label_11 = QtWidgets.QLabel(self.groupBox_2)
        self.label_11.setObjectName("label_11")
        self.gridLayout_3.addWidget(self.label_11, 1, 3, 1, 1)
        self.label_3 = QtWidgets.QLabel(self.groupBox_2)
        self.label_3.setObjectName("label_3")
        self.gridLayout_3.addWidget(self.label_3, 1, 0, 1, 1)
        self.achse_show = QtWidgets.QLineEdit(self.groupBox_2)
        self.achse_show.setEnabled(False)
        self.achse_show.setObjectName("achse_show")
        self.gridLayout_3.addWidget(self.achse_show, 2, 1, 1, 4)
        self.label_10 = QtWidgets.QLabel(self.groupBox_2)
        self.label_10.setObjectName("label_10")
        self.gridLayout_3.addWidget(self.label_10, 2, 0, 1, 1)
        self.station_show = QtWidgets.QComboBox(self.groupBox_2)
        self.station_show.setMinimumSize(QtCore.QSize(100, 0))
        self.station_show.setObjectName("station_show")
        self.gridLayout_3.addWidget(self.station_show, 0, 1, 1, 1)
        spacerItem = QtWidgets.QSpacerItem(40, 20, QtWidgets.QSizePolicy.Expanding, QtWidgets.QSizePolicy.Minimum)
        self.gridLayout_3.addItem(spacerItem, 0, 2, 1, 1)
        self.verticalLayout.addWidget(self.groupBox_2)
        self.label_6 = QtWidgets.QLabel(nodegenDialog)
        font = QtGui.QFont()
        font.setFamily("Segoe UI")
        font.setBold(True)
        font.setWeight(75)
        self.label_6.setFont(font)
        self.label_6.setObjectName("label_6")
        self.verticalLayout.addWidget(self.label_6)
        self.groupBox_3 = QtWidgets.QGroupBox(nodegenDialog)
        self.groupBox_3.setTitle("")
        self.groupBox_3.setObjectName("groupBox_3")
        self.gridLayout_4 = QtWidgets.QGridLayout(self.groupBox_3)
        self.gridLayout_4.setObjectName("gridLayout_4")
        self.label_9 = QtWidgets.QLabel(self.groupBox_3)
        self.label_9.setObjectName("label_9")
        self.gridLayout_4.addWidget(self.label_9, 1, 3, 1, 1)
        self.start_station = QtWidgets.QLineEdit(self.groupBox_3)
        self.start_station.setObjectName("start_station")
        self.gridLayout_4.addWidget(self.start_station, 1, 1, 1, 1)
        self.gef = QtWidgets.QDoubleSpinBox(self.groupBox_3)
        self.gef.setDecimals(6)
        self.gef.setMinimum(-99.0)
        self.gef.setObjectName("gef")
        self.gridLayout_4.addWidget(self.gef, 2, 4, 1, 1)
        self.label_4 = QtWidgets.QLabel(self.groupBox_3)
        self.label_4.setObjectName("label_4")
        self.gridLayout_4.addWidget(self.label_4, 2, 3, 1, 1)
        self.label_7 = QtWidgets.QLabel(self.groupBox_3)
        self.label_7.setObjectName("label_7")
        self.gridLayout_4.addWidget(self.label_7, 1, 0, 1, 1)
        self.end_station = QtWidgets.QLineEdit(self.groupBox_3)
        self.end_station.setObjectName("end_station")
        self.gridLayout_4.addWidget(self.end_station, 2, 1, 1, 1)
        self.count = QtWidgets.QSpinBox(self.groupBox_3)
        self.count.setObjectName("count")
        self.gridLayout_4.addWidget(self.count, 1, 4, 1, 1)
        self.deletenodes = QtWidgets.QCheckBox(self.groupBox_3)
        self.deletenodes.setChecked(True)
        self.deletenodes.setObjectName("deletenodes")
        self.gridLayout_4.addWidget(self.deletenodes, 0, 1, 1, 1)
        self.label_8 = QtWidgets.QLabel(self.groupBox_3)
        self.label_8.setObjectName("label_8")
        self.gridLayout_4.addWidget(self.label_8, 2, 0, 1, 1)
        spacerItem1 = QtWidgets.QSpacerItem(40, 20, QtWidgets.QSizePolicy.Expanding, QtWidgets.QSizePolicy.Minimum)
        self.gridLayout_4.addItem(spacerItem1, 1, 2, 1, 1)
        self.verticalLayout.addWidget(self.groupBox_3)
        self.edit_name = QtWidgets.QTableWidget(nodegenDialog)
        self.edit_name.setObjectName("edit_name")
        self.edit_name.setColumnCount(4)
        self.edit_name.setRowCount(0)
        item = QtWidgets.QTableWidgetItem()
        self.edit_name.setHorizontalHeaderItem(0, item)
        item = QtWidgets.QTableWidgetItem()
        self.edit_name.setHorizontalHeaderItem(1, item)
        item = QtWidgets.QTableWidgetItem()
        self.edit_name.setHorizontalHeaderItem(2, item)
        item = QtWidgets.QTableWidgetItem()
        self.edit_name.setHorizontalHeaderItem(3, item)
        self.verticalLayout.addWidget(self.edit_name)
        self.groupBox = QtWidgets.QGroupBox(nodegenDialog)
        self.groupBox.setTitle("")
        self.groupBox.setObjectName("groupBox")
        self.gridLayout_2 = QtWidgets.QGridLayout(self.groupBox)
        self.gridLayout_2.setObjectName("gridLayout_2")
        self.ok = QtWidgets.QPushButton(self.groupBox)
        self.ok.setMinimumSize(QtCore.QSize(100, 0))
        self.ok.setMaximumSize(QtCore.QSize(100, 16777215))
        self.ok.setAutoDefault(False)
        self.ok.setObjectName("ok")
        self.gridLayout_2.addWidget(self.ok, 0, 4, 1, 1)
        self.cancel = QtWidgets.QPushButton(self.groupBox)
        self.cancel.setMinimumSize(QtCore.QSize(100, 0))
        self.cancel.setMaximumSize(QtCore.QSize(100, 16777215))
        self.cancel.setAutoDefault(False)
        self.cancel.setObjectName("cancel")
        self.gridLayout_2.addWidget(self.cancel, 0, 5, 1, 1)
        self.dataname = QtWidgets.QLineEdit(self.groupBox)
        self.dataname.setObjectName("dataname")
        self.gridLayout_2.addWidget(self.dataname, 0, 1, 1, 1)
        self.label_12 = QtWidgets.QLabel(self.groupBox)
        self.label_12.setObjectName("label_12")
        self.gridLayout_2.addWidget(self.label_12, 0, 0, 1, 1)
        self.overwrite = QtWidgets.QCheckBox(self.groupBox)
        self.overwrite.setObjectName("overwrite")
        self.gridLayout_2.addWidget(self.overwrite, 0, 2, 1, 1)
        spacerItem2 = QtWidgets.QSpacerItem(40, 20, QtWidgets.QSizePolicy.Expanding, QtWidgets.QSizePolicy.Minimum)
        self.gridLayout_2.addItem(spacerItem2, 0, 3, 1, 1)
        self.verticalLayout.addWidget(self.groupBox)

        self.retranslateUi(nodegenDialog)
        QtCore.QMetaObject.connectSlotsByName(nodegenDialog)

    def retranslateUi(self, nodegenDialog):
        _translate = QtCore.QCoreApplication.translate
        nodegenDialog.setWindowTitle(_translate("nodegenDialog", "Erzeugung von Zusatzquerprofilen"))
        self.label_5.setText(_translate("nodegenDialog", "Muster Querschnitt auswählen:"))
        self.label.setText(_translate("nodegenDialog", "Station"))
        self.label_2.setText(_translate("nodegenDialog", "Knoten"))
        self.label_11.setText(_translate("nodegenDialog", "Gew ID"))
        self.label_3.setText(_translate("nodegenDialog", "Schnitt"))
        self.label_10.setText(_translate("nodegenDialog", "Achse"))
        self.label_6.setText(_translate("nodegenDialog", "Define:"))
        self.label_9.setText(_translate("nodegenDialog", "Schnitt Count"))
        self.label_4.setText(_translate("nodegenDialog", "Gefälle"))
        self.label_7.setText(_translate("nodegenDialog", "Start Station"))
        self.deletenodes.setText(_translate("nodegenDialog", "Zwischen liegenden Knoten Löschen"))
        self.label_8.setText(_translate("nodegenDialog", "End Station"))
        item = self.edit_name.horizontalHeaderItem(0)
        item.setText(_translate("nodegenDialog", "Schnitt"))
        item = self.edit_name.horizontalHeaderItem(1)
        item.setText(_translate("nodegenDialog", "Knoten"))
        item = self.edit_name.horizontalHeaderItem(2)
        item.setText(_translate("nodegenDialog", "Station"))
        item = self.edit_name.horizontalHeaderItem(3)
        item.setText(_translate("nodegenDialog", "Abstand"))
        self.ok.setText(_translate("nodegenDialog", "Update"))
        self.cancel.setText(_translate("nodegenDialog", "Cancel"))
        self.label_12.setText(_translate("nodegenDialog", "Neu Filename:"))
        self.overwrite.setText(_translate("nodegenDialog", "Overwrite Files"))




if __name__ == "__main__":
    import sys
    app = QtWidgets.QApplication(sys.argv)
    nodegenDialog = QtWidgets.QDialog()
    ui = Ui_nodegenDialog()
    ui.setupUi(nodegenDialog)
    nodegenDialog.show()
    sys.exit(app.exec_())
