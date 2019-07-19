# -*- coding: utf-8 -*-
'''import system modules'''
import os
from collections import OrderedDict
import pandas as pd

'''import pyqt5 modules'''
from PyQt5.QtWidgets     import (QDialog,
                                 QTableWidgetItem,
                                 QHeaderView)
from ui.tableDialog      import Ui_tableDialog

script_dir = os.getcwd()

class nodeTable(QDialog,Ui_tableDialog):

    def __init__(self, myapp,parent=None):
        super().__init__(parent)
        self.setupUi(self)
        self.main = myapp
        self.initiate()
        self.changes = 0
        self.undo.clicked.connect(self._undo)
        self.redo.clicked.connect(self._redo)
        self.remove.clicked.connect(self.update_m)
        self.add.clicked.connect(self.update_a)
        self.ok.clicked.connect(self.accept)
        self.cancel.clicked.connect(self.reject)
        self.sheet.itemChanged.connect(self.appenddf)
    
    def _undo(self):
        self.changes -=1
        if self.changes == 0:
            self.undo.setEnabled(False)
            
        self.df =self.db[self.changes].copy()
        self.writetab()
        self.redo.setEnabled(True)
            
    def _redo(self):
        self.changes +=1
        self.df = self.db[self.changes].copy()
        self.writetab()
        self.undo.setEnabled(True)
        if len(self.db) == self.changes+1:
            self.redo.setEnabled(False)
        
    def readtab(self):
        data = []
        for c in range(self.sheet.columnCount()):
            coldat = []
            for r in range(self.sheet.rowCount()):
                if self.sheet.item(r,c) is None or '':
                    coldat.append(-1e6)
                else:
                    if '.' in self.sheet.item(r,c).text():
                        coldat.append(float(self.sheet.item(r,c).text()))
                    else:
                        coldat.append(int(self.sheet.item(r,c).text()))
            data.append(coldat)
        _data = OrderedDict(zip(self.df.columns,data))
        df = pd.DataFrame(_data)
        df = df.astype(self._dtype)
        self.db.append(df)

    def writetab(self):
        self.sheet.blockSignals(True)
        self.sheet.clear()
        df = self.df.copy()
        for key,item in self._dform.items():
            df[key] = df[key].apply(func = item.format,axis=1)
        self.sheet.setColumnCount(len(df.columns))
        self.sheet.setHorizontalHeaderLabels((df.columns))
        self.sheet.setRowCount(len(df.index))
        for i in range(self.sheet.rowCount()):
            for c,ci in enumerate(df.columns):
                if not float(df[ci].loc[i]) == -1e6:
                    self.sheet.setItem(i,c,QTableWidgetItem(str(df[ci].loc[i])))
                else:
                    self.sheet.setItem(i,c,QTableWidgetItem(''))
        self.sheet.horizontalHeader().setSectionResizeMode(QHeaderView.ResizeToContents)
        self.sheet.blockSignals(False)
        
    def appenddf(self):
        self.changes +=1
        self.undo.setEnabled(True)
        self.redo.setEnabled(False)
        self.readtab()
        self.db = self.db[:self.changes+1]
        
    def update_m(self):
        self.changes +=1
        self.sheet.removeRow(self.sheet.currentRow())
        self.readtab()
        self.undo.setEnabled(True)
        self.db = self.db[:self.changes+1]

    def update_a(self):
        self.changes +=1
        self.sheet.insertRow(self.sheet.currentRow())
        self.readtab()
        self.undo.setEnabled(True)
        self.db = self.db[:self.changes+1]
        
    def initiate(self):
        self.df     = self.main.df_start.copy()
        self._dtype = self.main.h1d._dtype
        self._dform = self.main.h1d._dform
        self.df.reset_index(inplace=True)
        self.db = [self.df]
        self.startpath.setText(self.main.h1d.startpath)
        self.writetab()
    
def spreadwin(self):
    Popup = nodeTable(self)
    if Popup.exec_():
        self.weird = Popup.db