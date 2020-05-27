# -*- coding: utf-8 -*-
'''import system modules'''
import os
from modules.rawh1d      import HYDRET,writeStart

'''import pyqt5 modules'''
from PyQt5               import uic
from PyQt5.QtWidgets     import (QDockWidget,QFileDialog,
                                 QTableWidgetItem,
                                 QHeaderView)
from PyQt5.QtCore        import Qt

import sys
script_dir = os.getcwd()
SCRIPT_DIR = os.path.dirname(os.path.realpath(sys.argv[0]))
ui         = os.path.join(SCRIPT_DIR,'ui','startEditor.ui')

class nodeTable(QDockWidget):

    def __init__(self, myapp,parent=None):
        super().__init__(parent)
        uic.loadUi(ui,self)
        self.main = myapp
        self.initiate()
        self.changes = 0
        self.browse_start.clicked.connect(self.openfile)
        self.savefile.clicked.connect(self.savestart)
        self.expcsv.clicked.connect(self.exp2excel)
        self.undo.clicked.connect(self._undo)
        self.redo.clicked.connect(self._redo)
        self.sheet.itemChanged.connect(self.appenddf)

    def initiate(self):
        if not self.main.p_startdat.text() == '':
            if os.path.isfile(self.main.p_startdat.text()):
                self.df_path = os.path.abspath(self.main.p_startdat.text())
                self.loadfile()
    
    def openfile(self):
        file,ext = QFileDialog.getOpenFileName(caption='Start-Datei Öffnen',filter = 'Start-Datei (*.dat)')

        if not file == '':
            self.df_path = file
            self.loadfile()
    
    def savestart(self):
        file,ext = QFileDialog.getSaveFileName(caption='Start-Datei Speichern',filter = 'Start-Datei (*.dat)')

        if not file == '':
            writeStart(file,self.df,self._dform)
    
    def exp2excel(self):
        file,ext = QFileDialog.getSaveFileName(caption='Datei als Excel Speichern',filter = 'Excel-Datei (*.xlsx)')

        if not file == '':
            self.df.to_excel(file)
            
    def loadfile(self):
        h = HYDRET()
        self.df = h.readSTART(self.df_path)
        self._dtype = h._dtype
        self._dform = h._dform
        self.df.reset_index(inplace=True)
        self.db = [self.df]
        self.startpath.setText(self.df_path)
        self.setWindowTitle(os.path.basename(self.df_path))
        if not self.df is None: self.writetab()
        
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
        
    def readtab(self,i):
        row,column = i.row(),i.column()
        
        df = self.df.copy()
        if df.iat[row,column] is str:
            df.iat[row,column] = i.text()
        else:
            df.iat[row,column] = eval(i.text())
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
        df_array = df.values
        for i in range(self.sheet.rowCount()):
            for c,ci in enumerate(df.columns):
                if not float(df_array[i,c]) == -1e6:
                    self.sheet.setItem(i,c,QTableWidgetItem(str(df_array[i,c])))
                else:
                    self.sheet.setItem(i,c,QTableWidgetItem(''))
        self.sheet.horizontalHeader().setSectionResizeMode(QHeaderView.ResizeToContents)
        self.sheet.blockSignals(False)
        
    def appenddf(self,i):
        self.changes +=1
        self.undo.setEnabled(True)
        self.redo.setEnabled(False)
        self.readtab(i)
        self.db = self.db[:self.changes+1]

def spreadwin(self):
    Dock = nodeTable(self)
    self.addDockWidget(Qt.LeftDockWidgetArea,Dock)
    Dock.setFloating(True)
    Dock.setAllowedAreas(Qt.RightDockWidgetArea)
    if self.p_startdat.text() ==  '':win = 'Worksheet'
    else: win = self.p_startdat.text()
    Dock.setWindowTitle(win)