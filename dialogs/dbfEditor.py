# -*- coding: utf-8 -*-
'''import system modules'''
import os
import pandas    as pd
import shapefile as shp

'''import pyqt5 modules'''
from PyQt5                 import uic
from PyQt5.QtWidgets       import QDockWidget,QFileDialog,QTableWidgetItem,QHeaderView
from PyQt5.QtCore          import Qt

import sys
script_dir = os.getcwd()
SCRIPT_DIR = os.path.dirname(os.path.realpath(sys.argv[0]))
ui         = os.path.join(SCRIPT_DIR,'ui','dbfEditor.ui')

class showOutput(QDockWidget):
    def __init__(self,myapp,filepath,parent=None):
        super().__init__(parent)
        uic.loadUi(ui,self)
        self.main   = myapp
        self.fpath  = filepath
        self.browsedata.clicked.connect(self.openfile)
        self.savefile.clicked.connect(self.saveit)
        self.dftable.itemChanged.connect(self.updateTable)
        self.initiate()
        
    def initiate(self):
        if os.path.isfile(self.fpath):
            self.datapath.setText(self.fpath)
            self.loadfile()
            self.createTable()
            
    def openfile(self):
        file,ext = QFileDialog.getOpenFileName(caption='Gewässerachse Auswählen',filter='Database (*.shp *.dbf)')

        if not file == '':
            self.datapath.setText(file)
            self.fpath = file
            self.loadfile()
            self.createTable()
    
    def saveit(self):
        file,ext = QFileDialog.getSaveFileName(caption='Gewässerachse Speichern',filter='Database (*.shp *.dbf)')
        if not file == '':
            w           = shp.Writer(file)
            w.shapeType = self.r.shapeType
            w.fields    = self.r.fields[1:]

            for shaperec in range(len(self.df)):
                w.shape(self.ats[shaperec])
                vals = self.df.loc[shaperec].values.tolist()
                w.record(*vals)
            w.close()
            
    def loadfile(self):
        self.r = shp.Reader(self.fpath)
        self.r.encodingErrors = 'ignore'
        self.cols = [c[0] for c in self.r.fields[1:]]
        atr,self.ats = {},{}
        for n,i in enumerate(self.r.shapeRecords()):
            atr[n]      = i.record.as_dict()
            self.ats[n] = i.shape
        self.df = pd.DataFrame(atr).transpose()
        self.df = self.df[self.cols]
    
    def createTable(self):
        self.dftable.blockSignals(True)
        self.dftable.clear()
        self.dftable.setColumnCount(len(self.df.columns))
        self.dftable.setHorizontalHeaderLabels((self.df.columns).astype(str))
        self.dftable.setRowCount(len(self.df.index))
        self.dftable.setVerticalHeaderLabels((self.df.index).astype(str))
        
        df_array = self.df.values
        for r,idx in enumerate(self.df.index):
            for c,col in enumerate(self.df.columns):
                self.dftable.setItem(r,c,QTableWidgetItem(str(df_array[r,c])))
        self.dftable.horizontalHeader().setSectionResizeMode(QHeaderView.ResizeToContents)
        self.dftable.blockSignals(False)

    def updateTable(self,item):
        row,column,text = item.row(),item.column(),item.text()
        
        if type(self.df.iat[row,column]) is str:
            self.df.iat[row,column] = text
        else:
            self.df.iat[row,column] = eval(text)
        
def shpViewer(self):
    if hasattr(self,'dbfDock'):
        self.dbfDock.setVisible(True)
    else:
        filepath = self.p_achse.text()
        self.dbfDock = showOutput(self,filepath)
        self.addDockWidget(Qt.LeftDockWidgetArea,self.dbfDock)
    self.dbfDock.setFloating(True)
    self.dbfDock.setAllowedAreas(Qt.RightDockWidgetArea)