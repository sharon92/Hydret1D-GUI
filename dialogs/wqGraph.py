# -*- coding: utf-8 -*-
'''import system modules'''
import os
import pandas as pd
#from modules.rawh1d   import readHQ

'''import pyqt5 modules'''
from PyQt5               import uic
from PyQt5.QtWidgets     import QDockWidget,QFileDialog
from PyQt5.QtCore        import QAbstractTableModel,QVariant,Qt

import sys
script_dir = os.getcwd()
SCRIPT_DIR = os.path.dirname(os.path.realpath(sys.argv[0]))
ui         = os.path.join(SCRIPT_DIR,'ui','wqDock.ui')

class plotter(QDockWidget):
    def __init__(self,app,parent=None):
        super().__init__(parent)
        uic.loadUi(ui,self)
        self.app = app
#        self.addData.clicked.connect(self.openfile)
        self.wqKnoten.currentIndexChanged.connect(self.createTable)
        self.initiate()
        
    def initiate(self):
        if hasattr(self.app,'hz_df') & hasattr(self.app,'qz_df'):
            self.qz_df = self.app.qz_df
            self.hz_df = self.app.hz_df
            self.wqKnoten.blockSignals(True)
            self.wqKnoten.addItems(self.qz_df.index.astype(str))
            self.wqKnoten.blockSignals(False)
            self.createTable()
            
    def openfile(self):
        file,ext = QFileDialog.getOpenFileName(caption='Pegel-Daten Öffnen',filter = '*.xlsx;;*.csv;;*.dat')
        if not file == '':
            self.createTable()

    def createTable(self):
        w = self.hz_df.loc[int(self.wqKnoten.currentText())]
        q = self.qz_df.loc[int(self.wqKnoten.currentText())]
        self.df = pd.DataFrame([q,w]).transpose()
        self.df.columns = ['Abfluss','Wasserstand']
        model = PandasModel(self.df)
        self.wqTable.setModel(model)
        self.plot()
    
    def plot(self):
        self.wqPlot.clear()
        self.wqPlot.plot(self.df['Abfluss'],self.df['Wasserstand'],
                         pen=None,symbol='o',symbolBrush='b')

class PandasModel(QAbstractTableModel):
    def __init__(self, data, parent=None):
        QAbstractTableModel.__init__(self, parent)
        self._data = data

    def rowCount(self, parent=None):
        return len(self._data.values)

    def columnCount(self, parent=None):
        return self._data.columns.size

    def data(self, index, role=Qt.DisplayRole):
        if index.isValid():
            if role == Qt.DisplayRole:
                return QVariant(str(
                    self._data.values[index.row()][index.column()]))
        return QVariant()
    
    def headerData(self, rowcol, orientation, role):
        if role != Qt.DisplayRole:
            return QVariant()
        if orientation == Qt.Horizontal:
            try:
                return self._data.columns.tolist()[rowcol]
            except (IndexError, ):
                return QVariant()
        elif orientation == Qt.Vertical:
            try:
                # return self.df.index.tolist()
                return self._data.index.tolist()[rowcol]
            except (IndexError, ):
                return QVariant()
        return None
    
def wqDocker(self):
    if hasattr(self,'wqDock'):
        self.wqDock.setVisible(True)
        self.wqDock.initiate()
    else:
        self.wqDock = plotter(self)
        self.addDockWidget(Qt.LeftDockWidgetArea,self.wqDock)
    self.wqDock.setFloating(True)
    self.wqDock.setAllowedAreas(Qt.RightDockWidgetArea)
