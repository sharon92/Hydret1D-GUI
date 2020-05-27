# -*- coding: utf-8 -*-
'''import system modules'''
import os
from modules.rawh1d   import readHQ

'''import pyqt5 modules'''
from PyQt5               import uic
from PyQt5.QtWidgets     import QDockWidget,QFileDialog
from PyQt5.QtCore        import QAbstractTableModel,QVariant,Qt

import sys
script_dir = os.getcwd()
SCRIPT_DIR = os.path.dirname(os.path.realpath(sys.argv[0]))
ui         = os.path.join(SCRIPT_DIR,'ui','outputViewer.ui')
class showOutput(QDockWidget):
    def __init__(self,myapp,filepath,F,parent=None):
        super().__init__(parent)
        uic.loadUi(ui,self)
        self.main   = myapp
        self.fpath  = filepath
        self.ext    = F
        self.browsedata.clicked.connect(self.openfile)
        self.expexcel.clicked.connect(self.export2xl)
        self.expcsv.clicked.connect(self.export2csv)
        self.initiate()
        
    def initiate(self):
        if os.path.isfile(self.fpath):
            if self.ext == 'Q':
                self.df = self.main.qz_df
            elif self.ext == 'H':
                self.df = self.main.hz_df
            self.datapath.setText(self.fpath)
            self.ftype.setCurrentIndex(['A','B','T'].index(self.main.p_format.currentText()[0]))
            self.createTable()
    
    def export2xl(self):
        if hasattr(self,'df'):
            file,ext = QFileDialog.getSaveFileName(caption='Ergebnis-Datei Speichern',filter = 'Excel File *.xlsx')
            if not file == '': 
                self.df.to_excel(file)
    
    def export2csv(self):
        if hasattr(self,'df'):
            file,ext = QFileDialog.getSaveFileName(caption='Ergebnis-Datei Speichern',filter = 'CSV *.csv')
            if not file == '': self.df.to_csv(file,sep=';')
            
    def openfile(self):
        if self.ext == 'Q':
            file,ext = QFileDialog.getOpenFileName(caption='Ergebnis-Datei Öffnen',filter = 'Ergebnis-Datei (*.QCH)')
        elif self.ext == 'H':
            file,ext = QFileDialog.getOpenFileName(caption='Ergebnis-Datei Öffnen',filter = 'Ergebnis-Datei (*.HCH)')
        form = self.ftype.currentText()[0]
        if not file == '':
            self.df = readHQ(self,file,form=form)
            self.datapath.setText(file)
            self.createTable()

    def createTable(self):
        model = PandasModel(self.df)
        self.dftable.setModel(model)

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
    
def ergebnisViewer(self,F='Q'):
    if F=='Q': 
        if hasattr(self,'qgDock'):
            self.qgDock.setVisible(True)
        else:
            filepath = self.p_qch.text()
            self.qgDock = showOutput(self,filepath,F)
            self.addDockWidget(Qt.LeftDockWidgetArea,self.qgDock)
            self.qgDock.setWindowTitle('Abfluss-Ergebnis')
        self.qgDock.setFloating(True)
        self.qgDock.setAllowedAreas(Qt.RightDockWidgetArea)
    elif F== 'H': 
        if hasattr(self,'hgDock'): 
            self.hgDock.setVisible(True)
        else:
            filepath = self.p_hch.text()
            self.hgDock = showOutput(self,filepath,F)
            self.addDockWidget(Qt.LeftDockWidgetArea,self.hgDock)
            self.hgDock.setWindowTitle('Wasserstand-Ergebnis')
        self.hgDock.setFloating(True)
        self.hgDock.setAllowedAreas(Qt.RightDockWidgetArea)