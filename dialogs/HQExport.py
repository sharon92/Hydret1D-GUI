# -*- coding: utf-8 -*-
'''import system modules'''
import os
import pandas as pd
from   functools         import partial
'''import pyqt5 modules'''
from PyQt5               import uic
from PyQt5.QtWidgets     import (QDialog,
                                 QCheckBox,
                                 QTableWidgetItem,
                                 QFileDialog,
                                 QMessageBox)

import sys
import copy
script_dir = os.getcwd()
SCRIPT_DIR = os.path.dirname(os.path.realpath(sys.argv[0]))
ui         = os.path.join(SCRIPT_DIR,'ui','HQExport.ui')

class HQExport(QDialog):

    def __init__(self, myapp,parent=None):
        super().__init__(parent)
        uic.loadUi(ui,self)
        self.main = myapp
        self.hz = copy.deepcopy(self.main.hz_df.transpose())
        self.qz = copy.deepcopy(self.main.qz_df.transpose())
        self.all.stateChanged.connect(self.toggleAllKnoten)
        self.single.clicked.connect(self.exportSingle)
        self.many.clicked.connect(self.exportMany)
        self.initiate()
    
    def initiate(self):
        self.hch.setText(self.main.h1d.hch_f)
        self.qch.setText(self.main.h1d.qch_f)
        self.knoten.setColumnCount(1)
        self.knoten.setRowCount(len(self.hz.columns))
        for i,c in enumerate(self.hz.columns):
            it = QCheckBox(str(c),self.knoten)
            it.setCheckState(2)
            self.knoten.setCellWidget(i,0,it)
            it.stateChanged.connect(partial(self.knotenState,i,c))
            
    def toggleAllKnoten(self):
        switch=2 if self.all.checkState()==2 else 0
        for i in range(self.knoten.rowCount()):
            it = self.knoten.cellWidget(i,0)
            it.blockSignals(True)
            it.setCheckState(switch)
            it.blockSignals(False)
        if switch == 2:
            self.hz = copy.deepcopy(self.main.hz_df.transpose())
            self.qz = copy.deepcopy(self.main.qz_df.transpose())
        else:
            self.hz = pd.DataFrame()
            self.qz = pd.DataFrame()
        
    def knotenState(self,i,c):
        it = self.knoten.cellWidget(i,0)
        if it.checkState()==2:
            self.hz.insert(i,column=c,value=self.main.hz_df.transpose()[c])
            self.qz.insert(i,column=c,value=self.main.qz_df.transpose()[c])
        else:
            self.hz.drop(columns=c,inplace=True)
            self.qz.drop(columns=c,inplace=True)
        
    
    def exportSingle(self):
        file,ext = QFileDialog.getSaveFileName(caption='H-Q Tabelle',filter='*.csv')
        if file=='': return
        self.hz.join(self.qz,lsuffix='_h',rsuffix='_q').to_csv(file,sep=';',decimal=',')
    
    def exportMany(self):
        drc = QFileDialog.getExistingDirectory(caption='Ordner Auswählen')
        if drc=='': return
        for i in self.hz.columns:
            (pd.DataFrame([self.hz[i],self.qz[i]],index=['WSP','Q'])
             .transpose()
             .to_excel(os.path.join(drc,'WQ_'+str(i)+'.csv'),sep=';',decimal=','))

def HQPopup(self):
    if not (hasattr(self,'hz_df') & hasattr(self,'qz_df')):
        self.statusbar.showMessage('Ergebnis Dateien QCH und HCH sind nicht geladen!')
        return
    
    Popup = HQExport(self)
    if Popup.exec_():
        pass


