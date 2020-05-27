# -*- coding: utf-8 -*-
'''import system modules'''
import os

'''import pyqt5 modules'''
from PyQt5               import uic
from PyQt5.QtWidgets     import QDialog,QFileDialog

import sys
script_dir = os.getcwd()
SCRIPT_DIR = os.path.dirname(os.path.realpath(sys.argv[0]))
ui         = os.path.join(SCRIPT_DIR,'ui','textViewer.ui')

class showOutput(QDialog):
    def __init__(self,myapp,filepath,F,parent=None):
        super().__init__(parent)
        uic.loadUi(ui,self)
        self.main   = myapp
        self.fpath  = filepath
        self.ext    = F
        self.browsedata.clicked.connect(self.openfile)
        self.close.clicked.connect(self.reject)
        self.initiate()
        
    def initiate(self):
        if os.path.isfile(self.fpath):
            self.datapath.setText(self.fpath)
            self.writeText()
            
    def openfile(self):
        if   self.ext== 'RUN': fil = 'Run-Datei (*.run)'
        elif self.ext== 'HYD': fil = 'Hyd-Datei (*.hyd)'
        elif self.ext== 'PRO': fil = 'Pro-Datei (*.pro*)'
        elif self.ext== 'OUT': fil = 'Out-Datei (*.out)'
        file,ext = QFileDialog.getOpenFileName(caption='Ergebnis-Datei Öffnen',filter = fil)
        self.datapath.setText(file)
        self.fpath = file
        if not file == '':
            self.writeText()

    def writeText(self):
        with open(self.fpath,'r') as f:
            nl = f.readlines()
        for i in nl:
            self.textEdit.insertPlainText(i)
        

def textEditor(self,F='Q'):
    if   F== 'RUN': filepath = self.p_run.text()
    elif F== 'HYD': filepath = self.p_hyd.text()
    elif F== 'PRO': filepath = self.p_prodat.text()
    elif F== 'OUT': filepath = self.p_aus.text()
    Popup = showOutput(self,filepath,F)
    if Popup.exec_(): pass