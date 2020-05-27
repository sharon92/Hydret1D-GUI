# -*- coding: utf-8 -*-
'''import system modules'''
import os
import shutil

from modules.rawh1d      import HYDRET as h1d

'''import pyqt5 modules'''
from PyQt5               import uic
from PyQt5.QtWidgets     import QDialog
from PyQt5.QtGui         import QMovie
from PyQt5.QtCore        import (pyqtSignal,
                                 QProcess,
                                 QByteArray)

import sys
script_dir = os.getcwd()
SCRIPT_DIR = os.path.dirname(os.path.realpath(sys.argv[0]))
ui         = os.path.join(SCRIPT_DIR,'ui','runDialog.ui')

class runHydret(QDialog):
    errorSignal  = pyqtSignal(str) 
    outputSignal = pyqtSignal(str)
    def __init__(self,myapp, parent=None):
        super().__init__(parent)
        uic.loadUi(ui,self)
        self.output = None
        self.error  = None
        self.main   = myapp
        self.overwrite.stateChanged.connect(self.ovw)
        self.runbut.clicked.connect(self.start)
        self.ok.clicked.connect(self.accept)
        self.cancel.clicked.connect(self.reject)
        self.process = QProcess()
        self.process.readyReadStandardError.connect(self.onReadyReadStandardError)
        self.process.readyReadStandardOutput.connect(self.onReadyReadStandardOutput)
        self.process.finished.connect(self.finishstatement)
        self.s_gif = QMovie(os.path.join(script_dir,'icons','success.gif'),QByteArray(),self)
        self.s_gif.setCacheMode(QMovie.CacheAll) 
        self.s_gif.setSpeed(100) 
        self.e_gif = QMovie(os.path.join(script_dir,'icons','error.gif'),QByteArray(),self)
        self.e_gif.setCacheMode(QMovie.CacheAll) 
        self.e_gif.setSpeed(100) 
        self.w_gif = QMovie(os.path.join(script_dir,'icons','waiting.gif'),QByteArray(),self)
        self.w_gif.setCacheMode(QMovie.CacheAll) 
        self.w_gif.setSpeed(100) 
        self.initiate()
        
    def initiate(self):
        counter  = 0
        while True:
            bak_name = 'BAK_'+self.main.h1drun.upper().replace('.RUN','_')+str(counter)
            if not os.path.isdir(bak_name):
                break
            counter +=1
        self.dataname.setText(bak_name)
        self.p_name.setText(self.main.h1denv)
        
    def ovw(self):
        if self.overwrite.checkState() == 2:
            self.dataname.setEnabled(True)
        else:
            self.dataname.setEnabled(False)
            
    def start(self):
        self.runbut.setEnabled(False)
        self.ok.setEnabled(False)
        self.cancel.setEnabled(False)
        self.movie.setMovie(self.w_gif)
        self.w_gif.start()
        os.chdir(self.main.h1denv)
        if self.dataname.isEnabled():
            src = self.main.h1denv
            dst = os.path.join(src,self.dataname.text())
            if not os.path.exists(dst):
                os.mkdir(dst)
            [shutil.copy2(os.path.abspath(f),os.path.join(dst,f)) for f in os.listdir(src) if os.path.isfile(f)]
            
        self.main.statusbar.showMessage('Saving Modell...')
        self.main.saveProject()
        self.main.statusbar.showMessage('Hydret06 started...')
        self.process.start(self.main.h1drun[:-4]+'.bat')
        self.main.statusbar.showMessage('Ready')
        
    def onReadyReadStandardError(self):
        error = self.process.readAllStandardError().data().decode(errors='ignore')
        self.plainTextEdit.appendPlainText(error)
        self.errorSignal.emit(error)

    def onReadyReadStandardOutput(self):
        result = self.process.readAllStandardOutput().data().decode(errors='ignore')
        self.plainTextEdit.appendPlainText(result)
        self.outputSignal.emit(result)
        
    def finishstatement(self,i):
        self.runbut.setEnabled(True)
        self.ok.setEnabled(True)
        self.cancel.setEnabled(True)
        if not "STAND = 100 % VON 100 %" in self.plainTextEdit.toPlainText():
            self.movie.setMovie(self.e_gif)
            self.e_gif.start()
            statement = '*'*60+'\nModell Fehler!!!\nBitte Prüfen!\n'+'*'*60
            
        elif i == 0:
            self.movie.setMovie(self.s_gif)
            self.s_gif.start()
            statement = '*'*60+'\nSimulation erfolgreich abgeschlossen!!\n'+'*'*60
        self.plainTextEdit.appendPlainText(statement)

def runModel(self):
    Popup = runHydret(self)
    if Popup.exec_():
        self.statusbar.showMessage('Reloading Model...')
        n_h1d = h1d(hydret_path = self.h1drunpath)
        self.initiate(hyd = n_h1d,i=self.knotenNr.currentIndex())
        self.statusbar.showMessage('Ready')