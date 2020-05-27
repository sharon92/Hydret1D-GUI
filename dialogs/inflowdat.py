# -*- coding: utf-8 -*-
'''import system modules'''
import os
import numpy as np
'''import pyqt5 modules'''
from PyQt5               import uic
from PyQt5.QtWidgets     import (QDialog,
                                 QTableWidgetItem,
                                 QFileDialog,
                                 QMessageBox)

import sys
script_dir = os.getcwd()
SCRIPT_DIR = os.path.dirname(os.path.realpath(sys.argv[0]))
ui         = os.path.join(SCRIPT_DIR,'ui','inflowdat.ui')

class inflowTable(QDialog):

    def __init__(self, myapp,dat,num,idx,parent=None):
        super().__init__(parent)
        uic.loadUi(ui,self)
        self.main = myapp
        self.dat = dat
        self.num = num
        self.idx = idx
        self.einpfad = os.path.join(self.main.h1denv,self.dat)
        self.eindat.setText(self.dat)
        self.einbrowse.clicked.connect(self.browsedat)
        self.qfak.stateChanged.connect(self.plot)
        self.buttonBox.accepted.connect(self.accept)
        self.buttonBox.rejected.connect(self.reject)
        self.einqt.itemChanged.connect(self.qtable)
        self.eincount.valueChanged.connect(self.rowinc)
        self.initiate()
    
    def rowinc(self,i):
        self.einqt.setRowCount(i)
        
    def qtable(self):
        self.qwval = np.array([float(self.einqt.item(i,1).text()) for i in range(self.einqt.rowCount())])
        self.qwfak = self.qwval*self.fakval.value()
        self.plot()
        
    def browsedat(self):
        name = QFileDialog.getOpenFileName(caption='Zufluss Datei Öffnen')[0]
        if name!= '':
            self.eindat.setText(os.path.basename(name))
            self.dat = os.path.basename(name)
            self.einpfad = name
            self.initiate()
        
    def plot(self):
        
        if self.qfak.isChecked():
            q = self.qwval*self.fakval.value()
        else:
            q = self.qwval
        
        self.einqt.blockSignals(True)
        self.einqt.setRowCount(len(q))
        for n,i in enumerate(q):
            self.einqt.setItem(n,0,QTableWidgetItem(str(self.xval[n])))
            self.einqt.setItem(n,1,QTableWidgetItem(str(i)))
            
        self.graphicsView.clear()
        self.graphicsView.plot(self.xval,q,pen='b')
        self.einqt.blockSignals(False)
        
    def initiate(self):
        
        with open(self.einpfad,'r') as ein:
            lines = ein.readlines()
        self.eintext.setText(lines[0])
        self.eincount.setValue(int(lines[1].split()[0]))
        self.eindt.setValue(float(lines[1].split()[1]))
        self.vals = np.array([i for l in lines[2:] for i in list(map(float,l.replace(',','.').split()))])   
        if self.eincount.value()<0:
            self.xval  = self.vals[::2]
            self.qwval = self.vals[1::2]
        else:
            self.xval  = np.arange(0,self.eincount.value()*self.eindt.value(),self.eindt) 
            self.qwval = self.val.copy()[:len(self.xval)]
        if type(self.num) == tuple:
            self.fak = self.main.p_lie_table.cellWidget(self.num[1],3).value()
            self.qwfak = self.qwval*self.fak
            try:
                self.einstat.setValue(float(self.main.h1d.zdat[self.num[1]][40:50]))
            except:pass
            try:
                self.einbq.setValue(float(self.main.h1d.zdat[self.num[1]][50:60]))
            except:pass
        else:
            if self.num>=0:
                self.fak = float(self.main.p_nupe.item(self.num,3).text())
                self.qwfak = self.qwval*self.fak
                try:
                    self.einstat.setValue(self.main.q_station[self.num])
                except:pass
                try:
                    self.einbq.setValue(self.main.q_basis[self.num])
                except:pass
            elif self.num == -3:
                self.fak = self.main.p_wsg_fak.value()
                self.qwfak = self.qwval*self.fak
                if len(self.main.h1d.rb[30:].split()) > 1:
                    self.einstat.setValue(self.main.h1d.rb[30:].split()[1])
                if len(self.main.h1d.rb[30:].split()) > 2:
                    self.einbq.setValue(self.main.h1d.rb[30:].split()[2])
            elif self.num == -4:
                self.fak = self.main.p_abg_fak.value()
                self.qwfak = self.qwval*self.fak
                if len(self.main.h1d.rb[30:].split()) > 1:
                    self.einstat.setValue(self.main.h1d.rb[30:].split()[1])
                if len(self.main.h1d.rb[30:].split()) > 2:
                    self.einbq.setValue(self.main.h1d.rb[30:].split()[2])
            elif self.num == -5:
                self.einqt.setHorizontalHeaderLabels(("Q","Durchlassöffnung"))
                fak  = self.main.h1d.gatdat[self.idx][10].strip()
                stat = self.main.h1d.gatdat[self.idx][11].strip()
                qbas = self.main.h1d.gatdat[self.idx][12].strip()
                self.fak = 1 if fak == '' else float(fak)
                stat = 0 if stat == '' else float(stat)
                qbas = 0 if qbas == '' else float(qbas)
                self.qwfak = self.qwval*self.fak
                self.einstat.setValue(stat)
                self.einbq.setValue(qbas)
            elif self.num == -6:
                self.fak = 1 
                self.qwfak = self.qwval*self.fak
        self.fakval.setValue(self.fak)
        self.plot()

    
def datshow(self,num=0,idx=None):
    if type(num) == tuple:
        dat = self.p_lie_table.item(num[1],1).text()
    else:
        #zufluss knoten
        if num >= 0:
            dat = self.p_nupe.item(num,1).text()
        #untere rb art 3
        elif num == -3:
            dat = self.p_wsg_dat.text()
        #untere rb art 4
        elif num == -4:
            dat = self.p_abg_dat.text()
        #gate dat
        elif num == -5:
            dat = self.p_gate_table.item(idx,11).text()
        #weir dat
        elif num == -6:
            dat = self.p_weir_table.item(idx,9).text()
    if hasattr(self,'h1denv'):
        einpfad = os.path.join(self.h1denv,dat)
        if os.path.isfile(einpfad):
            Popup = inflowTable(self,dat,num,idx)
            if Popup.exec_():
                if os.path.isfile(Popup.einpfad):
                    text = Popup.dat+ ' existiert! Überschreiben?'
                    ask = QMessageBox.question(self,'Save Q Datei',text,QMessageBox.Yes,QMessageBox.No)
                    if ask == QMessageBox.Yes:
                        QMessageBox.information(self, "Achtung!","Nur Q-Datei und nicht Q-Faktor wird geschrieben!",QMessageBox.Ok)
                        with open(Popup.einpfad,'w') as out:
                            out.write(Popup.eintext.text()+'\n')
                            out.write('{}\t{}\n'.format(Popup.eincount,Popup.eindt))
                            for i in range(Popup.einqt.rowCount()):
                                out.write(Popup.einqt.item(i,0).text()+'\n')

