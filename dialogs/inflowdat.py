# -*- coding: utf-8 -*-
'''import system modules'''
import os
import numpy as np
'''import pyqt5 modules'''
from PyQt5.QtWidgets     import (QDialog,
                                 QTableWidgetItem,
                                 QFileDialog,
                                 QMessageBox)
from ui.inflowdat        import Ui_inflowdat

script_dir = os.getcwd()

class inflowTable(QDialog,Ui_inflowdat):

    def __init__(self, myapp,dat,num,parent=None):
        super().__init__(parent)
        self.setupUi(self)
        self.main = myapp
        self.dat = dat
        self.num = num
        self.einpfad = os.path.join(self.main.h1denv,self.dat)
        self.eindat.setText(self.dat)
        self.einbrowse.clicked.connect(self.browsedat)
        self.qchoose.buttonToggled.connect(self.plot)
        self.buttonBox.accepted.connect(self.accept)
        self.buttonBox.rejected.connect(self.reject)
        self.einqt.itemChanged.connect(self.qtable)
        self.eincount.valueChanged.connect(self.rowinc)
        self.initiate()
    
    def rowinc(self,i):
        self.einqt.setRowCount(i)
        
    def qtable(self):
        if self.qori.isChecked():
            self.qwval = np.array([float(self.einqt.item(i,0).text()) for i in range(self.einqt.rowCount())])
            self.qwfak = self.qwval*self.fak
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
            q = self.qwfak
        elif self.qori.isChecked():
            q = self.qwval
        self.einqt.blockSignals(True)
        self.einqt.setRowCount(len(q))
        for n,i in enumerate(q):
            self.einqt.setItem(n,0,QTableWidgetItem(str(i)))
            
        self.graphicsView.clear()
        self.graphicsView.plot(q,pen='b')
        self.einqt.blockSignals(False)
        
    def initiate(self):
        
        with open(self.einpfad,'r') as ein:
            lines = ein.readlines()
        self.eintext.setText(lines[0])
        self.eincount.setValue(int(lines[1].split()[0]))
        self.eindt.setValue(float(lines[1].split()[1]))
        self.qwval = np.array([i for l in lines[2:] for i in list(map(float,l.split()))])
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
                self.qwfak = self.qwval*fak
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
        self.plot()

    
def datshow(self,num=0):
    if type(num) == tuple:
        dat = self.p_lie_table.item(num[1],1).text()
    else:
        if num >= 0:
            dat = self.p_nupe.item(num,1).text()
        elif num == -3:
            dat = self.p_wsg_dat.text()
        elif num == -4:
            dat = self.p_abg_dat.text()
    if hasattr(self,'h1denv'):
        einpfad = os.path.join(self.h1denv,dat)
        if os.path.isfile(einpfad):
            Popup = inflowTable(self,dat,num)
            if Popup.exec_():
                if os.path.isfile(Popup.einpfad):
                    text = Popup.dat+ ' existiert! Überschreiben?'
                    ask = QMessageBox.question(self,'Save Q Datei',text,QMessageBox.Yes,QMessageBox.No)
                    QMessageBox.information(self, "Achtung! Nur Q-Datei und nicht Q-Faktor wird geschrieben!")
                    if ask == QMessageBox.Yes:
                        with open(Popup.einpfad,'w') as out:
                            out.write(Popup.eintext.text()+'\n')
                            out.write('{}\t{}\n'.format(Popup.eincount,Popup.eindt))
                            for i in range(Popup.einqt.rowCount()):
                                out.write(Popup.einqt.item(i,0).text()+'\n')

