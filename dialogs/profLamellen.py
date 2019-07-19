# -*- coding: utf-8 -*-
'''import system modules'''
import os
import numpy as np
import pandas as pd

from modules.rawh1d      import HYDRET as h1d

'''import pyqt5 modules'''
from PyQt5.QtWidgets     import (QDialog,
                                 QTableWidgetItem)

from ui.rauDialog        import Ui_rauDialog


script_dir = os.getcwd()

class profLamellen(QDialog,Ui_rauDialog):

    def __init__(self, myapp, parent=None):
        super().__init__(parent)
        self.setupUi(self)
        self.main = myapp
        self.initiate()
        self._schaltercheck.stateChanged.connect(self._schalter)
        self.overwrite.stateChanged.connect(self.ovw)
        self.nodes_table.itemSelectionChanged.connect(self.lcd)
        self.ok.clicked.connect(self.accept)
        self.cancel.clicked.connect(self.reject)
    
    def ovw(self):
        if self.overwrite.checkState() == 2:
            self.dataname.setEnabled(False)
        else:
            self.dataname.setEnabled(True)

    def _schalter(self):
        if self._schaltercheck.checkState() == 2:
            self._swert.setEnabled(True)
        else:
            self._swert.setEnabled(False)
        
    def lcd(self):
        self.idx = sorted(set([i.row() for i in self.nodes_table.selectedIndexes()]))
        self.count.display(len(self.idx))
        if len(self.idx)>0:
            node = int(self.nodes_table.item(self.idx[0],0).text())
            N    = self.main.df_pro.loc[node]
            self.samplerauX.setRowCount(N['Npoints'])
            self.samplerauY.setRowCount(N['Npoints'])
            for i in range(N['Npoints']):
                self.samplerauX.setItem(i,0,QTableWidgetItem(str(N['X'][i])))
                self.samplerauY.setItem(i,0,QTableWidgetItem(self.strickler.text()))
        
    def initiate(self):
        name = self.main.h1drun.upper().replace('.RUN','')
        path = self.main.h1denv
        counter = 0
        while True:
            start = name.replace('_ren','')+'_ren'+str(counter)+'_Start.dat'
            pro   = name.replace('_ren','')+'_ren'+str(counter)+'.pro'
            hyd   = name.replace('_ren','')+'_ren'+str(counter)+'.hyd'
            run   = name.replace('_ren','')+'_ren'+str(counter)+'.run'
            
            for f in [start,pro,hyd,run]:
                if os.path.isfile(os.path.join(path,f)):
                    create = False
                    break
                else: create = True
            if create:
                break
            counter +=1
        self.dataname.setText(name+'_gen'+str(counter))
        
        nodes = []
        for i in self.main.df_pro.index:
            if -1*i in self.main.df_pro.index:
                continue
            else:
                nodes.append(i)
        self.nodes_table.setRowCount(len(nodes))
        for i in range(self.nodes_table.rowCount()):
            node = nodes[i]
            self.nodes_table.setItem(i,0,QTableWidgetItem(str(node)))
            self.nodes_table.setItem(i,1,QTableWidgetItem(str(self.main.df_start.loc[abs(node)]['XL'])))
            self.nodes_table.setItem(i,2,QTableWidgetItem(str(int(self.main.df_start.loc[abs(node)]['ID']))))
            self.nodes_table.setItem(i,3,QTableWidgetItem(str(self.main.df_pro.loc[node]['PName'])))

def raumode(self):
    Popup = profLamellen(self)
    if Popup.exec_():
        nodes = []
        for i in Popup.idx:
            nodes.append(int(Popup.nodes_table.item(i,0).text()))
        
        for i in nodes:
            self.df_pro.loc[i]['Mode'] = Popup._modus.text()
            
            # appending Rauheitsprofil
            node                       = self.df_pro.loc[i].copy()
            node.name                  = -1*i
            node['Mode']               = Popup._lmodus.text()
            node['PName']              = 'RAUHEITSPROFIL'
            y = []
            for t in range(Popup.samplerau.rowCount()):
                y.append(float(Popup.samplerau.item(t,1).text()))
            node['Y']                  = np.array(y)
            self.df_pro                = self.df_pro.append(node)
            
            #appending Schalterprofil
            if Popup._schaltercheck.checkState() == 2:
                nodeS                      = self.df_pro.loc[i].copy()
                nodeS['Mode']              = Popup._smodus.text()
                nodeS['PName']             = 'SCHALTPROFIL'
                nodeS['Y']                 = np.array([float(Popup._swert.text())]*node['Npoints'])
                self.df_pro                = self.df_pro.append(nodeS)
        
        #reorder dataframe
        temp = pd.DataFrame(columns = self.df_pro.columns)
        idx  = self.df_pro[self.df_pro.index > 0].index
        for i in idx:
            node = self.df_pro.loc[i].copy()
            
            if -1*i in self.df_pro.index:
                iloc = np.argwhere(self.df_pro.index==-1*i)[0]
                for il in iloc:
                    if self.df_pro.iloc[il].Mode == Popup._lmodus.text():
                        nodeL = self.df_pro.iloc[il].copy()
                        temp = temp.append(nodeL)
                        temp = temp.append(node)
                for il in iloc:
                    if self.df_pro.iloc[il].Mode == Popup._smodus.text():
                        nodeS = self.df_pro.iloc[il].copy()
                        temp = temp.append(nodeS)
            else:
                temp = temp.append(node)
        del self.df_pro
        self.df_pro = temp
        self.saveProject()
        n_h1d = h1d(hydret_path = self.HydretEnv[0])
        self.initiate(hyd = n_h1d,i=self.knotenNr.currentIndex())