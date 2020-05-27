# -*- coding: utf-8 -*-
'''import system modules'''
import os
import numpy as np
import pandas as pd
from functools           import partial
from modules.rawh1d      import HYDRET as h1d
from modules.riverbed    import riv_bed

import sys
script_dir = os.getcwd()
SCRIPT_DIR = os.path.dirname(os.path.realpath(sys.argv[0]))
ui         = os.path.join(SCRIPT_DIR,'ui','rauDialog.ui')

'''import pyqt5 modules'''
from PyQt5               import uic
from PyQt5.QtWidgets     import (QDialog,
                                 QTableWidgetItem,
                                 QHeaderView)
from PyQt5.QtGui         import QColor

script_dir = os.getcwd()

class profLamellen(QDialog):

    def __init__(self, myapp, parent=None):
        super().__init__(parent)
        uic.loadUi(ui,self)
        self.main = myapp
        self.initiate()
        self.overwrite.stateChanged.connect(self.ovw)
        self.nodes_table.itemSelectionChanged.connect(self.lcd)
        self.ok.clicked.connect(self.accept)
        self.cancel.clicked.connect(self.reject)
        self.nodes_table.horizontalHeader().setSectionResizeMode(QHeaderView.ResizeToContents)
        comboboxlist = [self.f_xv,self.f_xb,self.vs_xv,self.vs_xb]
        [combobox.currentIndexChanged.connect(partial(self.vonbischanged,but=combobox)) for combobox in comboboxlist]
        [spinbox.valueChanged.connect(partial(self.vonbischanged,but=None)) for spinbox in [self.f_kst,self.vg_kst,self.vs_kst]]

    def ovw(self):
        if self.overwrite.checkState() == 2:
            self.dataname.setEnabled(False)
        else:
            self.dataname.setEnabled(True)
        
    def lcd(self):

        self.idx = sorted(set([i.row() for i in self.nodes_table.selectedIndexes()]))
        if len(self.idx)>0:
            self.N = self.main.df_pro.loc[int(self.nodes_table.item(self.idx[0],0).text())]
            self.banks()
            
    def banks(self):
        riv_bed_idx = riv_bed(self.N)[1]
        
        #flussbett
        self.left_fbank = self.N.Y[:riv_bed_idx].argmax()
        self.right_fbank = riv_bed_idx + self.N.Y[riv_bed_idx:].argmax()
        
        #vorland mit vegetation streifen
        if self.left_fbank < 3:
            self.left_sbank = 0
        else:
            self.left_sbank = self.left_fbank-3
        
        if self.right_fbank > len(self.N.X)-4:
            self.right_sbank = len(self.N.X)-1
        else:
            self.right_sbank = self.right_fbank+3

        self.vonbis(add=True)
    
    def vonbischanged(self,i,but=None):
        try:
            self.left_fbank  = self.f_xv.currentIndex()
            self.right_fbank = self.f_xb.currentIndex()
            
            self.left_sbank  = self.vs_xv.currentIndex()
            self.right_sbank = self.vs_xb.currentIndex()
            
            idx = [self.left_sbank,self.left_fbank,self.right_fbank,self.right_sbank]
            
            if but is None:
                self.vonbis(add=False)
            
            elif sorted(idx) == idx:
                self.vonbis(add=False)
                
            else:
                bord = [self.vs_xv,self.f_xv,self.f_xb,self.vs_xb].index(but)
                
                if bord == 0:
                    if self.left_sbank > self.left_fbank:
                        self.left_fbank = self.left_sbank
                    if self.left_fbank > self.right_fbank:
                        self.right_fbank = self.left_fbank
                    if self.right_fbank> self.right_sbank:
                        self.right_sbank = self.right_fbank
                        
                elif bord == 1:
                    if self.left_fbank < self.left_sbank:
                        self.left_sbank = self.left_fbank
                    if self.left_fbank > self.right_fbank:
                        self.right_fbank = self.left_fbank
                    if self.right_fbank> self.right_sbank:
                        self.right_sbank = self.right_fbank
                        
                elif bord == 2:
                    if self.right_fbank> self.right_sbank:
                        self.right_sbank = self.right_fbank
                    if self.right_fbank< self.left_fbank:
                        self.left_fbank = self.right_fbank
                    if self.left_fbank < self.left_sbank:
                        self.left_sbank = self.left_fbank
    
                elif bord == 3:
                    if self.right_sbank< self.right_fbank:
                        self.right_fbank = self.right_sbank
                    if self.right_fbank< self.left_fbank:
                        self.left_fbank = self.right_fbank
                    if self.left_fbank < self.left_sbank:
                        self.left_sbank = self.left_fbank
                self.vonbis(add=False)
        except:pass
    
    def vonbis(self,add=True):
        [i.blockSignals(True) for i in [self.f_xv,self.f_xb,self.vs_xv,self.vs_xb]]
        
        if add:
            [i.clear() for i in [self.f_xv,self.f_xb,self.vs_xv,self.vs_xb]]

            self.f_xv.addItems(self.N.X.astype(str))
            self.f_xb.addItems(self.N.X.astype(str))
            self.vs_xv.addItems(self.N.X.astype(str))
            self.vs_xb.addItems(self.N.X.astype(str))
            self.vg_xv.setText(str(self.N.X[0]))
            self.vg_xb.setText(str(self.N.X[-1]))
        self.f_kst2.setText(str(self.f_kst.value()))
        self.vs_kst2.setText(str(self.vs_kst.value()))
        self.vg_kst2.setText(str(self.vg_kst.value()))
       
        self.vs_xv.setCurrentIndex(self.left_sbank)
        self.f_xv.setCurrentIndex(self.left_fbank)
        self.f_xb.setCurrentIndex(self.right_fbank)
        self.vs_xb.setCurrentIndex(self.right_sbank)

        [i.blockSignals(False) for i in [self.f_xv,self.f_xb,self.vs_xv,self.vs_xb]]
        
        self.x= np.array([self.N.X[0],
                          float(self.vs_xv.currentText()),
                          float(self.vs_xv.currentText()),
                          float(self.f_xv.currentText()),
                          float(self.f_xv.currentText()),
                          float(self.f_xb.currentText()),
                          float(self.f_xb.currentText()),
                          float(self.vs_xb.currentText()),
                          float(self.vs_xb.currentText()),
                          self.N.X[-1]])
    
        self.y= np.full(10,self.vg_kst.value())
        self.y[2:4] = self.vs_kst.value()
        self.y[4:6] = self.f_kst.value()
        self.y[6:8] = self.vs_kst.value()
        
        self.plot()
        
    def plot(self):
        '''kst'''
        if hasattr(self,'profplot'):
            self.profplot.setData(self.N.X,self.N.Y)
            self.plotarrows(fresh=False)
            self.rauView.getViewBox().autoRange(items=[self.profplot])
        else:
            self.profplot = self.rauView.plot(self.N.X,self.N.Y,pen='k',symbol='d',
                                             symbolSize=7,symbolPen='r',symbolBrush='r')
            self.profplot.setZValue(0)
            self.rauView.showAxis('right')
            self.plotarrows(fresh=True)

    
    def plotarrows(self,fresh=True):

        ybot = self.N.Y.min()
        
        if fresh:
            self.vorg_l = self.rauView.plot(self.N.X[:self.vs_xv.currentIndex()+1],
                                            self.N.Y[:self.vs_xv.currentIndex()+1],
                                            pen=None,fillLevel=ybot,brush=QColor(0,150,0,150))

            self.vors_l = self.rauView.plot(self.N.X[self.vs_xv.currentIndex():self.f_xv.currentIndex()+1],
                                            self.N.Y[self.vs_xv.currentIndex():self.f_xv.currentIndex()+1],
                                            pen=None,fillLevel=ybot,brush=QColor(255,0,0,150))

            self.fb     = self.rauView.plot(self.N.X[self.f_xv.currentIndex():self.f_xb.currentIndex()+1],
                                            self.N.Y[self.f_xv.currentIndex():self.f_xb.currentIndex()+1],
                                            pen=None,fillLevel=ybot,brush=QColor(0,0,200,150))
            
            self.vors_r = self.rauView.plot(self.N.X[self.f_xb.currentIndex():self.vs_xb.currentIndex()+1],
                                            self.N.Y[self.f_xb.currentIndex():self.vs_xb.currentIndex()+1],
                                            pen=None,fillLevel=ybot,brush=QColor(255,0,0,150))
            
            self.vorg_r = self.rauView.plot(self.N.X[self.vs_xb.currentIndex():],
                                            self.N.Y[self.vs_xb.currentIndex():],
                                            pen=None,fillLevel=ybot,brush=QColor(0,150,0,150))

        else:
            self.vorg_l.setData(self.N.X[:self.vs_xv.currentIndex()+1],
                                            self.N.Y[:self.vs_xv.currentIndex()+1],fillLevel=ybot)

            self.vors_l.setData(self.N.X[self.vs_xv.currentIndex():self.f_xv.currentIndex()+1],
                                            self.N.Y[self.vs_xv.currentIndex():self.f_xv.currentIndex()+1],fillLevel=ybot)

            self.fb.setData(self.N.X[self.f_xv.currentIndex():self.f_xb.currentIndex()+1],
                                            self.N.Y[self.f_xv.currentIndex():self.f_xb.currentIndex()+1],fillLevel=ybot)
            
            self.vors_r.setData(self.N.X[self.f_xb.currentIndex():self.vs_xb.currentIndex()+1],
                                            self.N.Y[self.f_xb.currentIndex():self.vs_xb.currentIndex()+1],fillLevel=ybot)
            
            self.vorg_r.setData(self.N.X[self.vs_xb.currentIndex():],
                                            self.N.Y[self.vs_xb.currentIndex():],fillLevel=ybot)

        
    def initiate(self):
        name = self.main.h1drun.upper().replace('.RUN','')
        path = self.main.h1denv
        counter = 0
        while True:
            start = name.replace('_rau','')+'_rau'+str(counter)+'_Start.dat'
            pro   = name.replace('_rau','')+'_rau'+str(counter)+'.pro'
            hyd   = name.replace('_rau','')+'_rau'+str(counter)+'.hyd'
            run   = name.replace('_rau','')+'_rau'+str(counter)+'.run'
            
            for f in [start,pro,hyd,run]:
                if os.path.isfile(os.path.join(path,f)):
                    create = False
                    break
                else: create = True
            if create:
                break
            counter +=1
        self.dataname.setText(name+'_rau'+str(counter))
        
        nodes = []
        for i in self.main.df_pro.index:
            if i>0:
                if not (sorted(self.main.df_pro.loc[i].X) == self.main.df_pro.loc[i].X).all():
                    continue
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
        
        if Popup.dataname.isEnabled():
            self.h1d.prodat    = Popup.dataname.text()+'.pro'
            self.h1d.propath   = os.path.join(self.h1denv,self.h1d.prodat)
            self.p_prodat.setText(self.h1d.prodat)
            
        nodeID = int(Popup.nodes_table.item(Popup.idx[0],0).text())
        
        i = nodeID
        self.df_pro.loc[i]['Mode'] = 'RS'
        
        # appending Rauheitsprofil
        if -1*i in self.df_pro.index:
            _iloc = np.argwhere(self.df_pro.index==-1*i)[0]
            for il in _iloc:
                if self.df_pro.iloc[il]['Mode'] == 'H2':
                    nodeR           = self.df_pro.iloc[il]
                if self.df_pro.iloc[il]['Mode'] == 'ZS':
                    nodeS           = self.df_pro.iloc[il]
        else:
            nodeR                   = self.df_pro.loc[i].copy()
            nodeS                   = self.df_pro.loc[i].copy()
            
        nodeR.name                  = -1*i
        nodeR['Mode']               = 'H2'
        nodeR['PName']              = 'RAUHEITSPROFIL'
        nodeR['Npoints']            = len(Popup.x)
        nodeR.X                     = Popup.x
        nodeR.Y                     = Popup.y
        self.df_pro                 = self.df_pro.append(nodeR)
        
        #appending Schalterprofil
        if Popup._schaltercheck.checkState() == 2:
            nodeS['Mode']          = 'ZS'
            nodeS['PName']         = 'SCHALTPROFIL'
            self.df_pro            = self.df_pro.append(nodeS)

        #reorder dataframe
        temp = pd.DataFrame(columns = self.df_pro.columns)
        idx  = self.df_pro[self.df_pro.index > 0].index
        for i in idx:
            node = self.df_pro.loc[i].copy()
            
            if -1*i in self.df_pro.index:
                iloc = np.argwhere(self.df_pro.index==-1*i)[0]
                for il in iloc:
                    if self.df_pro.iloc[il].Mode == 'H2':
                        nodeL = self.df_pro.iloc[il].copy()
                        temp = temp.append(nodeL)
                for il in iloc:
                    if self.df_pro.iloc[il].Mode == 'ZS':
                        nodeS = self.df_pro.iloc[il].copy()
                        temp = temp.append(nodeS)
                temp = temp.append(node)
            else:
                temp = temp.append(node)
        del self.df_pro
        self.df_pro = temp
        self.saveProject()
        n_h1d = h1d(hydret_path = self.h1drunpath)
        self.initiate(hyd = n_h1d,i=self.knotenNr.currentIndex())