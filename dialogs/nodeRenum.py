# -*- coding: utf-8 -*-
'''import system modules'''
import os
import copy
import numpy as np
import shapefile as shp
from modules.rawh1d      import writePRO,writeStart,writeHYD,renumberHYD
'''import pyqt5 modules'''
from PyQt5               import uic
from PyQt5.QtWidgets     import (QDialog,QFileDialog,
                                 QTableWidgetItem)

import sys
script_dir = os.getcwd()
SCRIPT_DIR = os.path.dirname(os.path.realpath(sys.argv[0]))
ui         = os.path.join(SCRIPT_DIR,'ui','renumberDialog.ui')

class nodeRenum(QDialog):

    def __init__(self, myapp, parent=None):
        super().__init__(parent)
        uic.loadUi(ui,self)
        self.ok.clicked.connect(self.save)
        if hasattr(myapp,'df_start'):
            self.df_start = myapp.df_start.copy()
            self.df_pro   = myapp.df_pro.copy()
            self.h1d      = copy.deepcopy(myapp.h1d)
            self.initiate()
            
    def save(self):
        drc = QFileDialog.getExistingDirectory(caption='Ordner Auswählen, in dem Renumber.HYD,Renumber.PRO, und Renumber.DAT gespeichert werden sollen')
        
        if os.path.isdir(drc):
            ori,new = [],[]
            for i in range(self.sheet.rowCount()):
                ori.append(int(self.sheet.item(i,0).text()))
                new.append(int(self.sheet.item(i,1).text()))
            
            d_index = dict(zip(ori,new))
            
            hyd_p     = drc+'/renumber.hyd'
            startpath = drc+'/renumber_start.dat'
            propath   = drc+'/renumber.pro'
            self.df_start.rename(index = d_index, inplace=True)
            
            profil_index = self.df_pro.index
            newp_index   = []
            for pi in profil_index:
                if pi>0:newp_index.append(d_index[pi])
                else   :newp_index.append(-1*d_index[-1*pi])
            p_index = dict(zip(profil_index,newp_index))
            self.df_pro.rename(index = p_index,inplace=True)
            
            h1d = renumberHYD(self.h1d,d_index)
            writePRO(propath,self.df_pro)
            writeStart(startpath,self.df_start,self.h1d._dform)
            writeHYD(hyd_p,h1d)
            
    def initiate(self):
        original_index = self.df_start.index
        nID            = self.df_start['ID']
        new_index      = np.zeros(len(nID),dtype = int)
        
        rshp  = shp.Reader(self.h1d.achse)
        s_gid,knovon,knobis = [],[],[]
        for n,i in enumerate(rshp.records()):
            s_gid.append(i['GEW_ID'])
            knovon.append(i['KNOVON'])
            knobis.append(i['KNOBIS'])
        
        taken = []
        for kid in np.unique(nID):
            von = knovon[s_gid.index(kid)]
            bis = knobis[s_gid.index(kid)]
            taken.append(von)
            taken.append(bis)
            labels = [i for i in range(9999,1,-1) if i not in taken]
            idx = np.where(nID == kid)
            count = len(idx[0])
            
            counter = 1
            pos_counter = True
            while True:
                try:
                    lidx  = labels.index(bis+counter)
                    l     = labels[lidx:lidx+count]
                    break
                except:
                    if counter+bis == 1: pos_counter = False
                    if pos_counter: counter +=1
                    else:           counter -=1
                    
            new_index[idx] = l
            
            vidx           = np.where(original_index == von)
            new_index[vidx]= von
            bidx           = np.where(original_index == bis)
            new_index[bidx]= bis
            taken.append(l)
        self.sheet.setRowCount(len(original_index))
        for i in range(self.sheet.rowCount()):
            self.sheet.setItem(i,0, QTableWidgetItem(str(original_index[i])))
            self.sheet.setItem(i,1, QTableWidgetItem(str(new_index[i])))

def renumber(self):
    Popup = nodeRenum(self)
    if Popup.exec_(): pass