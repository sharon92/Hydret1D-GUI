# -*- coding: utf-8 -*-
'''import system modules'''
import os
import numpy as np
import shapefile as shp

from modules.rawh1d      import HYDRET as h1d
from modules.rawh1d      import renumberHYD

'''import pyqt5 modules'''
from PyQt5.QtWidgets     import (QDialog,
                                 QTableWidgetItem)

from ui.renumberDialog   import Ui_renDialog

script_dir = os.getcwd()

class nodeRenum(QDialog,Ui_renDialog):

    def __init__(self, myapp, parent=None):
        super().__init__(parent)
        self.setupUi(self)
        self.main = myapp
        self.initiate()
        self.overwrite.stateChanged.connect(self.ovw)
        self.ok.clicked.connect(self.accept)
        self.cancel.clicked.connect(self.reject)
    
    def ovw(self):
        if self.overwrite.checkState() == 2:
            self.dataname.setEnabled(False)
        else:
            self.dataname.setEnabled(True)
            
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
        self.dataname.setText(name+'_ren'+str(counter))
            
        original_index = self.main.df_start.index
        nID            = self.main.df_start['ID']
        new_index      = np.zeros(len(nID),dtype = int)
        
        rshp  = shp.Reader(self.main.h1d.achse)
        s_gid,knovon,knobis = [],[],[]
        for n,i in enumerate(rshp.records()):
            s_gid.append(i['GEW_ID'])
            knovon.append(i['KNOVON'])
            knobis.append(i['KNOBIS'])
        
        taken = []
        for kid in np.unique(nID):
            labels = [i for i in range(9999,99,-1) if i not in taken]
            von = knovon[s_gid.index(kid)]
            bis = knobis[s_gid.index(kid)]
            idx = np.where(nID == kid)
            count = len(idx[0])

            try:
                lidx  = labels.index(bis)
                l     = labels[lidx:lidx+count]
            except:
                lidx  = labels.index(bis+1)
                l     = labels[lidx-count:lidx]
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
    if Popup.exec_():
        ori,new = [],[]
        for i in range(Popup.sheet.rowCount()):
            ori.append(int(Popup.sheet.item(i,0).text()))
            new.append(int(Popup.sheet.item(i,1).text()))
        
        d_index = dict(zip(ori,new))
        
        if Popup.dataname.isEnabled():
            self.h1d.startdat  = Popup.dataname.text()+'_Start.dat'
            self.h1d.prodat    = Popup.dataname.text()+'.pro'
            self.h1drun        = Popup.dataname.text()+'.run'
            self.h1d.hyd_f     = Popup.dataname.text()+'.hyd'
            self.h1d.out_f     = Popup.dataname.text()+'.out'
            self.h1d.hyd_p     = os.path.join(self.h1denv,self.h1d.hyd_f)
            self.h1d.startpath = os.path.join(self.h1denv,self.h1d.startdat)
            self.h1d.propath   = os.path.join(self.h1denv,self.h1d.prodat)
            
        self.df_start.rename(index = d_index, inplace=True)
        self.df_wsp.rename(index = d_index,inplace=True)
        
        profil_index = self.df_pro.index
        newp_index   = []
        for pi in profil_index:
            if pi>0:
                newp_index.append(d_index[pi])
            else:
                newp_index.append(-1*d_index[-1*pi])
        p_index = dict(zip(profil_index,newp_index))
        self.df_pro.rename(index = p_index,inplace=True)
        
        renumberHYD(self.h1d,d_index,self)
        self.saveProject()
        n_h1d = h1d(hydret_path = os.path.join(self.h1denv,self.h1drun))
        self.initiate(hyd = n_h1d,i=self.knotenNr.currentIndex())