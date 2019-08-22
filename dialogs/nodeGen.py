# -*- coding: utf-8 -*-
'''import system modules'''
import os
import numpy as np

from modules.rawh1d    import HYDRET as h1d

'''import pyqt5 modules'''
from PyQt5.QtWidgets     import (QDialog,
                                 QTableWidgetItem,
                                 QHeaderView)

from ui.nodegenDialog    import Ui_nodegenDialog

script_dir = os.getcwd()

class nodeGen(QDialog, Ui_nodegenDialog):
    
    def __init__(self, myapp, parent=None):
        super().__init__(parent)
        self.setupUi(self)
        self.main = myapp
        self.kno_widget.itemSelectionChanged.connect(self.itemChange)
        self.count.valueChanged.connect(self.addSchnitt)
        self.edit_name.itemChanged.connect(self.updatetable)
        self.overwrite.stateChanged.connect(self.ovw)
        self.ok.clicked.connect(self.accept)
        self.cancel.clicked.connect(self.reject)
        self.kno_widget.horizontalHeader().setSectionResizeMode(QHeaderView.Stretch)
        self.edit_name.horizontalHeader().setSectionResizeMode(QHeaderView.Stretch)
        self.initiate()
    
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
        self.dataname.setText(name+'_gen'+str(counter))
        
        self.df   = self.main.df_pro[self.main.pos_].copy()
        self.df_s = self.main.df_start.copy()
        self.kno_widget.setRowCount(len(self.df))
        for n,i in enumerate(self.df.index):
            self.kno_widget.setItem(n,0,QTableWidgetItem(str(self.df_s.loc[i]['ID'])))
            self.kno_widget.setItem(n,1,QTableWidgetItem(str(self.df_s.loc[i]['XL'])))
            self.kno_widget.setItem(n,2,QTableWidgetItem(str(i)))
            self.kno_widget.setItem(n,3,QTableWidgetItem(str(self.df.loc[i]['PName'])))
        self.kno_widget.selectRow(list(self.df.index).index(self.main.loc))
        
    def itemChange(self):
        i = self.df.index[self.kno_widget.currentRow()]
        self.idno.setText('ID: '+str(self.df_s.loc[i]['ID']))
        if -1*i in self.main.df_pro.index:
            _iloc = np.argwhere(self.main.df_pro.index==-1*i)[0]
            for il in _iloc:
                if self.main.df_pro.iloc[il]['Mode'] == 'H2':
                    self.copy_rau.setEnabled(True)
                else:
                    self.copy_rau.setEnabled(False)
                    self.copy_rau.setChecked(False)
                if self.main.df_pro.iloc[il]['Mode'] == 'ZS':
                    self.copy_schalt.setEnabled(True)
                else:
                    self.copy_schalt.setEnabled(False)
                    self.copy_schalt.setChecked(False)
        self.addSchnitt()
    
    def updatetable(self):
        self.edit_name.blockSignals(True)
        ss = float(self.start_station.text())
        if self.edit_name.currentColumn() == 2:
            stations = []
            for i in range(self.edit_name.rowCount()):
                stations.append(float(self.edit_name.item(i,2).text()))
            abstand = [round(abs(stations[i]-ss),2) for i in range(len(stations))]
            for i in range(self.edit_name.rowCount()):
                self.edit_name.setItem(i,3,QTableWidgetItem(str(abstand[i])))

        elif self.edit_name.currentColumn() == 3:
            abstand = []
            for i in range(self.edit_name.rowCount()):
                abstand.append(float(self.edit_name.item(i,3).text()))
            station = [round(ss-abstand[i],2) for i in range(len(abstand))]
            for i in range(self.edit_name.rowCount()):
                self.edit_name.setItem(i,2,QTableWidgetItem(str(station[i])))
        self.edit_name.blockSignals(False)
                
    def addSchnitt(self):
        self.edit_name.blockSignals(True)
        count = self.count.value()
        
        try:
            start    = float(self.start_station.text())
            end      = float(self.end_station.text())
        except:
            return
        if not count == 0:
            schnitt  = self.kno_widget.item(self.kno_widget.currentRow(),3).text()
            stations = np.round(np.linspace(start,end,count+1,endpoint=False),2)[1:]
            abstand  = np.round(np.linspace(0,abs(start-end),count+1,endpoint=False),2)[1:]
            knoten   = [k for k in reversed(list(range(1,9999))) if k not in self.main.df_start.index ][:len(stations)]

            self.edit_name.setRowCount(count)
            for i in range(count):
                self.edit_name.setItem(i,0,QTableWidgetItem('Dup_'+schnitt))
                self.edit_name.setItem(i,1,QTableWidgetItem(str(knoten[i])))
                self.edit_name.setItem(i,2,QTableWidgetItem(str(stations[i])))
                self.edit_name.setItem(i,3,QTableWidgetItem(str(abstand[i])))

        else:
            self.edit_name.removeRow(0)
        self.edit_name.blockSignals(False)

def nodegenwinshow(self):
    Popup = nodeGen(self)
    if Popup.exec_():
        add = Popup.edit_name.rowCount()
            
        if add>0:
            if Popup.dataname.isEnabled():
                self.h1d.prodat    = Popup.dataname.text()+'.pro'
                self.h1d.propath   = os.path.join(self.h1denv,self.h1d.prodat)
                self.p_prodat.setText(self.h1d.prodat)
                    
            name,knoten,station = [],[],[]
            for i in range(Popup.edit_name.rowCount()):
                name.append(Popup.edit_name.item(i,0).text())
                knoten.append(int(Popup.edit_name.item(i,1).text()))
                station.append(float(Popup.edit_name.item(i,2).text()))
            
            muster_knoten = int(Popup.kno_widget.item(Popup.kno_widget.currentRow(),2).text())
            delete_knoten = Popup.deletenodes.checkState()
            gefaelle      = Popup.gef.value()

            #update start dataframe
            node          = self.df_start.loc[muster_knoten].copy()
            tiefp         = node['ZO']
            node_p        = self.df_pro.loc[muster_knoten].copy()
            node_y        = [y for y in node_p['Y']]
            konstant      = node['ZO'] - gefaelle*node['XL']
            ZO            = [gefaelle*s + konstant for s in station]
            

            if -1*muster_knoten in self.df_pro.index:
                _iloc = np.argwhere(self.df_pro.index==-1*muster_knoten)[0]
                if Popup.copy_rau.checkState() == 2:
                    for il in _iloc:
                        if self.df_pro.iloc[il].Mode == 'H2':
                            nodeR = self.df_pro.iloc[il].copy()
                if Popup.copy_schalt.checkState() == 2:
                    for il in _iloc:
                        if self.df_pro.iloc[il].Mode == 'ZS':
                            nodeS = self.df_pro.iloc[il].copy()
            
            ID       = int(float((Popup.kno_widget.item(Popup.kno_widget.currentRow(),0).text())))
            idx1     = (self.df_start['ID'] == ID) & (self.df_start['XL'] >station[0])
            iloc1    = len(self.df_start[idx1])
            df_upper = self.df_start[idx1].copy()

            idx2     = (self.df_start['ID'] == ID) & (self.df_start['XL'] <station[-1])
            iloc2    = self.df_start.index.get_loc(self.df_start[idx2].iloc[0].name)
            df_lower = self.df_start[idx2].copy()
            
            if iloc2-iloc1 > 0:
                df_interm = self.df_start[iloc1:iloc2].copy()

            for n,i in enumerate(knoten):
                if delete_knoten ==2:
                    node.name   = i
                    node['XL']  = station[n]
                    node['ZO']  = ZO[n]
                    node['X']   = 0
                    node['Y']   = 0
                    df_upper     = df_upper.append(node)
                    
                    if Popup.copy_rau.checkState() == 2:
                        nodeR.name        = -1*i
                        nodeR['Station']  = station[n]
                        nodeR['PName']    = 'RAUHEITSPROFIL'
                        self.df_pro       = self.df_pro.append(nodeR)
                        
                    if Popup.copy_schalt.checkState() == 2:
                        nodeS.name        = -1*i
                        nodeS['Station']  = station[n]
                        nodeS['PName']    = 'SCHALTPROFIL'
                        self.df_pro       = self.df_pro.append(nodeS)
                        
                    node_p.name      = i
                    node_p['Station']= station[n]
                    node_p['PName']  = name[n] 
                    node_p['Y']      = np.array(node_y+(ZO[n]-tiefp))
                    self.df_pro      = self.df_pro.append(node_p)
                else:
                    #TODO:- add logic here
                    pass

            df_upper = df_upper.append(df_lower)
            del self.df_start
            self.df_start = df_upper
            self.df_pro.reset_index(inplace=True)
            pidx = []
            for i in self.df_start.index:
                if -1*i in self.df_pro.Node.values:
                    try:
                        pidx.append(self.df_pro[(self.df_pro.Node == -1*i) & (self.df_pro.Mode == 'H2')].index[0])
                    except: pass
                if -1*i in self.df_pro.Node.values:
                    try:
                        pidx.append(self.df_pro[(self.df_pro.Node == -1*i) & (self.df_pro.Mode == 'ZS')].index[0])
                    except: pass
                if i in self.df_pro.Node.values:
                    pidx.append(self.df_pro[self.df_pro.Node == i].index[0])

            self.df_pro = self.df_pro.loc[pidx,:]
            self.df_pro.set_index(keys='Node',inplace=True)
            self.saveProject()
            n_h1d = h1d(hydret_path = self.HydretEnv[0])
            self.initiate(hyd = n_h1d,i=self.knotenNr.currentIndex())
        pass