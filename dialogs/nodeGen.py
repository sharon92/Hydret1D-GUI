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
        self.station_show.currentIndexChanged.connect(self.stationChange)
        self.count.valueChanged.connect(self.addSchnitt)
        self.edit_name.itemChanged.connect(self.updatetable)
        self.overwrite.stateChanged.connect(self.ovw)
        self.ok.clicked.connect(self.accept)
        self.cancel.clicked.connect(self.reject)
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
        
        self.station_show.addItems(self.main.df_pro['Station'].astype(str))
        self.station_show.setCurrentIndex(self.main.iloc)
        self.achse_show.setText(self.main.h1d.achse)
        
        try:
            self.start_station.setText(str(self.main.df_pro.iloc[self.main.iloc]['Station']))
            self.end_station.setText(str(self.main.df_pro.iloc[self.main.iloc+1]['Station']))
        except:
            self.start_station.setText(str(self.main.df_pro.iloc[self.main.iloc-1]['Station']))
            self.end_station.setText(str(self.main.df_pro.iloc[self.main.iloc]['Station']))

        self.schnitt_show.setText(self.main.df_pro.iloc[self.main.iloc]['PName'])
        self.node_show.setText(str(self.main.df_pro.iloc[self.main.iloc].name))
        self.id.setText(str(int(self.main.df_start.loc[int(self.node_show.text())]['ID'])))
        
    def stationChange(self,i):
        self.schnitt_show.setText(self.main.df_pro.iloc[i]['PName'])
        self.node_show.setText(str(self.main.df_pro.iloc[i].name))
        self.id.setText(str(int(self.main.df_start.loc[int(self.node_show.text())]['ID'])))
        
        try:
            self.start_station.setText(str(self.main.df_pro.iloc[i]['Station']))
            self.end_station.setText(str(self.main.df_pro.iloc[i+1]['Station']))
        except:
            self.start_station.setText(str(self.main.df_pro.iloc[i-1]['Station']))
            self.end_station.setText(str(self.main.df_pro.iloc[i]['Station']))
    
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
        
        if not count == 0:
#            try:
            start    = float(self.start_station.text())
            end      = float(self.end_station.text())
            schnitt  = self.schnitt_show.text()
            stations = np.round(np.linspace(start,end,count+1,endpoint=False),2)[1:]
            abstand  = np.round(np.linspace(0,abs(start-end),count+1,endpoint=False),2)[1:]
            knoten   = [k for k in reversed(list(range(1,9999))) if k not in self.main.df_start.index ][:len(stations)]

            self.edit_name.setRowCount(count)
            for i in range(count):
                self.edit_name.setItem(i,0,QTableWidgetItem('Dup_'+schnitt))
                self.edit_name.setItem(i,1,QTableWidgetItem(str(knoten[i])))
                self.edit_name.setItem(i,2,QTableWidgetItem(str(stations[i])))
                self.edit_name.setItem(i,3,QTableWidgetItem(str(abstand[i])))
#            except:
#                pass
        else:
            self.edit_name.removeRow(0)
        self.edit_name.blockSignals(False)

def nodegenwinshow(self):
    Popup = nodeGen(self)
    if Popup.exec_():
        add = Popup.edit_name.rowCount()
        
        if add>0:
            name,knoten,station = [],[],[]
            for i in range(Popup.edit_name.rowCount()):
                name.append(Popup.edit_name.item(i,0).text())
                knoten.append(int(Popup.edit_name.item(i,1).text()))
                station.append(float(Popup.edit_name.item(i,2).text()))
            muster_knoten = int(Popup.node_show.text())
            delete_knoten = Popup.deletenodes.checkState()
            gefaelle      = Popup.gef.value()

            #update start dataframe
            node          = self.df_start.loc[muster_knoten].copy()
            tiefp         = node['ZO']
            node_p        = self.df_pro.loc[muster_knoten].copy()
            node_y        = [y for y in node_p['Y']]
            konstant      = node['ZO'] - gefaelle*node['XL']
            ZO            = [gefaelle*s + konstant for s in station]
            
            idx1     = (self.df_start['ID'] == int(Popup.id.text())) & (self.df_start['XL'] >station[0])
            iloc1    = len(self.df_start[idx1])
            df_upper = self.df_start[idx1].copy()

            idx2     = (self.df_start['ID'] == int(Popup.id.text())) & (self.df_start['XL'] <station[-1])
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
                    
                    node_p.name      = i
                    node_p['Station']= station[n]
                    node_p['PName']  = name[n] 
                    node_p['Y']      = np.array(node_y+(ZO[n]-tiefp))
                    self.df_pro      = self.df_pro.append(node_p)
                else:
                    pass
            

            df_upper = df_upper.append(df_lower)
            del self.df_start
            self.df_start = df_upper
            
            pidx = [i for i in self.df_start.index if i in self.df_pro.index]
            self.df_pro = self.df_pro.loc[pidx,:]
            
            self.saveProject()
            n_h1d = h1d(hydret_path = self.HydretEnv[0])
            self.initiate(hyd = n_h1d,i=self.knotenNr.currentIndex())
        pass