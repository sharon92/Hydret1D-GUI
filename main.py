# -*- coding: utf-8 -*-
from fbs_runtime.application_context.PyQt5 import ApplicationContext
'''import system modules'''
import sys
import os
import numpy as np
import pandas as pd
import shapefile as shp

'''import modules to read hydret'''
from modules.rawh1d      import HYDRET as h1d
from modules.rawh1d      import renumberHYD,writePRO,writeStart,writeHYD,writeRUN
from modules.loaddata    import dataadd,dataremove
from modules.editsection import (update_labels,
                                 knoten_label,
                                 edit_modus,
                                 edit_maxHeight,
                                 delete_coords,
                                 insert_coords,
#                                 undo_but,
#                                 redo_but,
                                 _handlecopy,
                                 _handlepaste)
from modules.loadhyd     import (load_hyd,
                                 inflowNodes,
                                 printCurves,
                                 defWeirs,
                                 defGates,
                                 defJunctions,
                                 lateralInflows,
                                 ovfmode,
                                 updateHyd)
from modules.loadgeo     import load_start
from modules.riverbed    import riv_bed
from modules.plotting    import (plot,
                                 wsp_df_update,
                                 nodePlot,
                                 langPlot,
                                 changeAR,
                                 plan_name,
                                 loadresult,
                                 update_wsp,
                                 nodechange_inPRO,
                                 onXYselection,
                                 colorpicker)

'''import pyqt5 modules'''
import pyqtgraph as pg
from PyQt5.QtGui         import (QFont,
                                 QColor,
                                 QIcon,
                                 QPixmap)
from PyQt5.QtWidgets     import (QMainWindow,
                                 QMessageBox,
                                 QDialog,
                                 QFileDialog,
                                 QAbstractItemView,
                                 QTableWidgetItem,
                                 QHeaderView)
from PyQt5.QtCore        import (QCoreApplication,
                                 pyqtSignal,
                                 QProcess)

from ui.hydretUI         import Ui_MainWindow
from ui.dialog           import Ui_Dialog
from ui.nodedialog       import Ui_nodeDialog
from ui.raudialog        import Ui_raudialog
#from pyupdater.client import Client
#from client_config import ClientConfig
#
#APP_NAME = 'Hydret1D'
#APP_VERSION = '1.0.0'
#
#ASSET_NAME = 'Hydret1D'
#ASSET_VERSION = '1.0.0'
#
#def print_status_info(info):
#    total = info.get(u'total')
#    downloaded = info.get(u'downloaded')
#    status = info.get(u'status')
#    print(downloaded, total, status)
#
#client = Client(ClientConfig(), refresh=True,
#                        progress_hooks=[print_status_info])
#
#app_update = client.update_check(APP_NAME, APP_VERSION)
#
#if app_update is not None:
#    app_update.download(background=True)
#    
#    if app_update.is_downloaded():
#        app_update.extract_restart()
        
pg.setConfigOption('background', 'w')
pg.setConfigOption('foreground', 'k')
_translate = QCoreApplication.translate

SCRIPT_DIR = os.path.dirname(os.path.realpath(sys.argv[0]))

global DOUBLECLICK_FILE
DOUBLECLICK_FILE = False

class nodeDuplicate(QDialog, Ui_nodeDialog):
    
    def __init__(self, parent=None):
        super().__init__(parent)
        self.setupUi(self)
        self.station_show.currentIndexChanged.connect(self.stationChange)
        self.count.valueChanged.connect(self.addSchnitt)
        self.edit_name.itemChanged.connect(self.updatetable)
        self.ok.clicked.connect(self.accept)
        self.cancel.clicked.connect(self.reject)
        self.edit_name.horizontalHeader().setSectionResizeMode(QHeaderView.Stretch)
        self.initiate()
        
    def initiate(self):
        self.station_show.addItems(myapp.df_pro['Station'].astype(str))
        self.station_show.setCurrentIndex(myapp.iloc)
        self.achse_show.setText(myapp.h1d.achse)
        
        try:
            self.start_station.setText(str(myapp.df_pro.iloc[myapp.iloc]['Station']))
            self.end_station.setText(str(myapp.df_pro.iloc[myapp.iloc+1]['Station']))
        except:
            self.start_station.setText(str(myapp.df_pro.iloc[myapp.iloc-1]['Station']))
            self.end_station.setText(str(myapp.df_pro.iloc[myapp.iloc]['Station']))

        self.schnitt_show.setText(myapp.df_pro.iloc[myapp.iloc]['PName'])
        self.node_show.setText(str(myapp.df_pro.iloc[myapp.iloc].name))
        self.id.setText(str(int(myapp.df_start.loc[int(self.node_show.text())]['ID'])))
        
    def stationChange(self,i):
        self.schnitt_show.setText(myapp.df_pro.iloc[i]['PName'])
        self.node_show.setText(str(myapp.df_pro.iloc[i].name))
        self.id.setText(str(int(myapp.df_start.loc[int(self.node_show.text())]['ID'])))
        
        try:
            self.start_station.setText(str(myapp.df_pro.iloc[i]['Station']))
            self.end_station.setText(str(myapp.df_pro.iloc[i+1]['Station']))
        except:
            self.start_station.setText(str(myapp.df_pro.iloc[i-1]['Station']))
            self.end_station.setText(str(myapp.df_pro.iloc[i]['Station']))
    
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
            knoten   = [k for k in reversed(list(range(1,9999))) if k not in myapp.df_start.index ][:len(stations)]

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

class lamellen(QDialog,Ui_raudialog):

    def __init__(self, parent=None):
        super().__init__(parent)
        self.setupUi(self)
        self.initiate()
        self._schaltercheck.stateChanged.connect(self._schalter)
        self.nodes_table.itemSelectionChanged.connect(self.lcd)
        self.del_.clicked.connect(self.update_m)
        self.add.clicked.connect(self.update_a)
        self.ok.clicked.connect(self.accept)
        self.cancel.clicked.connect(self.reject)
    
    def update_m(self):
        self.samplerau.removeRow(self.samplerau.currentRow())

    def update_a(self):
        self.samplerau.insertRow(self.samplerau.currentRow())
        
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
            N    = myapp.df_pro.loc[node]
            self.samplerau.setRowCount(N['Npoints'])
            for i in range(N['Npoints']):
                self.samplerau.setItem(i,0,QTableWidgetItem(str(N['X'][i])))
                self.samplerau.setItem(i,1,QTableWidgetItem(self.strickler.text()))
        
    def initiate(self):
        nodes = []
        for i in myapp.df_pro.index:
            if -1*i in myapp.df_pro.index:
                continue
            else:
                nodes.append(i)
        self.nodes_table.setRowCount(len(nodes))
        for i in range(self.nodes_table.rowCount()):
            node = nodes[i]
            self.nodes_table.setItem(i,0,QTableWidgetItem(str(node)))
            self.nodes_table.setItem(i,1,QTableWidgetItem(str(myapp.df_start.loc[abs(node)]['XL'])))
            self.nodes_table.setItem(i,2,QTableWidgetItem(str(int(myapp.df_start.loc[abs(node)]['ID']))))
            self.nodes_table.setItem(i,3,QTableWidgetItem(str(myapp.df_pro.loc[node]['PName'])))
        
        
class hydretPopup(QDialog, Ui_Dialog):
    errorSignal  = pyqtSignal(str) 
    outputSignal = pyqtSignal(str)
    def __init__(self, parent=None):
        super().__init__(parent)
        self.setupUi(self)
        self.output = None
        self.error  = None
        self.process = QProcess()
        self.process.readyReadStandardError.connect(self.onReadyReadStandardError)
        self.process.readyReadStandardOutput.connect(self.onReadyReadStandardOutput)

    def onReadyReadStandardError(self):
        error = self.process.readAllStandardError().data().decode(errors='ignore')
        self.plainTextEdit.appendPlainText(error)
        self.errorSignal.emit(error)

    def onReadyReadStandardOutput(self):
        result = self.process.readAllStandardOutput().data().decode(errors='ignore')
        self.plainTextEdit.appendPlainText(result)
        self.outputSignal.emit(result)
        
class MainW(QMainWindow, Ui_MainWindow):
    
    def __init__(self):
        QMainWindow.__init__(self)
        self.setupUi(self)
        self.showMaximized()
        
        #status
        self.Edit = False
        self.changes = 0
        self.statusbar = self.statusBar
        self.statusbar.showMessage('Ready')
        
        #Menu commands
        self.openp.triggered.connect(self.OpenEnv)
        self.start_editing.triggered.connect(self.initiateEditing)
        self.stop_editing.triggered.connect(self.FinishEditing)
        self.savep.triggered.connect(self.saveProject)
        self.run.triggered.connect(self.runModel)
        self.node_renumber.triggered.connect(self.renumber)
        self.node_rau.triggered.connect(self.raumode)
        self.Duplicateschnitts.triggered.connect(self.duplicatewinshow)
        self.loadwsp_2.triggered.connect(connections.connect_loadresult)
        #self.closef.triggered.connect(MainW.restoreState(STATE))
        #self.saveasp.triggered.connect(self.savePRO)
        
        #Docks
        self.tabifyDockWidget(self.qp_dock,self.lp_dock)
        #self.setTabPosition(Qt.RightDockWidgetArea,QTabWidget.North)

#        #hyd signals
        self.p_nqin.valueChanged.connect(connections.connect_inflowNodes)
        self.p_nwel.valueChanged.connect(connections.connect_printCurves)
        self.p_njunc.valueChanged.connect(connections.connect_defJunctions)
        self.p_nweirs.valueChanged.connect(connections.connect_defWeirs)
        self.p_ngates.valueChanged.connect(connections.connect_defGates)
        self.p_latinf.valueChanged.connect(connections.connect_lateralInflows)
        self.p_ovfbil_2.currentIndexChanged.connect(connections.connect_ovfmode)
        
        #Profil Props
        self.qschnp = ['VK','RK','VF','RF','DF','OF','H2','RS','ZS']
        self.qschnt = [1,2,3,4,5,6,7,8,9]
        self.itype_dict = {1:('h',6,'k'),
                           2:('s',6,'r'),
                           3:('p',6,'k'),
                           4:('o',6,'b'),
                           5:('+',9,'k'),
                           9:('d',9,'b'),
                           0:('star',10,'g')}
        
        self.knotenNr.currentIndexChanged.connect(self.idChange)
        self.station_label.currentIndexChanged.connect(self.idChange)
        self.schnittName_label.currentIndexChanged.connect(self.idChange)
        self.lang_ID.currentTextChanged.connect(self.gewidChanged)
        self.coords_table.itemSelectionChanged.connect(connections.connect_onXYselection)
        self.coords_table.itemChanged.connect(self.plot_update_coords)
        self.p_wspdat.itemChanged.connect(connections.connect_update_wsp)
        
        #loading data (shapefiles)
        self.lp_adddata.clicked.connect(connections.connect_dataadd)
        self.lp_removedata.clicked.connect(connections.connect_dataremove)
        
        #plotting
        self.ratio = 1
        self.AspectRatio.valueChanged.connect(connections.connect_changeAR)
        self.p_plan.editingFinished.connect(connections.connect_plan_name)
        self.p_wspdat.itemDoubleClicked.connect(connections.connect_colorpicker)
        self.langView.scene().sigMouseMoved.connect(MouseMovements.mouseMoved_lang)
        self.nodeView.scene().sigMouseMoved.connect(MouseMovements.mouseMoved_node)
        self.graphicsView.scene().sigMouseMoved.connect(MouseMovements.mouseMoved_quer)
        self.graphicsView2.scene().sigMouseMoved.connect(MouseMovements.mouseMoved_quer2)
        self.graphicsView.getViewBox().sigResized.connect(self.updateViews)
    
        #connect radio buttons
        self._rquer.toggled.connect(self.table_toggle)
        self._rrau.toggled.connect(self.table_toggle)
        self._rschalter.toggled.connect(self.table_toggle)
        
        #under editing mode
        self.edit_knoten.editingFinished.connect(connections.connect_knoten_label)
        self.modus_label.activated[str].connect(connections.connect_edit_modus)
        self.maxHeight_label.editingFinished.connect(connections.connect_edit_maxHeight)
        self.delete_points.clicked.connect(connections.connect_delete_coords)
        self.insert_points.clicked.connect(connections.connect_insert_coords)
        self.undo.clicked.connect(self.undo_but)
        self.redo.clicked.connect(self.redo_but)
        self.copy_clip.clicked.connect(connections.connect_handlecopy)
        self.paste_clip.clicked.connect(connections.connect_handlepaste)
        
        #icons
        icon = QIcon()
        icon.addPixmap(QPixmap(os.path.join(SCRIPT_DIR,"icons","hydret1D.ico")), QIcon.Normal,QIcon.Off)
        self.setWindowIcon(icon)
        icon1 = QIcon()
        icon1.addPixmap(QPixmap(os.path.join(SCRIPT_DIR,"icons","redo.ico")), QIcon.Normal,QIcon.Off)
        self.redo.setIcon(icon1)
        icon2 = QIcon()
        icon2.addPixmap(QPixmap(os.path.join(SCRIPT_DIR,"icons","undo.ico")), QIcon.Normal,QIcon.Off)
        self.undo.setIcon(icon2)
        icon3 = QIcon()
        icon3.addPixmap(QPixmap(os.path.join(SCRIPT_DIR,"icons","water.ico")), QIcon.Normal,QIcon.Off)
        self.actionProfil.setIcon(icon3)
        icon4 = QIcon()
        icon4.addPixmap(QPixmap(os.path.join(SCRIPT_DIR,"icons","map-3-24.ico")), QIcon.Normal,QIcon.Off)
        self.actionLageplan.setIcon(icon4)
        icon5 = QIcon()
        icon5.addPixmap(QPixmap(os.path.join(SCRIPT_DIR,"icons","long.ico")), QIcon.Normal,QIcon.Off)
        self.actionL_ngschnit.setIcon(icon5)
        icon6 = QIcon()
        icon6.addPixmap(QPixmap(os.path.join(SCRIPT_DIR,"icons","new.ico")), QIcon.Normal,QIcon.Off)
        self.newp.setIcon(icon6)
        icon7 = QIcon()
        icon7.addPixmap(QPixmap(os.path.join(SCRIPT_DIR,"icons","open32.ico")), QIcon.Normal,QIcon.Off)
        self.openp.setIcon(icon7)
        icon8 = QIcon()
        icon8.addPixmap(QPixmap(os.path.join(SCRIPT_DIR,"icons","save.ico")), QIcon.Normal,QIcon.Off)
        self.savep.setIcon(icon8)
        icon9 = QIcon()
        icon9.addPixmap(QPixmap(os.path.join(SCRIPT_DIR,"icons","runmodel.ico")), QIcon.Normal,QIcon.Off)
        self.run.setIcon(icon9)
        
        #set table sizes for nicer display
        self.coords_table.horizontalHeader().setSectionResizeMode(QHeaderView.Stretch)
        self.wsp_table.horizontalHeader().setSectionResizeMode(QHeaderView.Stretch)
        
        self.p_wspdat.horizontalHeader().setSectionResizeMode(0,QHeaderView.Stretch)
        for i in range(4):
            self.p_wspdat.horizontalHeader().setSectionResizeMode(i+1,QHeaderView.ResizeToContents)
        
        self.p_weir_table.horizontalHeader().setSectionResizeMode(9,QHeaderView.Stretch)
        for i in range(9):
            self.p_weir_table.horizontalHeader().setSectionResizeMode(i,QHeaderView.ResizeToContents)
            
        for i in range(9):
            self.p_gate_table.horizontalHeader().setSectionResizeMode(i,QHeaderView.ResizeToContents)
        self.p_gate_table.horizontalHeader().setSectionResizeMode(9,QHeaderView.Stretch)
        
        self.p_lie_table.horizontalHeader().setSectionResizeMode(1,QHeaderView.Stretch)
        for i in [0,2,3,4,5,6]:
            self.p_lie_table.horizontalHeader().setSectionResizeMode(i,QHeaderView.ResizeToContents)

        self.p_nxj_table.horizontalHeader().setSectionResizeMode(QHeaderView.Stretch)
        self.p_dxlgam_table.horizontalHeader().setSectionResizeMode(QHeaderView.Stretch)
        
        self.p_nupe.horizontalHeader().setSectionResizeMode(1,QHeaderView.Stretch)
        for i in [0,2,3]:
            self.p_nupe.horizontalHeader().setSectionResizeMode(i,QHeaderView.ResizeToContents)
     
    def raumode(self):
        Popup = lamellen()
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
                    iloc = np.argwhere(myapp.df_pro.index==-1*i)[0]
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
        
    def renumber(self):
        
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
        
        d_index = dict(zip(original_index,new_index))
        self.df_start.rename(index = d_index, inplace=True)
        self.df_wsp.rename(index = d_index,inplace=True)
        
        profil_index = self.df_pro.index
        newp_index   = []
        for pi in profil_index:
            if pi>0:
                newp_index.append(d_index[pi])
            else:
                newp_index.append(-1*d_index[pi])
        p_index = dict(zip(profil_index,newp_index))
        self.df_pro.rename(index = p_index,inplace=True)
        
        self.h1d = renumberHYD(self.h1d,d_index,myapp)
        self.saveProject()
        n_h1d = h1d(hydret_path = self.HydretEnv[0])
        self.initiate(hyd = n_h1d,i=self.knotenNr.currentIndex())
            

    def duplicatewinshow(self):
        Popup = nodeDuplicate()
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
        
    def runModel(self):
        os.chdir(self.h1denv)
        self.statusbar.showMessage('Saving Modell...')
        self.saveProject()
        self.statusbar.showMessage('Running Hydret06...')
        Popup = hydretPopup(self)
        Popup.show()
        Popup.process.start(self.h1drun[:-4]+'.bat')
        #Popup.process.waitForFinished()
        self.statusbar.showMessage('Reloading Model...')
        n_h1d = h1d(hydret_path = self.HydretEnv[0])
        self.initiate(hyd = n_h1d,i=self.knotenNr.currentIndex())
        self.statusbar.showMessage('Ready')
    
    def OpenEnv(self):
        #Zukunft run datei als h1d benannt
        if DOUBLECLICK_FILE:
            self.HydretEnv = (sys.argv[1],'')
        else:    
            self.HydretEnv = QFileDialog.getOpenFileName(caption='Hydret Projekt Öffnen (H1D/RUN File)',filter="*.run*;;*.h1d*")

        if self.HydretEnv[0] != '':
            
            #initiate model
            self.h1dmodel = h1d(hydret_path = self.HydretEnv[0])
            self.h1denv   = os.path.dirname(self.HydretEnv[0])
            self.h1drun   = self.HydretEnv[0].split('/')[-1]
            self.statusbar.showMessage('Loading Modell...')
            self.initiate(hyd = self.h1dmodel)
        
    def initiate(self,hyd,i=0):
        self.h1d = hyd
        self.savep.setEnabled(True)
        self.saveasp.setEnabled(True)
        self.run.setEnabled(True)
        self.loadwsp_2.setEnabled(True)
        self.node_renumber.setEnabled(True)
        self.node_delete.setEnabled(True)
        self.node_rau.setEnabled(True)
        self.Duplicateschnitts.setEnabled(True)
        
        #view box item update
        try:
            self.achse = hyd.achse
            self.lp_view_box(hyd.achse.split("\\")[-1])
        except:
            pass

        #initiate HYD 
        load_hyd(myapp)
        '''hyd checks'''
        lead_ = (60./self.h1d.dt)*self.h1d.toth+1
        if not self.h1d.lead == lead_:
            self.h1d.lead == lead_
            self.statusbar.showMessage('Anzahl der Ganglinienstützstellen ersetz!')
            self.p_lead.setText(str(lead_))
        
        nl_  = len(self.h1d.df_start.index) -1
        if not self.h1d.nl == -1*nl_:
            self.h1d.nl = -1*nl_
            self.statusbar.showMessage('Anzahl der Gewässerabschnitte ersetz!')
            self.p_nl.setText(str(-1*nl_))
        
        #set querprofil view active
        self.df_pro   = hyd.df_pro
        self.start_editing.setEnabled(True)
        self.qp_widget.setEnabled(True)
        self.copy_clip.setEnabled(True)

        #set lageplan view active
        self.df_start     = hyd.df_start
        self.mod_plan     = self.p_plan.text()
        self.df_wsp       = pd.DataFrame({self.mod_plan : self.df_start['HZERO']},
                                          index = self.df_start.index)
        self.wsp_view_dat = [(self.h1d.startdat,True,QColor(28,163,236,150),True)]
        self.wsp_dict     = [self.mod_plan]

        #update Profile
        self.knotenNr.blockSignals(True)
        self.station_label.blockSignals(True)
        self.schnittName_label.blockSignals(True)
        self.lang_ID.blockSignals(True)
        try:
            self.knotenNr.clear()
            self.station_label.clear()
            self.schnittName_label.clear()
            self.lang_ID.clear()
        except:
            pass
        self.pos_ = self.df_pro.index>0
        self.knotenNr.addItems(self.df_pro[self.pos_].index.astype(str))
        self.station_label.addItems(self.df_pro[self.pos_]['Station'].astype(str))
        self.schnittName_label.addItems(self.df_pro[self.pos_]['PName'].astype(str))
        
        #Default Node to show
        self.knotenNr.setCurrentIndex(i)
        self.station_label.setCurrentIndex(i)
        self.schnittName_label.setCurrentIndex(i)

        self.loc    = int(self.knotenNr.currentText())
        self.iloc   = (self.df_pro.index.tolist()).index(self.loc)
        self.Node   = self.df_pro.loc[self.loc]
        self.knotenNr.blockSignals(True)
        self.station_label.blockSignals(True)
        self.schnittName_label.blockSignals(True)
        
        
        #update Längschnitt
        self.lang_ID.addItems(np.unique(self.df_start['ID'].values).astype(str))
        self.gewid_current = int(self.df_start.loc[self.df_pro.index[self.iloc]]['ID'])
        self.lang_ID.setCurrentText(_translate("MainWindow", str(self.gewid_current)))
        self.lang_ID.blockSignals(False)
        self.idChange(i=i)
        
        #update start
        try:
            self.v4_leg.scene().removeItem(self.v4_leg)
            self.v4_leg = self.nodeView.addLegend()
        except:
            self.v4_leg = self.nodeView.addLegend()
        try:
            nodePlot(myapp)
            self.node_label = pg.TextItem(color='k',border='k',fill='w')
            self.nodeView.addItem(self.node_label)
            self.node_label.setPos(self.df_start.iloc[0]['X'],self.df_start.iloc[0]['Y'])
            self.vLine_node = pg.InfiniteLine(angle=90, movable=False)
            self.vLine_node.setPos(self.df_start.iloc[0]['X'])
            self.hLine_node = pg.InfiniteLine(angle=0,  movable=False)
            self.hLine_node.setPos(self.df_start.iloc[0]['Y'])
            self.nodeView.addItem(self.vLine_node, ignoreBounds=False)
            self.nodeView.addItem(self.hLine_node, ignoreBounds=False)
            self.node_label.setZValue(10000)
            self.vLine_node.setZValue(9999)
            self.hLine_node.setZValue(9999)
        except:
            pass
        
        #update langschnitt
        try:        
            try:
                self.v3_leg.scene().removeItem(self.v3_leg)
                self.v3_leg = self.langView.addLegend()
            except:
                self.v3_leg = self.langView.addLegend()
            self = langPlot(myapp,self.gewid_current)
            self.lang_label = pg.TextItem(color='k',border='k',fill='w')
            self.vLine_lang = pg.InfiniteLine(angle=90, movable=False)
            self.hLine_lang = pg.InfiniteLine(angle=0,  movable=False)
        except:
            pass
        
        self.statusbar.showMessage('Ready')
    
    def gewidChanged(self,i):
        self.gewid_current = int(i)
        try:
            self.v3_leg.scene().removeItem(self.v3_leg)
            self.v3_leg = self.langView.addLegend()
        except:
            self.v3_leg = self.langView.addLegend()
        self = langPlot(myapp,self.gewid_current)
        self.lang_label = pg.TextItem(color='k',border='k',fill='w')
        self.vLine_lang = pg.InfiniteLine(angle=90, movable=False)
        self.hLine_lang = pg.InfiniteLine(angle=0,  movable=False)
        
    def table_toggle(self):
        self.idChange(i=self.knotenNr.currentIndex())
    
    def updateViews(self):
        try:
            self.p2.setGeometry(self.graphicsView.getViewBox().sceneBoundingRect())
            self.p2.linkedViewChanged(self.graphicsView.getViewBox(),self.p2.XAxis)
        except: pass
    
    def idChange(self,i):
        '''
        i = index of combo box (0,1,2...n)
        '''
        #blocking signals to prevent infinite loops
        self.knotenNr.blockSignals(True)
        self.station_label.blockSignals(True)
        self.schnittName_label.blockSignals(True)
        
        self.knotenNr.setCurrentIndex(i)
        #set labels from .pro file
        self.loc   = int(self.knotenNr.currentText())
        self.iloc  = (self.df_pro.index.tolist()).index(self.loc)
        
        #update editing info
        self = update_labels(myapp)
        self.edit_knoten.clear()
        
        if self.Edit:
            df   = self.df_copy
            self.df_s = self.df_start_copy
        else:
            df   = self.df_pro
            self.df_s = self.df_start
            
        self.Node = df.loc[self.loc]
        
        #enabling radio buttons
        if -1*self.loc in df.index:
            _iloc = np.argwhere(df.index==-1*self.loc)[0]
            for il in _iloc:
                if df.iloc[il].Mode == 'H2':
                    self.iloc_r = il
                    try:
                        del self.p2
                    except:
                        pass
                    self.p2 = pg.ViewBox()
                    self.Node_R = df.iloc[il]
                    self._rrau.setEnabled(True)
            for il in _iloc:
                if df.iloc[il].Mode == 'ZS':
                    self.iloc_s = il
                    self.Node_S = df.iloc[il]
                    self._rschalter.setEnabled(True)
        else:
            self._rquer.setCheckedState = 2
            self._rrau.setEnabled(False)
            self._rschalter.setEnabled(False)
            
        #graphicView 2 content
        try:
            self.loc2  = int(self.knotenNr.itemText(i+1))
            self.iloc2 = (df.index.tolist()).index(self.loc2)
        except:
            self.loc2  = int(self.knotenNr.itemText(i-1))
            self.iloc2 = (df.index.tolist()).index(self.loc2)
        
        self.Node2 = df.loc[self.loc2]
        
        #lcd
        self.Punkte_label.display(self.Node['Npoints'])
        
        try:
            self.modus_label.setCurrentIndex(self.qschnp.index(self.Node['Mode']))
        except:
            self.modus_label.setCurrentIndex(self.qschnp.index(self.h1d.xsecmo))
            self.Node['Mode'] = self.h1d.xsecmo
        
        if self.Node['CTAB'] == 1:
            self.ctab_label.setCurrentIndex(1)
        
        elif self.Node['CTAB'] == 0:
            self.ctab_label.setCurrentIndex(0)
        
        self.maxHeight_label.setText(str(self.Node['Max Height']))
        self.station_label.setCurrentIndex(i)
        self.schnittName_label.setCurrentIndex(i)
        
        #set labels from start file
        load_start(myapp)

        #set coords from .pro
        self.coords_table.blockSignals(True)
        
        try:
            self.coords_table.clear()
        except:
            pass
        
        #radio buttons check
        if self._rquer.isChecked():
            self.coords_table.setRowCount(self.Node['Npoints'])
            self.coords_table.setHorizontalHeaderLabels(("X","Y"))
            for table_i in range(self.Node['Npoints']):
                self.coords_table.setItem(table_i,0,QTableWidgetItem(str(self.Node['X'][table_i])))
                self.coords_table.setItem(table_i,1,QTableWidgetItem(str(self.Node['Y'][table_i])))
            #rivbed color
            '''River Bed'''
            riv_bed_y,riv_bed_idx,riv_bed_x = riv_bed(self.Node)
            
            rfont = QFont()
            rfont.setBold(True)
            self.coords_table.item(riv_bed_idx,0).setForeground(QColor(255,99,71))
            self.coords_table.item(riv_bed_idx,1).setForeground(QColor(255,99,71))
            self.coords_table.item(riv_bed_idx,0).setFont(rfont)
            self.coords_table.item(riv_bed_idx,1).setFont(rfont)
            self.coords_table.scrollToItem(self.coords_table.item(riv_bed_idx,1),QAbstractItemView.PositionAtCenter)
            self.coords_table.selectRow(riv_bed_idx)
        
        elif self._rrau.isChecked():
            self.coords_table.setRowCount(self.Node_R['Npoints'])
            self.coords_table.setHorizontalHeaderLabels(("X","Strickler"))
            for table_i in range(self.Node_R['Npoints']):
                self.coords_table.setItem(table_i,0,QTableWidgetItem(str(self.Node_R['X'][table_i])))
                self.coords_table.setItem(table_i,1,QTableWidgetItem(str(self.Node_R['Y'][table_i])))

        elif self._rschalter.isChecked():
            self.coords_table.setRowCount(self.Node_S['Npoints'])
            self.coords_table.setHorizontalHeaderLabels(("X","Y"))
            for table_i in range(self.Node_S['Npoints']):
                self.coords_table.setItem(table_i,0,QTableWidgetItem(str(self.Node_S['X'][table_i])))
                self.coords_table.setItem(table_i,1,QTableWidgetItem(str(self.Node_S['Y'][table_i])))
        
        try:
            self.v1_leg.scene().removeItem(self.v1_leg)
            self.v1_leg = self.graphicsView.addLegend()
        except:
            self.v1_leg = self.graphicsView.addLegend()
        try:
            self.v2_leg.scene().removeItem(self.v2_leg)
            self.v2_leg = self.graphicsView2.addLegend()
        except:
            self.v2_leg = self.graphicsView2.addLegend()
        plot(myapp,self.Node2,self.graphicsView2)
        plot(myapp,self.Node,self.graphicsView)
        nodechange_inPRO(myapp)
        try:
            self.v3_leg.scene().removeItem(self.v3_leg)
            self.v3_leg = self.langView.addLegend()
        except:
            self.v3_leg = self.langView.addLegend()
        wsp_df_update(myapp)
        
        #Querschnitt labels on movement
        try:
            self.quer_label =pg.TextItem(color='k',border='k',fill='w')
            self.graphicsView.addItem(self.quer_label)
            self.quer_label.setPos(self.df_pro.loc[self.loc]['X'][0],self.df_pro.loc[self.loc]['Y'][0])
            self.vLine_quer = pg.InfiniteLine(angle=90, movable=False)
            self.vLine_quer.setPos(self.df_pro.loc[self.loc]['X'][0])
            self.hLine_quer = pg.InfiniteLine(angle=0,  movable=False)
            self.hLine_quer.setPos(self.df_pro.loc[self.loc]['Y'][0])
            self.graphicsView.addItem(self.vLine_quer, ignoreBounds=False)
            self.graphicsView.addItem(self.hLine_quer, ignoreBounds=False)
            self.quer_label.setZValue(10000)
            self.vLine_quer.setZValue(9999)
            self.hLine_quer.setZValue(9999)
        except:
            pass
        try:
            self.quer2_label =pg.TextItem(color='k',border='k',fill='w')
            self.graphicsView2.addItem(self.quer2_label)
            self.quer2_label.setPos(self.df_pro.loc[self.loc2]['X'][0],self.df_pro.loc[self.loc2]['Y'][0])
            self.vLine_quer2 = pg.InfiniteLine(angle=90, movable=False)
            self.vLine_quer2.setPos(self.df_pro.loc[self.loc2]['X'][0])
            self.hLine_quer2 = pg.InfiniteLine(angle=0,  movable=False)
            self.hLine_quer2.setPos(self.df_pro.loc[self.loc2]['Y'][0])
            self.graphicsView2.addItem(self.vLine_quer2, ignoreBounds=False)
            self.graphicsView2.addItem(self.hLine_quer2, ignoreBounds=False)
            self.quer2_label.setZValue(10000)
            self.vLine_quer2.setZValue(9999)
            self.hLine_quer2.setZValue(9999)
        except:
            pass
        self.coords_table.blockSignals(False)
        self.knotenNr.blockSignals(False)
        self.station_label.blockSignals(False)
        self.schnittName_label.blockSignals(False)

    def initiateEditing(self):
        self.Edit = True
        self.graphicsView.setLabel('left',"EDITING MODE")
        self.stop_editing.setEnabled(True)
        self.start_editing.setEnabled(False)
        
        self.coords_table.setEditTriggers(QAbstractItemView.DoubleClicked)
        self.edit_knoten.setEnabled(True)
        self.edit_station.setEnabled(True)
        self.edit_pname.setEnabled(True)
        self.paste_clip.setEnabled(True)
        
        self.ctab_label.setEnabled(True)
        self.modus_label.setEnabled(True)
        self.maxHeight_label.setEnabled(True)
        
        self.geo_info.setEnabled(True)
        if self.p_ovfbil_2.currentIndex() == 0:
            self.ovf_modbox.setEnabled(False)
        else:
            self.ovf_modbox.setEnabled(True)
        self.wsp_rau_box.setEnabled(True)
        self.plot_editor_box.setEnabled(True)
        
        self.df_copy = self.df_pro.copy()
        self.df_start_copy = self.df_start.copy()
        db_ = self.df_copy.copy()
        self.df_db   = [db_]
        self.idChange(i=self.knotenNr.currentIndex())

    def FinishEditing(self):
        self.coords_table.setEditTriggers(QAbstractItemView.NoEditTriggers)
        STATE_EDIT = update_labels(myapp)
        self.Edit = False
        self = update_labels(myapp)
        self.npoints_delete.display(0)
        self.graphicsView.showLabel('left',show=False)
        saveEdits = QMessageBox.question(self,'Editor',"Save Edits?", QMessageBox.Yes | QMessageBox.No, QMessageBox.No)
        
        if saveEdits == QMessageBox.Yes:
            self = STATE_EDIT
            self.df_pro.update(self.df_copy)
            self.df_start.update(self.df_start_copy)
            self.knotenNr.blockSignals(True)
            self.knotenNr.clear()
            self.pos_ = self.df_pro.index>0
            self.knotenNr.addItems(self.df_pro[self.pos_].index.astype(str))
            self.station_label.addItems(self.df_pro[self.pos_]['Station'].astype(str))
            self.schnittName_label.addItems(self.df_pro[self.pos_]['PName'].astype(str))
            self.knotenNr.blockSignals(False)
        else:
            pass
        
        del self.df_copy
        del self.df_db

        self.stop_editing.setEnabled(False)
        self.start_editing.setEnabled(True)
        self.edit_knoten.setEnabled(False)
        self.edit_station.setEnabled(False)
        self.edit_pname.setEnabled(False)
        self.paste_clip.setEnabled(False)
        
        self.ctab_label.setEnabled(False)
        self.modus_label.setEnabled(False)
        self.maxHeight_label.setEnabled(False)
        
        self.geo_info.setEnabled(False)
        self.wsp_rau_box.setEnabled(False)
        self.plot_editor_box.setEnabled(False)
        self.idChange(i=self.knotenNr.currentIndex())
    
    def saveProject(self):
        self.statusbar.showMessage('Saving PRO...')
        writePRO(self.h1d.propath,self.df_pro)
        self.statusbar.showMessage('Saving Start...')
        writeStart(self.h1d.startpath,self.df_start)
        self.statusbar.showMessage('Saving HYD...')
        self.h1d = updateHyd(myapp)
        writeHYD(self.h1d.hyd_p,self.h1d)
        self.statusbar.showMessage('Saving RUN...')
        writeRUN(self.h1drun[:-4]+'.run',self.h1d)
        self.statusbar.showMessage('Ready')

    def undo_but(self):
        self.changes -=1
        self.df_copy.update(self.df_db[self.changes])
        
        self.idChange(i=self.knotenNr.currentIndex())
        
        if self.changes == 0:
            self.undo.setEnabled(False)
        self.redo.setEnabled(True)
    
    def redo_but(self):
        self.changes +=1
        self.df_copy.update(self.df_db[self.changes])
        self.idChange(i=self.knotenNr.currentIndex())
        
        if self.changes == len(self.df_db)-1:
            self.redo.setEnabled(False)
        if self.changes > 0:
            self.undo.setEnabled(True)
            
    def plot_update_coords(self):
        if self.Edit:
            self.changes +=1
            x,y = [],[]
            for i in range(self.coords_table.rowCount()):
                x.append(float(self.coords_table.item(i,0).text()))
                y.append(float(self.coords_table.item(i,1).text()))
            
            if self._rquer.isChecked():
                self.df_copy.iat[self.iloc,6] = np.array(x)
                self.df_copy.iat[self.iloc,7] = np.array(y)
                self.df_db.append(self.df_copy)
            elif self._rrau.isChecked():
                self.df_copy.iat[self.iloc_r,6] = np.array(x)
                self.df_copy.iat[self.iloc_r,7] = np.array(y)
                self.df_db.append(self.df_copy)
            elif self._rschalter.isChecked():
                self.df_copy.iat[self.iloc_s,6] = np.array(x)
                self.df_copy.iat[self.iloc_s,7] = np.array(y)
                self.df_db.append(self.df_copy)
            self.undo.setEnabled(True)
            self.idChange(i = self.knotenNr.currentIndex())
        
class connections:
    
    def connect_inflowNodes():
        inflowNodes(myapp)
    
    def connect_printCurves():
        printCurves(myapp)
        
    def connect_defJunctions():
        defJunctions(myapp)
    
    def connect_defWeirs():
        defWeirs(myapp)
        
    def connect_defGates():
        defGates(myapp)
    
    def connect_lateralInflows():
        lateralInflows(myapp)

    def connect_ovfmode():
        ovfmode(myapp)

    def connect_knoten_label():
        knoten_label(myapp)
        
    def connect_edit_modus():
        edit_modus(myapp)
        
    def connect_edit_maxHeight():
        edit_maxHeight(myapp)
        
    def connect_delete_coords():
        delete_coords(myapp)
        
    def connect_insert_coords():
        insert_coords(myapp)
        
#    def connect_undo_but():
#        undo_but(myapp)
#        
#    def connect_redo_but():
#        redo_but(myapp)

    def connect_handlecopy():
        _handlecopy(myapp)
        
    def connect_handlepaste():
        _handlepaste(myapp)
        
    def connect_changeAR():
        changeAR(myapp)
        
    def connect_plan_name():
        plan_name(myapp)

    def connect_loadresult():
        try:
            myapp.v3_leg.scene().removeItem(myapp.v3_leg)
            myapp.v3_leg = myapp.langView.addLegend()
        except:
            myapp.v3_leg = myapp.langView.addLegend()
        loadresult(myapp)
        
    def connect_update_wsp():
        try:
            myapp.v1_leg.scene().removeItem(myapp.v1_leg)
            myapp.v1_leg = myapp.graphicsView.addLegend()
        except:
            myapp.v1_leg = myapp.graphicsView.addLegend()
        try:
            myapp.v2_leg.scene().removeItem(myapp.v2_leg)
            myapp.v2_leg = myapp.graphicsView2.addLegend()
        except:
            myapp.v2_leg = myapp.graphicsView2.addLegend()
        try:
            myapp.v3_leg.scene().removeItem(myapp.v3_leg)
            myapp.v3_leg = myapp.langView.addLegend()
        except:
            myapp.v3_leg = myapp.langView.addLegend()
        update_wsp(myapp)
    
    def connect_onXYselection():
        onXYselection(myapp)
    
    def connect_colorpicker():
        colorpicker(myapp)

    def connect_dataadd():
        dataadd(myapp)

    def connect_dataremove():
        dataremove(myapp)
        
class MouseMovements:
          
    def mouseMoved_lang(evt):
        try:
            mousePoint = myapp.langView.getViewBox().mapSceneToView(evt)
            xi = mousePoint.x()
            #yi = mousePoint.y()
            gid = myapp.gewid_current
            gdx = (myapp.df_start['ID'] == gid)
            val = min(myapp.df_start['XL'][gdx], key=lambda x: abs(x - xi))
            idx = myapp.df_start[gdx][myapp.df_start['XL'][gdx]==val].index[0]
            
            sohle = myapp.df_start.loc[idx]['ZO']
            try:
                t = "{:>} {:>}\n".format('Schnitt:',myapp.df_pro.loc[idx]['PName'])
            except:
                t = '-\n'
            text  = t+"{:>} {:>0.2f}\n{:>} {:>}\n{:>} {:>0.2f}".format('Station:',val,'Knoten:',idx,'Sohle:  ',sohle)
            
            for i in myapp.df_wsp.columns:
                t = "\n{:>} {:>0.2f}".format(i+':',myapp.df_wsp.loc[idx][i])
                text = text+t

            myapp.langView.addItem(myapp.lang_label)
            myapp.langView.addItem(myapp.vLine_lang, ignoreBounds=False)
            myapp.langView.addItem(myapp.hLine_lang, ignoreBounds=False)
            myapp.lang_label.setTextWidth(150)
            myapp.lang_label.setZValue(10000)
            myapp.vLine_lang.setZValue(9999)
            myapp.hLine_lang.setZValue(9999)
            myapp.lang_label.setText(text)
            myapp.lang_label.setPos(val,sohle)
            myapp.vLine_lang.setPos(val)
            myapp.hLine_lang.setPos(sohle)
        except:
            pass

    def mouseMoved_node(evt):
        try:
            mousePoint = myapp.nodeView.getViewBox().mapSceneToView(evt)
            xi = mousePoint.x()
            yi = mousePoint.y()
            
            pt    = np.array([xi,yi])
            nodes = np.array(list(zip(myapp.df_start['X'].values,myapp.df_start['Y'].values)))
            dist  = np.linalg.norm(nodes - pt, ord=2, axis=1)
            
            nearest = sorted(list(zip(dist,myapp.df_start.index)))[0]
            try:
                sname = myapp.df_pro.loc[nearest[1]]['PName']
            except:
                sname = '--'
            
            if nearest[0] < 500:
                myapp.node_label.setText('Node: '+ str(nearest[1])+'\nStation: '+str(myapp.df_start.loc[nearest[1]]['XL'])+
                                         '\nSchnitt: '+sname)
                myapp.node_label.setPos(myapp.df_start.loc[nearest[1]]['X'],myapp.df_start.loc[nearest[1]]['Y'])
                
            myapp.vLine_node.setPos(myapp.df_start.loc[nearest[1]]['X'])
            myapp.hLine_node.setPos(myapp.df_start.loc[nearest[1]]['Y'])
        except:
            pass

    def mouseMoved_quer(evt):
        try:
            mousePoint = myapp.graphicsView.getViewBox().mapSceneToView(evt)
            xi = mousePoint.x()
            yi = mousePoint.y()
            
            pt    = np.array([xi,yi])
            nodes = np.array(list(zip(myapp.df_pro.loc[myapp.loc]['X'],myapp.df_pro.loc[myapp.loc]['Y'])))
            dist  = np.linalg.norm(nodes - pt, ord=2, axis=1)
            nearest = sorted(list(zip(dist,range(len(dist)))))[0]
            
            if nearest[0] < 500:
                myapp.quer_label.setText("x: {:0.2f}, y: {:0.2f}".format(myapp.df_pro.loc[myapp.loc]['X'][nearest[1]],
                                         myapp.df_pro.loc[myapp.loc]['Y'][nearest[1]]))
                myapp.quer_label.setPos(myapp.df_pro.loc[myapp.loc]['X'][nearest[1]],
                                         myapp.df_pro.loc[myapp.loc]['Y'][nearest[1]])
                
            myapp.vLine_quer.setPos(myapp.df_pro.loc[myapp.loc]['X'][nearest[1]])
            myapp.hLine_quer.setPos(myapp.df_pro.loc[myapp.loc]['Y'][nearest[1]])
        except:
            pass
        
    def mouseMoved_quer2(evt):
        try:
            mousePoint = myapp.graphicsView2.getViewBox().mapSceneToView(evt)
            xi = mousePoint.x()
            yi = mousePoint.y()
            
            pt    = np.array([xi,yi])
            nodes = np.array(list(zip(myapp.df_pro.loc[myapp.loc2]['X'],myapp.df_pro.loc[myapp.loc2]['Y'])))
            dist  = np.linalg.norm(nodes - pt, ord=2, axis=1)
            nearest = sorted(list(zip(dist,range(len(dist)))))[0]
            
            if nearest[0] < 500:
                myapp.quer2_label.setText("x: {:0.2f}, y: {:0.2f}".format(myapp.df_pro.loc[myapp.loc2]['X'][nearest[1]],
                                         myapp.df_pro.loc[myapp.loc2]['Y'][nearest[1]]))
                myapp.quer2_label.setPos(myapp.df_pro.loc[myapp.loc2]['X'][nearest[1]],
                                         myapp.df_pro.loc[myapp.loc2]['Y'][nearest[1]])
                
            myapp.vLine_quer2.setPos(myapp.df_pro.loc[myapp.loc2]['X'][nearest[1]])
            myapp.hLine_quer2.setPos(myapp.df_pro.loc[myapp.loc2]['Y'][nearest[1]])
        except:
            pass
        
if __name__ == '__main__':
    appctxt = ApplicationContext()
    myapp = MainW()
    myapp.show()
    try:
        if sys.argv[1].lower().endswith('.run'):
            try:
                DOUBLECLICK_FILE = True
                myapp.OpenEnv()
            except:
                DOUBLECLICK_FILE = False
                myapp.statusbar.showMessage('Invalid File!')
    except:
        pass
    exit_code = appctxt.app.exec_()
    sys.exit(exit_code)