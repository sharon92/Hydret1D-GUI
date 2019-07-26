# -*- coding: utf-8 -*-
from fbs_runtime.application_context.PyQt5 import ApplicationContext

'''import system modules'''
import sys
import os
import numpy             as np
import pandas            as pd

'''import modules to read hydret'''
from modules.rawh1d      import HYDRET as h1d
from modules.rawh1d      import writePRO,writeStart,writeHYD,writeRUN
from modules.editsection import update_labels
from modules.loadhyd     import load_hyd,updateHyd
from modules.loadgeo     import load_start,load_qinfo
from modules.plotting    import (qplots,uqplots,
                                 plotROI,
                                 wsp_df_update,
                                 nodePlot,
                                 langPlot,
                                 nodeMarker)

'''import pyqt5 modules'''
import pyqtgraph          as     pg

from PyQt5.QtWidgets      import (QMainWindow,
                                  QMessageBox,
                                  QFileDialog,
                                  QAbstractItemView)
from PyQt5.QtCore         import QCoreApplication,QEvent
from PyQt5.QtGui          import QColor
from ui.interface         import initiateBeautify,connections
from ui.hydretUI          import Ui_MainWindow

pg.setConfigOption('background', 'w')
pg.setConfigOption('foreground', 'k')
_translate = QCoreApplication.translate

SCRIPT_DIR = os.path.dirname(os.path.realpath(sys.argv[0]))

global DOUBLECLICK_FILE
DOUBLECLICK_FILE = False

__version__ = '1.1.0'
        
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
        
        #Docks
        self.tabifyDockWidget(self.qp_dock,self.ls_dock)
        self.tabifyDockWidget(self.ls_dock,self.lp_dock)
        
        self.qp_view.triggered.connect(self.qdockv)
        self.lp_view.triggered.connect(self.lpdockv)
        self.ls_view.triggered.connect(self.lsdockv)
        
        qplotD =     {
                      'view'   : [self.graphicsView,self.graphicsView2],
                      'node'    : [None,None],
                      'rnode'   : [None,None],
                      'riloc'   : [None,None],
                      'snode'   : [None,None],
                      'siloc'   : [None,None],
                      'axis'    : [None,None],
                      'raxis'   : [None,None],
                      'rvbox'   : [None,None],
                      'annotate': [None,None],
                      'riverbed': [None,None],
                      'plotbot' : [None,None],
                      'sohle'   : [None,None],
                      'rbank'   : [None,None],
                      'wbank'   : [None,None],
                      'pointer' : [None,None]
                          }
        
        self.qplotD = pd.DataFrame(qplotD,dtype=object,index=[0,1])

        #make connections
        connections(self)
        
        #Beautify GUI
        initiateBeautify(self)
    
    def qdockv(self):
        
        if not self.qp_dock.isVisible():
            self.qp_dock.setVisible(True)
            
    def lpdockv(self):
        
        if not self.lp_dock.isVisible():
            self.lp_dock.setVisible(True)
        self.lp_dock.show()
            
    def lsdockv(self):
        
        if not self.ls_dock.isVisible():
            self.ls_dock.setVisible(True)
        self.ls_dock.show()
            
    def OpenEnv(self):
        #Zukunft run datei als h1d benannt
        if DOUBLECLICK_FILE:
            self.HydretEnv = (sys.argv[1],'')
        else:    
            self.HydretEnv = QFileDialog.getOpenFileName(caption='Hydret Projekt Öffnen (H1D/RUN File)',filter="*.run*;;*.h1d*")

        if self.HydretEnv[0] != '':
            
            #initiate model
            self.h1dmodel = h1d(hydret_path = self.HydretEnv[0])
            self.h1denv   = os.path.abspath(os.path.dirname(self.HydretEnv[0]))
            self.h1drun   = os.path.abspath(self.HydretEnv[0]).split('\\')[-1]
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
        self.n_starttab.setEnabled(True)
        self.node_gen.setEnabled(True)

        #view box item update
        try:
            self.achse = hyd.achse
            self.lp_view_box(hyd.achse.split("\\")[-1])
        except:
            pass

        #initiate HYD 
        load_hyd(self)
        '''hyd checks'''
        lead_ = (60./self.h1d.tinc)*self.h1d.toth+1
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
            nodePlot(self)
            self.node_label = pg.TextItem(color='k',border='k',fill='w')
            self.vLine_node = pg.InfiniteLine(angle=90, movable=False)
            self.hLine_node = pg.InfiniteLine(angle=0,  movable=False)
            nodeMarker(self,self.df_s)
        except:
            pass
        
        #update langschnitt
        try:        
            langPlot(self)
            self.lang_label = pg.TextItem(color='k',border='k',fill='w')
            self.vLine_lang = pg.InfiniteLine(angle=90, movable=False)
            self.hLine_lang = pg.InfiniteLine(angle=0,  movable=False)
        except:
            pass
        
        self.statusbar.showMessage('Ready')
        
    def toggle(self):
        self.idChange(i=self.knotenNr.currentIndex())
        
    def idChange(self,i):
        '''
        i = index of combo box (0,1,2...n)
        '''
        #blocking signals to prevent infinite loops
        self.knotenNr.blockSignals(True)
        self.station_label.blockSignals(True)
        self.schnittName_label.blockSignals(True)
        self.changes = 0
        self.knotenNr.setCurrentIndex(i)

        if self.Edit:
            if self.undo.isEnabled() or self.redo.isEnabled():
                ask = QMessageBox.question(self,'Editor',"Save Edits on the last node?",
                                                 QMessageBox.Yes | QMessageBox.No, QMessageBox.No)
                if ask == QMessageBox.No:
                    self.df_copy = self.df_db[0].copy()
                else:
                    self.df_copy = self.df_db[-1].copy()
            df = self.df_copy.copy()
            del self.df_db
            self.undo.setEnabled(False)
            self.redo.setEnabled(False)
            self.df_s = self.df_start_copy
            db_ = df.copy()
            self.df_db   = [db_]
            #update editing info
            self = update_labels(self)
            self.edit_knoten.clear()
        else:
            df   = self.df_pro.copy()
            self.df_s = self.df_start
            
        #Plot q1 Node
        self.loc   = int(self.knotenNr.currentText())
        self.iloc  = (self.df_pro.index.tolist()).index(self.loc)
        self.Node = df.loc[self.loc]
        
        #Plot q2 Node
        try:
            self.loc2  = int(self.knotenNr.itemText(i+1))
            self.iloc2 = (df.index.tolist()).index(self.loc2)
        except:
            self.loc2  = int(self.knotenNr.itemText(i-1))
            self.iloc2 = (df.index.tolist()).index(self.loc2)
        self.Node2 = df.loc[self.loc2]
        self.qplotD['node'][0] = self.Node
        self.qplotD['node'][1] = self.Node2
        
        #unmark highlighted nodes if any
        self.graphicsView.scene().sigMouseClicked.emit(QEvent.MouseButtonPress)
        
        #set display information for Querschnitts
        load_qinfo(self,df,i)

        #set labels from start file
        load_start(self)

        #try updating the existing plot
        if not ((self.qplotD['axis'][0] is None) or (self.qplotD['axis'][1] is None)):
            uqplots(self)
        else:
            qplots(self)
            wsp_df_update (self)

        if self.Edit:
            self.graphicsView.addItem(self.editable_schnitt)
            self.editable_schnitt.blockSignals(True)
            plotROI(self)
            self.editable_schnitt.blockSignals(False)
            
        nodeMarker(self,self.df_s)
        
        self.knotenNr.blockSignals(False)
        self.station_label.blockSignals(False)
        self.schnittName_label.blockSignals(False)

    def initiateEditing(self):
        self.tabifyDockWidget(self.lp_dock,self.qp_dock)
        self.Edit = True
        self.graphicsView.setLabel('left',"EDITING MODE")
        self.stop_editing.setEnabled(True)
        self.start_editing.setEnabled(False)
        self.delete_rows.setEnabled(True)
        
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
        self.graphicsView.scene().sigMouseClicked.emit(QEvent.MouseButtonPress)
        self.editable_schnitt.blockSignals(True)
        self.editable_schnitt.clearPoints()
        self.editable_schnitt.blockSignals(False)
        self.stop_editing.setEnabled(False)
        self.start_editing.setEnabled(True)
        self.undo.setEnabled(False)
        self.redo.setEnabled(False)
        self.delete_rows.setEnabled(False)
        self.edit_knoten.setEnabled(False)
        self.edit_station.setEnabled(False)
        self.edit_pname.setEnabled(False)
        self.paste_clip.setEnabled(False)
        
        self.ctab_label.setEnabled(False)
        self.modus_label.setEnabled(False)
        self.maxHeight_label.setEnabled(False)
        
        self.geo_info.setEnabled(False)
        self.wsp_rau_box.setEnabled(False)
        self.idChange(i=self.knotenNr.currentIndex())
    
    def saveProject(self):
        updateHyd(self)
        self.statusbar.showMessage('Saving PRO...')
        writePRO(self.h1d.prodat,self.df_pro)
        self.statusbar.showMessage('Saving Start...')
        writeStart(self.h1d.startpath,self.df_start,self.h1d._dform)
        self.statusbar.showMessage('Saving HYD...')
        writeHYD(self.h1d.hyd_p,self.h1d)
        self.statusbar.showMessage('Saving RUN...')
        writeRUN(self.h1drun,self.h1d)
        self.statusbar.showMessage('Ready')

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