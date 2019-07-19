# -*- coding: utf-8 -*-
from fbs_runtime.application_context.PyQt5 import ApplicationContext
'''import system modules'''
import sys
import os

from functools import partial
import numpy as np
import pandas as pd

'''import modules to read hydret'''
from modules.rawh1d      import HYDRET as h1d
from modules.rawh1d      import writePRO,writeStart,writeHYD,writeRUN
from modules.loaddata    import dataadd,dataremove
from modules.editsection import (update_labels,
                                 knoten_label,
                                 edit_modus,
                                 edit_maxHeight,
                                 _handlecopy,
                                 _handlepaste)
from modules.loadhyd     import (load_hyd,
                                 inflowNodes,
                                 printCurves,
                                 idown_change,
                                 normq,
                                 defWeirs,
                                 defGates,
                                 defJunctions,
                                 lateralInflows,
                                 ovfmode,
                                 updateHyd)
from modules.loadgeo     import load_start
from modules.riverbed    import riv_bed
from modules.plotting    import (qplot1,qplot2,
                                 uqplot1,uqplot2,
                                 plotROI,updateViews,
                                 update_schnitt,plot_update_coords,
                                 wsp_df_update,
                                 nodePlot,
                                 langPlot,ulangPlot,
                                 changeAR,
                                 plan_name,
                                 loadresult,update_wsp,
                                 undo_but,redo_but,
                                 nodeMarker,gewUnmark,
                                 xyMarker,xyUnmark,
                                 colorpicker)

'''import pyqt5 modules'''
import pyqtgraph as pg
from PyQt5.QtGui         import QFont,QColor
from PyQt5.QtWidgets     import (QMainWindow,
                                 QMessageBox,
                                 QFileDialog,
                                 QAbstractItemView,
                                 QTableWidgetItem)
from PyQt5.QtCore        import (QCoreApplication)
#from PyQt5.Qt import PYQT_VERSION_STR
#print("PyQt version:", PYQT_VERSION_STR)
from ui.beautify          import initiateBeautify
from dialogs.nodeGen      import nodegenwinshow
from dialogs.nodeRenum    import renumber
from dialogs.nodeTable    import spreadwin
from dialogs.profLamellen import raumode
from dialogs.runHydret    import runModel

from ui.hydretUI         import Ui_MainWindow

pg.setConfigOption('background', 'w')
pg.setConfigOption('foreground', 'k')
_translate = QCoreApplication.translate

SCRIPT_DIR = os.path.dirname(os.path.realpath(sys.argv[0]))

global DOUBLECLICK_FILE
DOUBLECLICK_FILE = False
        
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
        self.loadwsp_2.triggered.connect(connections.connect_loadresult)
        #self.closef.triggered.connect(MainW.restoreState(STATE))
        #self.saveasp.triggered.connect(self.savePRO)
        
        #dialog windows
        self.node_renumber.triggered.connect(partial(renumber,self))
        self.node_rau.triggered.connect(partial(raumode,self))
        self.n_starttab.triggered.connect(partial(spreadwin,self))
        self.node_gen.triggered.connect(partial(nodegenwinshow,self))
        self.run.triggered.connect(partial(runModel,self))
        
        
        #Docks
        self.tabifyDockWidget(self.qp_dock,self.lp_dock)
        
        #hyd signals
        self.p_idown.currentIndexChanged.connect(partial(idown_change,self))
        self.p_nqin.valueChanged.connect(partial(inflowNodes,self))
        self.p_nwel.valueChanged.connect(partial(printCurves,self))
        self.qhtcount.valueChanged.connect(partial(normq,self))
        self.p_njunc.valueChanged.connect(partial(defJunctions,self))
        self.p_nweirs.valueChanged.connect(partial(defWeirs,self))
        self.p_ngates.valueChanged.connect(partial(defGates,self))
        self.p_latinf.valueChanged.connect(partial(lateralInflows,self))
        self.p_ovfbil_2.currentIndexChanged.connect(partial(ovfmode,self))
        
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
        self.lang_ID.currentTextChanged.connect(partial(ulangPlot,self))
        self.coords_table.itemSelectionChanged.connect(partial(xyMarker,self))
        self.coords_table.itemChanged.connect(partial(plot_update_coords,self))
        self.p_wspdat.itemChanged.connect(connections.connect_update_wsp)
        
        #loading data (shapefiles)
        self.lp_adddata.clicked.connect(connections.connect_dataadd)
        self.lp_removedata.clicked.connect(connections.connect_dataremove)
        
        #plotting
        self.ratio = 1
        self.AspectRatio.valueChanged.connect(partial(changeAR,self))
        self.p_plan.editingFinished.connect(partial(plan_name,self))
        self.p_wspdat.itemDoubleClicked.connect(partial(colorpicker,self))
        self.langView.scene().sigMouseMoved.connect(MouseMovements.mouseMoved_lang)
        self.nodeView.scene().sigMouseMoved.connect(MouseMovements.mouseMoved_node)
        self.nodeView.scene().sigMouseClicked.connect(partial(gewUnmark,self))
        self.graphicsView.scene().sigMouseClicked.connect(partial(xyUnmark,self))
        self.graphicsView.scene().sigMouseMoved.connect(MouseMovements.mouseMoved_quer)
        self.graphicsView2.scene().sigMouseMoved.connect(MouseMovements.mouseMoved_quer2)
        self.graphicsView.getViewBox().sigResized.connect(partial(updateViews,self))
        self.editable_schnitt = pg.PolyLineROI([],movable=False)
        self.editable_schnitt.setPen(pg.mkPen(color='y',width=3))
        self.editable_schnitt.sigRegionChanged.connect(partial(update_schnitt,self))
        self.editable_schnitt.sigRegionChangeFinished.connect(partial(plot_update_coords,self))
    
        #connect radio buttons
        self._rquer.toggled.connect(self.toggle)
        self._rrau.toggled.connect(self.toggle)
        self._rschalter.toggled.connect(self.toggle)
        
        #under editing mode
        self.undo.clicked.connect(partial(undo_but,self))
        self.redo.clicked.connect(partial(redo_but,self))
        self.edit_knoten.editingFinished.connect(partial(knoten_label,self))
        self.modus_label.activated[str].connect(partial(edit_modus,self))
        self.maxHeight_label.editingFinished.connect(partial(edit_maxHeight,self))
        self.copy_clip.clicked.connect(partial(_handlecopy,self))
        self.paste_clip.clicked.connect(partial(_handlepaste,self))
        
        #Beautify GUI
        initiateBeautify(self)
    
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
            nodePlot(self)
            self.nplot.scene().sigMouseClicked.connect(self.nodeViewCLICK)
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
        
        #set labels from .pro file
        self.loc   = int(self.knotenNr.currentText())
        self.iloc  = (self.df_pro.index.tolist()).index(self.loc)

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
            self._rquer.setChecked(True)
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
        load_start(self)

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
        
        #try updating the existing plot
        try:
            uqplot1(self)
            uqplot2(self)
        except:
            #if plot don't exist, create them
            qplot1(self)
            qplot2(self)
            wsp_df_update (self)

        if self.Edit:
            self.graphicsView.addItem(self.editable_schnitt)
            self.editable_schnitt.blockSignals(True)
            plotROI(self)
            self.editable_schnitt.blockSignals(False)
        nodeMarker(self)

        self.graphicsView.getViewBox().autoRange(items=[self.ax1,self.annotate1])
        self.coords_table.blockSignals(False)
        self.knotenNr.blockSignals(False)
        self.station_label.blockSignals(False)
        self.schnittName_label.blockSignals(False)

    def initiateEditing(self):
        self.tabifyDockWidget(self.lp_dock,self.qp_dock)
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
        self.idChange(i=self.knotenNr.currentIndex())
    
    def saveProject(self):
        self.statusbar.showMessage('Saving PRO...')
        writePRO(self.h1d.propath,self.df_pro)
        self.statusbar.showMessage('Saving Start...')
        writeStart(self.h1d.startpath,self.df_start,self.h1d._dform)
        self.statusbar.showMessage('Saving HYD...')
        updateHyd(self)
        writeHYD(self.h1d.hyd_p,self.h1d)
        self.statusbar.showMessage('Saving RUN...')
        writeRUN(self.h1drun,self.h1d)
        self.statusbar.showMessage('Ready')

class connections:

    def connect_loadresult():
        loadresult(myapp)
        
    def connect_update_wsp():
        update_wsp(myapp)

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
            nx    = np.array([float(myapp.coords_table.item(_x,0).text()) for _x in range(myapp.coords_table.rowCount())])
            ny    = np.array([float(myapp.coords_table.item(_y,1).text()) for _y in range(myapp.coords_table.rowCount())])
            nodes = np.array(list(zip(nx,ny)))
            dist  = np.linalg.norm(nodes - pt, ord=2, axis=1)
            nearest = sorted(list(zip(dist,range(len(dist)))))[0]
            
            if nearest[0] < 0.5:
                myapp.quer_label.setText("({:0.2f},{:0.2f})".format(nx[nearest[1]],ny[nearest[1]]))
                myapp.quer_label.setPos(nx[nearest[1]],ny[nearest[1]])
            else:
                myapp.quer_label.setText('')

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
            
            if nearest[0] < 0.5:
                myapp.quer2_label.setText("({:0.2f},{:0.2f})".format(myapp.df_pro.loc[myapp.loc2]['X'][nearest[1]],
                                         myapp.df_pro.loc[myapp.loc2]['Y'][nearest[1]]))
                myapp.quer2_label.setPos(myapp.df_pro.loc[myapp.loc2]['X'][nearest[1]],
                                         myapp.df_pro.loc[myapp.loc2]['Y'][nearest[1]])
            else:
                myapp.quer2_label.setText('')
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