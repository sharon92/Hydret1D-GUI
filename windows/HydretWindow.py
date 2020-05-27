# -*- coding: utf-8 -*-
'''import system modules'''
import os
import sys
import pickle
import subprocess
import numpy             as np
import pandas            as pd
from packaging           import version

'''import modules to read hydret'''
from modules.rawh1d      import HYDRET as h1d
from modules.loadhyd     import load_hyd
from modules.loadgeo     import load_start,load_qinfo
from modules.plotting    import (qplots,uqplots,plotcols,
                                 plot_wsp,langPlot,
                                 nodeMarker)
from modules.nodePlot    import nodePlot
from modules.savefiles   import loadgui
from modules.plot3d      import plot3d

'''import pyqt5 modules'''
import pyqtgraph          as     pg

from PyQt5.QtWidgets      import (QMainWindow,
                                  QMessageBox,
                                  QFileDialog,
                                  QListWidgetItem)
from PyQt5                import uic
from PyQt5.QtCore         import QCoreApplication,QEvent,QDateTime
from windows.beautifyH    import initiateBeautify,connections

_translate = QCoreApplication.translate

class MainW(QMainWindow):
    
    def __init__(self,__version__):
        QMainWindow.__init__(self)
        self.src_path = os.path.dirname(sys.argv[0])
        uic.loadUi(os.path.join(self.src_path,'ui','hydretUI.ui'),self)
        self.setWindowTitle(_translate("MainWindow", "Hydret1D-GUI  v"+str(__version__)))
        self.version = __version__
        self.showMaximized()
        
        #status
        self.Edit = False
        self.changes = 0
        self.statusbar = self.statusBar
        self.statusbar.showMessage('Ready')
        
        
#        Docks
        self.tabifyDockWidget(self.qp_dock,self.ls_dock)
        self.tabifyDockWidget(self.ls_dock,self.lp_dock)
        
        qplotD =     {
                      'view'    : [self.graphicsView,self.graphicsView2],
                      'node'    : [None,None],
                      'rnode'   : [None,None],
                      'riloc'   : [None,None],
                      'snode'   : [None,None],
                      'siloc'   : [None,None],
                      'axis'    : [None,None],
                      'lamtbank': [None,None],
                      'lamcbank': [{},None],
                      'lambank' : [None,None],
                      'annotate': [None,None],
                      'riverbed': [None,None],
                      'plotbot' : [None,None],
                      'sohle'   : [None,None],
                      'rbank'   : [None,None],
                      'pointer' : [None,None]
                      }
        
        self.qplotD = pd.DataFrame(qplotD,dtype=object,index=[0,1])
        self.lw_dict      = {}
        self.lw_dictL     = {'sohle'              : [None,None,None],
                             'max WSP'            : [None,None,None],
                             'min WSP'            : [None,None,None],
                             'instationär WSP'    : [None,None,None],
                             'instationär Abfluss': [None,None,None],
                             'Anfangs WSP'        : [None,None,None]
                             }
        self.lwopt_dict   = {}
        self.lwopt_dictL  = {}
        self.loadshp_dict = {}
       
        #make connections
        connections(self)
        
        #Beautify GUI
        self.script_dir = self.src_path
        initiateBeautify(self)
        #look for updates
        self.checkForUpdates(__version__,clicked=False)
    
            
    def OpenEnv(self,DOUBLECLICK_FILE=False):
        if DOUBLECLICK_FILE: self.HydretEnv,ext = (sys.argv[1],'')
        else: self.HydretEnv,ext = QFileDialog.getOpenFileName(caption='Hydret Projekt Öffnen (Hydret GUI/RUN File)',filter="Hydret Projekt (*.run *.hgui)")
            
        if self.HydretEnv != '':
            
            if os.path.splitext(os.path.basename(self.HydretEnv))[1].lower() == '.run':
                #initiate model
                self.h1dmodel   = h1d(hydret_path = self.HydretEnv)
                self.h1denv     = os.path.abspath(os.path.dirname(self.HydretEnv))
                self.h1drun     = os.path.abspath(self.HydretEnv).split('\\')[-1]
                self.h1drunpath = self.HydretEnv
                self.statusbar.showMessage(self.h1drun+' wird geladen...')
                self.initiate(hyd = self.h1dmodel)
            
            elif os.path.splitext(os.path.basename(self.HydretEnv))[1].lower() == '.hgui':
                
                #initiate model
                self.statusbar.showMessage(self.h1drun+' wird geladen...')
                with open(self.HydretEnv, 'rb') as json_file:
                    self.proj   = pickle.load(json_file)
                
                self.hguiname   = os.path.basename(self.HydretEnv)
                self.h1dmodel   = h1d(hydret_path = self.proj['Runpath'])
                self.h1denv     = os.path.abspath(os.path.dirname(self.proj['Runpath']))
                self.h1drun     = os.path.abspath(self.proj['Runpath']).split('\\')[-1]
                self.h1drunpath = self.proj['Runpath']
                self.p_plan.setText(self.proj['Plan'])
                
                self.buttonGroup.blockSignals(True)
                self.buttonGroup_2.blockSignals(True)
                for b in self.buttonGroup.buttons():
                    if b.objectName == self.proj['nodeview']:
                        b.setChecked(True)
                        
                for b in self.buttonGroup_2.buttons():
                    if b.objectName == self.proj['nodemethod']:
                        b.setChecked(True)
                self.buttonGroup.blockSignals(False)
                self.buttonGroup_2.blockSignals(False)
                
                self.initiate(hyd = self.h1dmodel)
                loadgui(self,self.proj)
            self.statusbar.showMessage('Fertig!')
            self.statusbar.showMessage('Ready')
    
    def reloadModel(self):
        gi = self.lang_ID.currentIndex()
        ki = self.knotenNr.currentIndex()
        
        #states of different views
        q1 = self.graphicsView.getViewBox().getState(copy=True)
        q2 = self.graphicsView2.getViewBox().getState(copy=True)
        l1 = self.langView.getViewBox().getState(copy=True)
        l2 = self.qlangView.getViewBox().getState(copy=True)
        n  = self.nodeView.getViewBox().getState(copy=True)
        self.h1dmodel   = h1d(hydret_path = self.HydretEnv)
        self.statusbar.showMessage(self.h1drun+' wird erneut geladen...')
        self.initiate(hyd = self.h1dmodel,i=ki)
        self.lang_ID.setCurrentIndex(gi)
        
        #restore states
        self.graphicsView.getViewBox().setState(q1)
        self.graphicsView2.getViewBox().setState(q2)
        self.langView.getViewBox().setState(l1)
        self.qlangView.getViewBox().setState(l2)
        self.qlangView.getViewBox().setXLink(self.langView.getViewBox())
        self.nodeView.getViewBox().setState(n)
        
        self.statusbar.showMessage('Fertig!')
        
    def initiate(self,hyd,i=0):
        self.h1d = hyd
        self.menuSave.setEnabled(True)
        self.run.setEnabled(True)
        self.node_renumber.setEnabled(True)
        self.arbox.setEnabled(True)
        self.lp_layers.setEnabled(True)
        self.reload.setEnabled(True)
        
        self.ls_listWidget.clear()
        #view box item update
        try:
            self.achse = hyd.achse
            self.lp_view_box(hyd.achse.split("\\")[-1])
        except:
            pass
        
        #adjustwindows
        w1,w2 = self.splitter.sizes()
        self.splitter.setSizes([(w1+w2)*0.7,(w1+w2)*0.3])
        
        self.forceplot = True
        #initiate defaults
        if not hasattr(self,'plotdefaults'):
            self.forceplot = True
            plotcols(self)
        
        #initiate HYD 
        load_hyd(self)
        '''hyd checks'''
        lead_ = (60./self.h1d.tinc)*self.h1d.toth+1
        if not self.h1d.lead == lead_:
            self.h1d.lead == lead_
            self.statusbar.showMessage('Anzahl der Ganglinienstützstellen ersetzt zum '+str(lead_))
            self.p_lead.setText(str(lead_))
        
        nl_  = len(self.h1d.df_start.index) -1
        if not self.h1d.nl == -1*nl_:
            self.h1d.nl = -1*nl_
            self.statusbar.showMessage('Anzahl der Gewässerabschnitte ersetzt zum '+str(nl_))
            self.p_nl.setValue(-1*nl_)
        
        #set querprofil view active
        self.df_pro   = hyd.df_pro
        self.qp_widget.setEnabled(True)
        self.copy_clip.setEnabled(True)

        #set lageplan view active
        self.df_start     = hyd.df_start
        
        #reindex qch und hch
        if hasattr(self,'hz_df'): 
            self.hz_df = self.hz_df.reindex(columns=self.df_start.index).transpose()
            item = QListWidgetItem('instationär WSP')
            item.setCheckState(2)
            self.ls_listWidget.addItem(item)
            item = QListWidgetItem('max WSP')
            item.setCheckState(2)
            self.ls_listWidget.addItem(item)
            item = QListWidgetItem('Anfangs WSP')
            item.setCheckState(2)
            self.ls_listWidget.addItem(item)
            item = QListWidgetItem('min WSP')
            item.setCheckState(2)
            self.ls_listWidget.addItem(item)
        else:
            item = QListWidgetItem('Anfangs WSP')
            item.setCheckState(2)
            self.ls_listWidget.addItem(item)
            
        if hasattr(self,'qz_df'): 
            self.qz_df = self.qz_df.reindex(columns=self.df_start.index).transpose()
            item = QListWidgetItem('instationär Abfluss')
            item.setCheckState(2)
            self.ls_listWidget.addItem(item)
            
        item = QListWidgetItem('sohle')
        item.setCheckState(2)
        self.ls_listWidget.addItem(item)
        
        self.mod_plan     = self.p_plan.text()

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
        if not np.isnan(self.df_start.loc[self.df_pro.index[self.iloc]]['ID']):
            self.lang_ID.addItems(np.unique(self.df_start['ID'].values).astype(str))
            self.gewid_current = int(self.df_start.loc[self.df_pro.index[self.iloc]]['ID'])
            self.lang_ID.setCurrentText(_translate("MainWindow", str(self.gewid_current)))
            self.lang_ID.blockSignals(False)
        self.idChange(i=i)
        
        #update start
        self.node_label = pg.TextItem(color='k',border='k',fill='w')
        self.vLine_node = pg.InfiniteLine(angle=90, movable=False)
        self.hLine_node = pg.InfiniteLine(angle=0,  movable=False)
        self.node_label.setZValue(10000)
        self.vLine_node.setZValue(9999)
        self.hLine_node.setZValue(9999)
        nodePlot(self)
#        
        #update langschnitt
        # try:        
        langPlot(self)
        self.lang_label = pg.TextItem(color='k',anchor = (0,1),border='k',fill='w')
        self.langView.addItem(self.lang_label)
        self.lang_label.hide()
        self.lang_label.setTextWidth(150)
        self.lang_label.setZValue(10000)
        try:
            plot3d(self)
        except:
            self.statusbar.showMessage('3D wird nicht geplottet.')
        
        if hasattr(self,'proj'): dt = QDateTime.fromString(self.proj['DateTime'])
        else: dt = self.sim_datetime.dateTime()
        self.sim_datetime.setDateTime(dt)
        self.show_simdatetime.setDateTime(dt)
        self.show_simdatetime.setMinimumDateTime(dt)
        if hasattr(self,'h1d'): self.show_simdatetime.setMaximumDateTime(dt.addSecs(self.h1d.toth*3600))
        self.statusbar.showMessage('Ready')
    
    def updateDateTimeRange(self,dt):
        self.show_simdatetime.setDateTime(dt)
        self.show_simdatetime.setMinimumDateTime(dt)
        if hasattr(self,'h1d'): self.show_simdatetime.setMaximumDateTime(dt.addSecs(self.h1d.toth*3600))
        
    def toggle(self):
        self.idChange(i=self.knotenNr.currentIndex())
        
    def idChange(self,i):
        '''
        i = index of combo box (0,1,2...n)
        '''
        #blocking signals to prevent infinite loops
        self.statusbar.showMessage('Ready')
        self.knotenNr.blockSignals(True)
        self.station_label.blockSignals(True)
        self.schnittName_label.blockSignals(True)

        self.changes = 0
        self.knotenNr.setCurrentIndex(i)

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
        try:
            if self.forceplot:
                qplots(self)
                plot_wsp(self)
            else: uqplots(self)
            
            if hasattr(self,'editable_schnitt'):
                if self.editable_schnitt in self.graphicsView.items():
                    self.graphicsView.removeItem(self.editable_schnitt)
                
                
            nodeMarker(self,self.df_s)
        except Exception as error:
            self.statusbar.showMessage('Plot Fehler an Knoten: '+str(self.loc)+'\nFehler Meldung: '+repr(error))
        self.knotenNr.blockSignals(False)
        self.station_label.blockSignals(False)
        self.schnittName_label.blockSignals(False)

    #look for updates
    def checkForUpdates(self,__version__,clicked=True):
        try:
            cver = r'x:\Programme\Hydret1D-GUI\version'
            with open(cver,'r') as v: line = v.readlines()[0]
            ver,pfad = line.split(',')
            
            if version.parse(ver) > version.parse(__version__):
                ask = QMessageBox.question(self,'Updates verfügbar! Aktualisieren?',('Aktuelle Version: '
                                                                      +__version__+' --> Neuste Version: '
                                                                      +ver),
                                 QMessageBox.Yes,QMessageBox.No)
                if ask == QMessageBox.Yes:
                    subprocess.Popen(r'explorer /select,'+os.path.abspath(pfad))
                    sys.exit()
    
            else:
                if clicked:
                    QMessageBox.question(self,'Up to Date!',('Aktuelle Version: '
                                                                          +__version__+'\nVersion Verfügbar: '
                                                                          +ver),QMessageBox.Ok)

        except:
            pass
