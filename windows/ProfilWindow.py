# -*- coding: utf-8 -*-
'''import system modules'''
import sys
import os
import subprocess
import pandas            as pd
from packaging           import version
from functools           import partial

'''import modules to read hydret'''
from modules.rawh1d      import HYDRET as h1d
from modules.editsection import update_labels2
from modules.loadgeo     import load_qinfo2
from modules.plotting    import (qplots,uqplots,plotcols,
                                 plotROI,
                                 update_schnitt,
                                 plot_update_coords)
from modules.plot3d       import plot3d

'''import pyqt5 modules'''
import pyqtgraph          as     pg

from PyQt5.QtWidgets      import (QMainWindow,
                                  QMessageBox,
                                  QFileDialog,
                                  QAbstractItemView)
from PyQt5                import uic
from PyQt5.QtCore         import QCoreApplication,QEvent
from windows.beautifyP    import initiateBeautify,connections

_translate = QCoreApplication.translate

class ProfW(QMainWindow):
    
    def __init__(self,__version__,propath=None,sdf=None):
        QMainWindow.__init__(self)        
        self.script_dir = self.src_path = os.path.dirname(sys.argv[0])
        uic.loadUi(os.path.join(self.src_path,'ui','proViewer.ui'),self)
        self.setWindowTitle(_translate("MainWindow", "Hydret1D-GUI Profil Editor v"+str(__version__)))
        
        #status
        self.Edit = False
        self.changes = 0
        self.statusbar = self.statusBar
        self.statusbar.showMessage('Ready')
        self.src_path = sys.argv[0]
        self.df_start = sdf
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
       
        #make connections
        connections(self)
        
        #Beautify GUI
        initiateBeautify(self)
        #look for updates
        self.checkForUpdates(__version__,clicked=False)
        
        if propath is not None: self.OpenEnv(propath=propath)
    
            
    def OpenEnv(self,propath=None):
        
        if propath:
            self.propath = propath
        else:
            self.propath,ext = QFileDialog.getOpenFileName(caption='Hydret Pro-Datei Öffnen (.pro File)',
                                                               filter="*.pr*")
        if self.propath != '':
            #initiate model
            self.df_pro   = h1d().readPRO(self.propath)
            
            self.statusbar.showMessage(os.path.basename(self.propath)+' wird geladen...')
            self.initiate()
            if self.df_start is not None:
                self.gid3d.addItems(self.df_start.ID.unique().astype(str))
            try:
                plot3d(self)
            except:
                self.statusbar.showMessage('3D wird nicht geplottet.')
            self.statusbar.showMessage('Fertig!')
            self.statusbar.showMessage('Ready')
    
    def reloadModel(self):
        ki = self.knotenNr.currentIndex()
        self.statusbar.showMessage(os.path.basename(self.propath)+' wird erneut geladen...')
        self.df_pro   = h1d().readPRO(self.propath)
        self.initiate(i=ki)
        self.statusbar.showMessage('Fertig!')
        
    def initiate(self,i=0):
        self.savepro.setEnabled(True)
        self.menuJunctions.setEnabled(True)
        self.arbox.setEnabled(True)
        self.AspectRatio.setEnabled(True)
        self.reload.setEnabled(True)
                
        self.forceplot = True
        #initiate defaults
        if not hasattr(self,'plotdefaults'):
            self.forceplot = True
            plotcols(self)
        
        #set querprofil view active
        self.start_editing.setEnabled(True)
        self.copy_clip.setEnabled(True)

        #update Profile
        self.knotenNr.blockSignals(True)
        self.station_label.blockSignals(True)
        self.schnittName_label.blockSignals(True)
        try:
            self.knotenNr.clear()
            self.station_label.clear()
            self.schnittName_label.clear()
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
        self.idChange(i=i)
        self.statusbar.showMessage('Ready')

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
            self.df_db   = [df]
            #update editing info
            self = update_labels2(self)
        else:
            df   = self.df_pro.copy()

        #Plot q1 Node
        self.loc   = int(self.knotenNr.currentText())
        self.iloc  = (self.df_pro.index.tolist()).index(self.loc)
        self.Node  = df.loc[self.loc]
        
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
        load_qinfo2(self,df,i)

        #try updating the existing plot
        try:
            if self.forceplot:
                qplots(self)
            else: uqplots(self)
            
            if hasattr(self,'editable_schnitt'):
                if self.editable_schnitt in self.graphicsView.items():
                    self.graphicsView.removeItem(self.editable_schnitt)
                
            if self.Edit:
                self.editable_schnitt = pg.PolyLineROI([],movable=False)
                self.editable_schnitt.setPen(pg.mkPen(color='y',width=3))
                self.editable_schnitt.sigRegionChanged.connect(partial(update_schnitt,self))
                self.editable_schnitt.sigRegionChangeFinished.connect(partial(plot_update_coords,self))
                self.editable_schnitt.blockSignals(True)
                self.graphicsView.addItem(self.editable_schnitt)
                plotROI(self)
                self.editable_schnitt.blockSignals(False)
                
        except Exception as error:
            self.statusbar.showMessage('Plot Fehler an Knoten: '+str(self.loc)+'\nFehler Meldung: '+repr(error))
        self.knotenNr.blockSignals(False)
        self.station_label.blockSignals(False)
        self.schnittName_label.blockSignals(False)

    def initiateEditing(self):
        self.Edit = True
        self.graphicsView.setLabel('left',"EDITING MODE")
        self.stop_editing.setEnabled(True)
        self.start_editing.setEnabled(False)
        self.delete_rows.setEnabled(True)
        
        self.coords_table.setEditTriggers(QAbstractItemView.DoubleClicked)
        self.paste_clip.setEnabled(True)
        
        self.ctab_label.setEnabled(True)
        self.modus_label.setEnabled(True)
        self.maxHeight_label.setEnabled(True)
        
        # self.knotenNr.setEditable(True)
        # self.station_label.setEditable(True)
        # self.schnittName_label.setEditable(True)

        self.df_copy = self.df_pro.copy()
        db_ = self.df_copy.copy()
        self.df_db   = [db_]
        self.idChange(i=self.knotenNr.currentIndex())

    def FinishEditing(self):
        self.coords_table.setEditTriggers(QAbstractItemView.NoEditTriggers)
        STATE_EDIT = update_labels2(self)
        self.Edit  = False
        self       = update_labels2(self)
        self.savepro.setEnabled(True)
        self.graphicsView.showLabel('left',show=False)
        saveEdits = QMessageBox.question(self,'Editor',"Save Edits?", QMessageBox.Yes | QMessageBox.No, QMessageBox.No)
        
        if saveEdits == QMessageBox.Yes:
            self = STATE_EDIT
            self.df_pro.update(self.df_copy)
            self.knotenNr.blockSignals(True)
            self.knotenNr.clear()
            self.pos_ = self.df_pro.index>0
            self.knotenNr.addItems(self.df_pro[self.pos_].index.astype(str))
            self.station_label.addItems(self.df_pro[self.pos_]['Station'].astype(str))
            self.schnittName_label.addItems(self.df_pro[self.pos_]['PName'].astype(str))
            self.knotenNr.blockSignals(False)
        else:  pass
        
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
        self.paste_clip.setEnabled(False)
        
        self.ctab_label.setEnabled(False)
        self.modus_label.setEnabled(False)
        self.maxHeight_label.setEnabled(False)
        
        # self.knotenNr.setEditable(False)
        # self.station_label.setEditable(False)
        # self.schnittName_label.setEditable(False)
        self.idChange(i=self.knotenNr.currentIndex())

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
