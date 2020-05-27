# -*- coding: utf-8 -*-
'''import system modules'''
import sys
import os
import subprocess
import numpy             as np
import pandas            as pd
from packaging           import version
from functools           import partial

'''import modules to read hydret'''
from modules.rawh1d      import HYDRET as h1d
from modules.rawh1d      import readHQ
from modules.langPlot    import (plotcols,
                                 langPlot)

'''import pyqt5 modules'''
import pyqtgraph          as     pg

from PyQt5.QtWidgets      import (QMainWindow,
                                  QMessageBox,
                                  QFileDialog,
                                  QListWidgetItem,
                                  QAbstractItemView)
from PyQt5                import uic
from PyQt5.QtCore         import QCoreApplication,QEvent,QDateTime
from windows.beautifyls   import initiateBeautify,connections

_translate = QCoreApplication.translate

class lsW(QMainWindow):
    
    def __init__(self,__version__,gui):
        QMainWindow.__init__(self)        
        self.script_dir = self.src_path = os.path.dirname(sys.argv[0])
        uic.loadUi(os.path.join(self.src_path,'ui','lsViewer.ui'),self)
        self.setWindowTitle(_translate("MainWindow", "Hydret1D-GUI Rauheit Editor v"+str(__version__)))
        self.gui = gui
        
        #status
        self.Edit = False
        self.changes = 0
        self.statusbar = self.statusBar
        self.statusbar.showMessage('Ready')
        self.src_path = sys.argv[0]

        self.lw_dictL     = {'sohle'              : [None,None,None],
                             'max WSP'            : [None,None,None],
                             'min WSP'            : [None,None,None],
                             'instationär WSP'    : [None,None,None],
                             'Anfangs WSP'        : [None,None,None],
                             'effektiv Rauheit'   : [None,None,None],
                             'wsp     Stufe 0'    : [None,None,None],
                             'rauheit Stufe 0'    : [None,None,None],
                             'wsp     Stufe 1'    : [None,None,None],
                             'rauheit Stufe 1'    : [None,None,None],
                             'wsp     Stufe 2'    : [None,None,None],
                             'rauheit Stufe 2'    : [None,None,None],
                             'wsp     Stufe 3'    : [None,None,None],
                             'rauheit Stufe 3'    : [None,None,None],
                             'wsp     Stufe 4'    : [None,None,None],
                             'rauheit Stufe 4'    : [None,None,None],
                             'wsp     Stufe 5'    : [None,None,None],
                             'rauheit Stufe 5'    : [None,None,None],
                             'wsp     Stufe 6'    : [None,None,None],
                             'rauheit Stufe 6'    : [None,None,None],
                             'wsp     Stufe 7'    : [None,None,None],
                             'rauheit Stufe 7'    : [None,None,None]
                             }
        self.lwopt_dictL  = {}
        
        for n,keys in enumerate(self.lw_dictL.keys()):
            item = QListWidgetItem(keys)
            item.setCheckState(0)
            if n < 6: item.setCheckState(2)
            self.ls_listWidget.addItem(item)
        #setup results data
        self.hz_df = self.gui.hz_df
        self.hch.setText(self.gui.h1d.hch_p)
                
        self.lzeitslider.setMaximum(int(self.gui.h1d.lead)-1)
        self.lzeitslider.setTickInterval(1)
        self.lzeitslider.setSingleStep(1)
        self.lzeitslider.setValue(int(self.gui.h1d.lead)-1)
        
        #make connections
        connections(self)
        
        #Beautify GUI
        initiateBeautify(self)
        #look for updates
        self.checkForUpdates(__version__,clicked=False)
        hyd = self.gui.h1d
        self.initiate(hyd)
    
    def reloadModel(self):
        gi = self.lang_ID.currentIndex()
        self.h1dmodel   = h1d(hydret_path = self.gui.HydretEnv,l=gi)
        self.statusbar.showMessage(self.gui.h1drun+' wird erneut geladen...')
        self.initiate(hyd = self.gui.h1dmodel)
        self.statusbar.showMessage('Fertig!')
        
    def initiate(self,hyd,l = 0):
        self.h1d = hyd
        self.run.setEnabled(True)
        self.reload.setEnabled(True)
    
        self.forceplot = True
        #initiate defaults
        if not hasattr(self,'plotdefaults'):
            self.forceplot = True
            plotcols(self)
        #set lageplan view active
        self.df_start  = hyd.df_start
        
        #reindex qch und hch
        self.hz_df = readHQ(self,self.hch.text(),form=self.gui.h1d.format[0])
        self.hz_df = self.hz_df.reindex(columns=self.df_start.index).transpose()

        #update Längschnitt
        self.lang_ID.blockSignals(True)
        try:self.lang_ID.clear()
        except:pass
        
        self.lang_ID.addItems(np.unique(self.df_start['ID'].values).astype(str))
        self.lang_ID.setCurrentIndex(l)
        self.gewid_current = int(self.lang_ID.currentText())
        self.lang_ID.blockSignals(False)
       
        #update langschnitt       
        langPlot(self)
        self.lang_label = pg.TextItem(color='k',anchor = (0,1),border='k',fill='w')
        self.langView.addItem(self.lang_label)
        self.lang_label.hide()
        self.lang_label.setTextWidth(150)
        self.lang_label.setZValue(10000)
        
        if hasattr(self.gui,'proj'): dt = QDateTime.fromString(self.gui.proj['DateTime'])
        else: dt = self.gui.sim_datetime.dateTime()
        self.show_simdatetime.setMinimumDateTime(dt)
        self.show_simdatetime.setMaximumDateTime(dt.addSecs(self.h1d.toth*3600))
        currentdt = dt.addSecs(self.lzeitslider.value()*self.h1d.tinc*60)
        self.show_simdatetime.setDateTime(currentdt)
        self.statusbar.showMessage('Ready')
            
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
        except:pass