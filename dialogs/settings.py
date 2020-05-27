# -*- coding: utf-8 -*-
'''import system modules'''
import os
import pickle
import copy
from functools           import partial

'''import pyqt5 modules'''
from PyQt5               import uic
from PyQt5.QtWidgets     import QDialog,QColorDialog
from PyQt5.QtGui         import QColor
from modules.rawh1d      import HYDRET as h1d

import sys
script_dir = os.getcwd()
SCRIPT_DIR = os.path.dirname(os.path.realpath(sys.argv[0]))
ui         = os.path.join(SCRIPT_DIR,'ui','defaults.ui')

class defaultColors(QDialog):
    def __init__(self, dpath=None,parent=None):
        super().__init__(parent)
        uic.loadUi(ui,self)
        self.ok.clicked.connect(self.accept)
        self.cancel.clicked.connect(self.reject)
        
        self.buttons =  {
                          'i1'  : [self.c1,self.v1,self.s1,self.g1],
                          
                          'i2'  : [self.c2,self.v2,self.s2,self.g2],
                          
                          'i3'  : [self.c3,self.v3,self.s3,self.g3],
                          
                          'i4'  : [self.c4,self.v4,self.s4,self.g4],
                          
                          'i5'  : [self.c5,self.v5,self.s5,self.g5],
                          
                          'i6'  : [self.c6,self.v6,self.s6,self.g6],
                          
                          'i7'  : [self.c7,self.v7,self.s7,self.g7],
                          
                          'i8'  : [self.c8,self.v8,self.s8,self.g8],
                          
                          'i9'  : [self.c9,self.v9,self.s9,self.g9],
                          
                          'q'   : [self.qc,None,self.qs,self.qg],
                          
                          'qp'  : [self.qpc,None,self.qps,self.qpw],
                          
                          'l'   : [self.lc,None,self.ls,self.lg],
                          
                          'lp'  : [self.lpc,None,self.lps,self.lpw],
                          
                          'qf'  : [self.qfc,self.qf,None,self.qb],
                          
                          'lf'  : [self.lfc,self.lf,None,self.lb],
                          
                          'w'   : [self.wpc,None,self.wps,self.wpw],
                          
                          'e'   : [None,None,self.eps,self.epw]
                          
                          }
        self.orivals = {
                          'i1'  : [QColor(0,0,0,255),True,0,6],
                          
                          'i2'  : [QColor(255,0,0,255),True,1,6],
                          
                          'i3'  : [QColor(0,0,0,255),True,2,6],
                          
                          'i4'  : [QColor(0,0,255,255),True,3,6],
                          
                          'i5'  : [QColor(0,0,0,255),True,4,9],
                          
                          'i6'  : [QColor(0,170,127,255),True,6,10],
                          
                          'i7'  : [QColor(0,170,127,255),True,6,10],
                          
                          'i8'  : [QColor(0,170,127,255),True,6,10],
                          
                          'i9'  : [QColor(0,0,255,255),True,5,9],
                          
                          'q'   : [QColor(255,0,0,255),True,5,1],
                          
                          'qp'  : [QColor(0,0,0,255),None,0,1],
                          
                          'l'   : [QColor(85,0,255,255),True,3,2],
                          
                          'lp'  : [QColor(0,0,0,255),None,0,1],
                          
                          'qf'  : [QColor(211,140,105,255),True,None,2],
                          
                          'lf'  : [QColor(211,140,105,255),True,None,0.2],
                          
                          'w'   : [QColor(0,0,0,255),None,0,1],
                          
                          'e'   : [None,None,1,1]
                          }
        if dpath is not None:
            pickle_in  = open(dpath,'rb')
            self.defaultvals = pickle.load(pickle_in)
            for key in self.buttons.keys():
                if key in self.defaultvals.keys():
                    dkcontains = True
                else:
                    dkcontains = False
                    break
            if dkcontains:
                self.updatefromdict()
            else:
                self.defaultvals = copy.deepcopy(self.orivals)
        else:
            self.defaultvals = copy.deepcopy(self.orivals)
        
        for key,but in self.buttons.items():
            if but[0] is not None:
                but[0].clicked.connect(partial(self.colorpicker,button = but[0],dkey = key))
            if but[1] is not None:
                but[1].stateChanged.connect(partial(self.updatevis,button = but[1],dkey = key))
            if but[2] is not None:
                but[2].currentIndexChanged.connect(partial(self.updatesymb,dkey=key))
            but[3].valueChanged.connect(partial(self.updatesize,dkey=key))
        
        self.reset.clicked.connect(self.restoredict)
    
    def restoredict(self):
        self.defaultvals = copy.deepcopy(self.orivals)
        self.updatefromdict()
    
    def updatefromdict(self):
        
        for key,item in self.buttons.items():
            if item[0] is not None:
                item[0].setStyleSheet("QPushButton {\n"
        "     background-color: rgba"+str(self.defaultvals[key][0].getRgb())+"; border: 1px solid black;\n"
        "}")
            if item[1] is not None:
                if self.defaultvals[key][1]: s =2
                else: s= 0
                item[1].setCheckState(s)
            if item[2] is not None:
                item[2].setCurrentIndex(self.defaultvals[key][2])
            item[3].setValue(self.defaultvals[key][3])
            
        
    def colorpicker(self,button,dkey):
        color = QColorDialog.getColor(parent = self,title = 'Choose Color', options = QColorDialog.ShowAlphaChannel)
        if color.isValid():
            button.setStyleSheet("QPushButton {\n"
    "     background-color: rgba"+str(color.getRgb())+"; border: 1px solid black;\n"
    "}")
            self.defaultvals[dkey][0] = color
            
    def updatesymb(self,i,dkey):
        self.defaultvals[dkey][2] = i
        
    def updatesize(self,i,dkey):
        self.defaultvals[dkey][3] = i
        
    def updatevis(self,button,dkey):
        if button.checkState() == 2:
            self.defaultvals[dkey][1] = True
        else:
            self.defaultvals[dkey][1] = False

def settingswin(self,dpath):
    if dpath == 'None':
        Popup = defaultColors()
    else:
        Popup = defaultColors(dpath=dpath)
    if Popup.exec_():
        
        if Popup.saveDefault.isChecked():
            if not os.path.isdir(os.path.join(self.script_dir,'defaults')):
                os.mkdir(os.path.join(self.script_dir,'defaults'))
            ndpath = os.path.join(self.script_dir,'defaults','default_dict.pickle')
            pickle_out = open(ndpath,'wb')
            pickle.dump(Popup.defaultvals,pickle_out)
            pickle_out.close()
        
        self.plotdefaults = Popup.defaultvals
        
        if hasattr(self,'h1drunpath'):
            n_h1d = h1d(hydret_path = self.h1drunpath)
            self.initiate(hyd = n_h1d,i=self.knotenNr.currentIndex())

