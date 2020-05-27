# -*- coding: utf-8 -*-
'''import system modules'''
import sys
import os
from functools            import partial

'''import pyqt5 modules'''
#import pyqtgraph          as     pg
from PyQt5.QtCore         import Qt
from PyQt5.QtGui          import QIcon,QPixmap#,QColor
from PyQt5.QtWidgets      import QHeaderView

from modules.langPlot     import (ulangPlot,
                                  langPlotUpdate,langPlotBeautify,
                                  plot_update_coords,
                                  undo_but,redo_but,
                                  xyMarker,xyUnmark,
                                  looplzeit,fps_changed,
                                  l_looper,dt_changed,
                                  langPlotUpdate,langPlotBeautify)
from dialogs.plotPropL    import changePropsL

SCRIPT_DIR = os.path.dirname(os.path.realpath(sys.argv[0]))

def connections(self):
    
    #Menu commands
    self.checkver.triggered.connect(partial(self.checkForUpdates,clicked=True))
    self.reload.triggered.connect(self.reloadModel)
    self.closeall.triggered.connect(self.close)

     
    #Profil Props
    self.symbols = ['h','s','p','o','+','d','star'] 
    self.penstyle= [Qt.SolidLine,Qt.DashLine,Qt.DotLine,Qt.DashDotLine,Qt.DashDotDotLine]
    
    #längsschnitt und querschnitt viewer
    self.ls_listWidget.itemDoubleClicked.connect(partial(langPlotBeautify,self))
    self.ls_listWidget.itemChanged.connect(partial(langPlotUpdate,self))

    self.lang_ID.currentTextChanged.connect(partial(ulangPlot,self))
    self.wrTable.itemSelectionChanged.connect(partial(xyMarker,self))
    self.wrTable.itemChanged.connect(partial(plot_update_coords,self))

    #sliders
    self.play_lzeit.stateChanged.connect(partial(l_looper,self))
    self.fps.valueChanged.connect(partial(fps_changed,self))
    self.lzeitslider.valueChanged.connect(partial(looplzeit,self))
    self.show_simdatetime.dateTimeChanged.connect(partial(dt_changed,self))
    
    # self.langView.scene().sigMouseClicked.connect(partial(xyUnmark,self))
    # self.langView.scene().sigMouseMoved.connect(partial(pointer_q1,self))
 
#    connect radio buttons
    # self.lsType.buttonToggled.connect(self.toggle)
    # self.rType.buttonToggled.connect(self.toggle)
    
    #under editing mode
    # self.undo.clicked.connect(partial(undo_but,self))
    # self.redo.clicked.connect(partial(redo_but,self))
    self.statusbar.messageChanged.connect(partial(logging,self))
    
def initiateBeautify(self):

    #icons
    icon = QIcon()
    icon.addPixmap(QPixmap(os.path.join(SCRIPT_DIR,"icons","rauEditor.png")), QIcon.Normal,QIcon.Off)
    self.setWindowIcon(icon)
    icon24 = QIcon()
    icon24.addPixmap(QPixmap(os.path.join(SCRIPT_DIR,"icons","settings.ico")), QIcon.Normal,QIcon.Off)
    self.settings.setIcon(icon24)
    icon28 = QIcon()
    icon28.addPixmap(QPixmap(os.path.join(SCRIPT_DIR,"icons","reload.ico")), QIcon.Normal,QIcon.Off)
    self.reload.setIcon(icon28)

    #set table sizes for nicer display
    self.wrTable.horizontalHeader().setSectionResizeMode(QHeaderView.Stretch)

def logging(self,i):
#    if not i == 'Ready':self.logWindow.insertPlainText(i+'\n')
    self.logWindow.insertPlainText(i+'\n')