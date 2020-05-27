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

from dialogs.nodeGen      import nodegenwinshow
from dialogs.nodeRenum    import renumber
from dialogs.profLamellen import raumode
from dialogs.settings     import settingswin
from modules.editsection  import edit_maxHeight
from modules.savefiles    import savePro
from modules.plotting     import (updateViews,
                                  plot_update_coords,changeAR,
                                  undo_but,redo_but,del_but,
                                  xyMarker,xyUnmark,
                                  _handlecopy,
                                  _handlepaste,
                                  pointer_q1,pointer_q2)
from modules.plot3d       import  plot3d,color_change_3d

SCRIPT_DIR = os.path.dirname(os.path.realpath(sys.argv[0]))

def connections(self):
    
    #Menu commands
    self.openp.triggered.connect(self.OpenEnv)
    self.start_editing.triggered.connect(self.initiateEditing)
    self.stop_editing.triggered.connect(self.FinishEditing)
    self.savepro.triggered.connect(partial(savePro,self))
    if os.path.isfile(os.path.join(SCRIPT_DIR,'defaults','default_dict.pickle')):
        self.dpath = os.path.join(SCRIPT_DIR,'defaults','default_dict.pickle')
    else:
        self.dpath = 'None'
    self.settings.triggered.connect(partial(settingswin,self,self.dpath))
    self.checkver.triggered.connect(partial(self.checkForUpdates,clicked=True))
    self.reload.triggered.connect(self.reloadModel)
    self.closeall.triggered.connect(self.close)
    
    #dialog windows
    self.node_renumber.triggered.connect(partial(renumber,self))
    self.node_rau.triggered.connect(partial(raumode,self))
    self.node_gen.triggered.connect(partial(nodegenwinshow,self))
     
    #Profil Props
    self.qschnp  = ['VK','RK','VF','RF','DF','OF','H2','RS','ZS']
    self.qschnt  = [1,2,3,4,5,6,7,8,9]
    self.symbols = ['h','s','p','o','+','d','star'] 
    self.penstyle= [Qt.SolidLine,Qt.DashLine,Qt.DotLine,Qt.DashDotLine,Qt.DashDotDotLine]
    
    self.knotenNr.currentIndexChanged.connect(self.idChange)
    self.station_label.currentIndexChanged.connect(self.idChange)
    self.schnittName_label.currentIndexChanged.connect(self.idChange)
    self.coords_table.itemSelectionChanged.connect(partial(xyMarker,self))
    self.coords_table.itemChanged.connect(partial(plot_update_coords,self))

    #plotting
    self.AspectRatio.valueChanged.connect(partial(changeAR,self))
    self.arbox.toggled.connect(partial(changeAR,self))
    
    self.graphicsView.scene().sigMouseClicked.connect(partial(xyUnmark,self))
    self.graphicsView.scene().sigMouseMoved.connect(partial(pointer_q1,self))
    self.graphicsView2.scene().sigMouseMoved.connect(partial(pointer_q2,self))
    self.graphicsView.getViewBox().sigResized.connect(partial(updateViews,self))
 
    #3d viewer
    self.gid3d.currentIndexChanged.connect(partial(plot3d,self))
    self.colorMap.currentIndexChanged.connect(partial(plot3d,self))
    self.color_dis.valueChanged.connect(partial(plot3d,self))
    self.drawEdges.stateChanged.connect(partial(plot3d,self))
    self.drawFaces.stateChanged.connect(partial(plot3d,self))
    
    
#    connect radio buttons
    self.buttonGroup_4.buttonToggled.connect(self.toggle)
    
    #under editing mode
    self.undo.clicked.connect(partial(undo_but,self))
    self.redo.clicked.connect(partial(redo_but,self))
    self.delete_rows.clicked.connect(partial(del_but,self))
    self.maxHeight_label.valueChanged.connect(partial(edit_maxHeight,self))
    self.copy_clip.clicked.connect(partial(_handlecopy,self))
    self.paste_clip.clicked.connect(partial(_handlepaste,self))
    
    self.statusbar.messageChanged.connect(partial(logging,self))
    
def initiateBeautify(self):

    #icons
    icon = QIcon()
    icon.addPixmap(QPixmap(os.path.join(SCRIPT_DIR,"icons","proEditor.png")), QIcon.Normal,QIcon.Off)
    self.setWindowIcon(icon)
    icon1 = QIcon()
    icon1.addPixmap(QPixmap(os.path.join(SCRIPT_DIR,"icons","redo3.ico")), QIcon.Normal,QIcon.Off)
    self.redo.setIcon(icon1)
    icon2 = QIcon()
    icon2.addPixmap(QPixmap(os.path.join(SCRIPT_DIR,"icons","undo3.ico")), QIcon.Normal,QIcon.Off)
    self.undo.setIcon(icon2)
    icon3 = QIcon()
    icon3.addPixmap(QPixmap(os.path.join(SCRIPT_DIR,"icons","edit.ico")), QIcon.Normal,QIcon.Off)
    self.start_editing.setIcon(icon3)
    icon4 = QIcon()
    icon4.addPixmap(QPixmap(os.path.join(SCRIPT_DIR,"icons","editfinish.ico")), QIcon.Normal,QIcon.Off)
    self.stop_editing.setIcon(icon4)
    icon6 = QIcon()
    icon6.addPixmap(QPixmap(os.path.join(SCRIPT_DIR,"icons","new.ico")), QIcon.Normal,QIcon.Off)
    self.newp.setIcon(icon6)
    icon7 = QIcon()
    icon7.addPixmap(QPixmap(os.path.join(SCRIPT_DIR,"icons","open32.ico")), QIcon.Normal,QIcon.Off)
    self.openp.setIcon(icon7)
    icon10 = QIcon()
    icon10.addPixmap(QPixmap(os.path.join(SCRIPT_DIR,"icons","copy.ico")), QIcon.Normal,QIcon.Off)
    self.copy_clip.setIcon(icon10)
    icon11 = QIcon()
    icon11.addPixmap(QPixmap(os.path.join(SCRIPT_DIR,"icons","Paste.ico")), QIcon.Normal,QIcon.Off)
    self.paste_clip.setIcon(icon11)
    icon12 = QIcon()
    icon12.addPixmap(QPixmap(os.path.join(SCRIPT_DIR,"icons","delete.ico")), QIcon.Normal,QIcon.Off)
    self.delete_rows.setIcon(icon12)
    icon16 = QIcon()
    icon16.addPixmap(QPixmap(os.path.join(SCRIPT_DIR,"icons","save_pro.bmp")), QIcon.Normal,QIcon.Off)
    self.savepro.setIcon(icon16)
    icon24 = QIcon()
    icon24.addPixmap(QPixmap(os.path.join(SCRIPT_DIR,"icons","settings.ico")), QIcon.Normal,QIcon.Off)
    self.settings.setIcon(icon24)
    icon28 = QIcon()
    icon28.addPixmap(QPixmap(os.path.join(SCRIPT_DIR,"icons","reload.ico")), QIcon.Normal,QIcon.Off)
    self.reload.setIcon(icon28)

    #set table sizes for nicer display
    self.coords_table.horizontalHeader().setSectionResizeMode(QHeaderView.Stretch)


def logging(self,i):
#    if not i == 'Ready':self.logWindow.insertPlainText(i+'\n')
    self.logWindow.insertPlainText(i+'\n')