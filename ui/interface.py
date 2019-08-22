# -*- coding: utf-8 -*-
'''import system modules'''
import sys
import os
from functools            import partial

'''import pyqt5 modules'''
import pyqtgraph          as     pg
from PyQt5.QtCore         import Qt
from PyQt5.QtGui          import QIcon,QPixmap#,QColor
from PyQt5.QtWidgets      import QHeaderView



from dialogs.nodeGen      import nodegenwinshow
from dialogs.nodeRenum    import renumber
from dialogs.nodeTable    import spreadwin
from dialogs.profLamellen import raumode
from dialogs.runHydret    import runModel
from dialogs.settings     import settingswin

from modules.loaddata    import dataadd,dataremove
from modules.editsection  import (knoten_label,
                                  edit_modus,
                                  edit_maxHeight)

from modules.plotting     import (updateViews,update_schnitt,
                                  plot_update_coords,changeAR,
                                  plan_name,gewUnmark,ulangPlot,
                                  undo_but,redo_but,del_but,
                                  xyMarker,xyUnmark,
                                  colorpicker,nodePlot,
                                  _handlecopy,
                                  _handlepaste,
                                  loadresult,update_wsp,
                                  pointer_q1,pointer_q2,
                                  pointer_lang,pointer_node)

from modules.loadhyd      import (inflowNodes,
                                  printCurves,
                                  idown_change,
                                  normq,
                                  defWeirs,
                                  defGates,
                                  defJunctions,
                                  lateralInflows,
                                  ovfmode)


SCRIPT_DIR = os.path.dirname(os.path.realpath(sys.argv[0]))


def connections(self):
    
    #Menu commands
    self.openp.triggered.connect(self.OpenEnv)
    self.start_editing.triggered.connect(self.initiateEditing)
    self.stop_editing.triggered.connect(self.FinishEditing)
    self.savep.triggered.connect(self.saveProject)
    self.loadwsp_2.triggered.connect(partial(loadresult,self))
    if os.path.isfile(os.path.join(SCRIPT_DIR,'defaults','default_dict.pickle')):
        self.dpath = os.path.join(SCRIPT_DIR,'defaults','default_dict.pickle')
    else:
        self.dpath = 'None'
    self.settings.triggered.connect(partial(settingswin,self,self.dpath))
    self.checkver.triggered.connect(partial(self.checkForUpdates,clicked=True))
    self.closeall.triggered.connect(self.close)
    #self.saveasp.triggered.connect(self.savePRO)
    
    #dialog windows
    self.node_renumber.triggered.connect(partial(renumber,self))
    self.node_rau.triggered.connect(partial(raumode,self))
    self.n_starttab.triggered.connect(partial(spreadwin,self))
    self.node_gen.triggered.connect(partial(nodegenwinshow,self))
    self.run.triggered.connect(partial(runModel,self))
    
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
    self.qschnp  = ['VK','RK','VF','RF','DF','OF','H2','RS','ZS']
    self.qschnt  = [1,2,3,4,5,6,7,8,9]
    self.symbols = ['h','s','p','o','+','d','star'] 
    self.penstyle= [Qt.SolidLine,Qt.DashLine,Qt.DotLine,Qt.DashDotLine,Qt.DashDotDotLine]
    
    self.knotenNr.currentIndexChanged.connect(self.idChange)
    self.station_label.currentIndexChanged.connect(self.idChange)
    self.schnittName_label.currentIndexChanged.connect(self.idChange)
    self.lang_ID.currentTextChanged.connect(partial(ulangPlot,self))
    self.coords_table.itemSelectionChanged.connect(partial(xyMarker,self))
    self.coords_table.itemChanged.connect(partial(plot_update_coords,self))
    self.p_wspdat.itemChanged.connect(partial(update_wsp,self))
    
    #loading data (shapefiles)
    self.lp_adddata.clicked.connect(partial(dataadd,self))
    self.lp_removedata.clicked.connect(partial(dataremove,self))
    
    #plotting
    self.AspectRatio.valueChanged.connect(partial(changeAR,self))
    self.arbox.toggled.connect(partial(changeAR,self))
    self.p_plan.editingFinished.connect(partial(plan_name,self))
    self.p_wspdat.itemDoubleClicked.connect(partial(colorpicker,self))
    self.langView.scene().sigMouseMoved.connect(partial(pointer_lang,self))
    self.nodeView.scene().sigMouseMoved.connect(partial(pointer_node,self))
    self.nodeView.scene().sigMouseClicked.connect(partial(gewUnmark,self))
    self.graphicsView.scene().sigMouseClicked.connect(partial(xyUnmark,self))
    self.graphicsView.scene().sigMouseMoved.connect(partial(pointer_q1,self))
    self.graphicsView2.scene().sigMouseMoved.connect(partial(pointer_q2,self))
    self.graphicsView.getViewBox().sigResized.connect(partial(updateViews,self))
    self.editable_schnitt = pg.PolyLineROI([],movable=False)
    self.editable_schnitt.setPen(pg.mkPen(color='y',width=3))
    self.editable_schnitt.sigRegionChanged.connect(partial(update_schnitt,self))
    self.editable_schnitt.sigRegionChangeFinished.connect(partial(plot_update_coords,self))

    #connect radio buttons
    self._rquer.toggled.connect(self.toggle)
    self._rrau.toggled.connect(self.toggle)
    self._rschalter.toggled.connect(self.toggle)
    self.nodemapview.toggled.connect(partial(nodePlot,self))
    self.nodeplanview.toggled.connect(partial(nodePlot,self))
    
    #under editing mode
    self.undo.clicked.connect(partial(undo_but,self))
    self.redo.clicked.connect(partial(redo_but,self))
    self.delete_rows.clicked.connect(partial(del_but,self))
    self.edit_knoten.editingFinished.connect(partial(knoten_label,self))
    self.modus_label.activated[str].connect(partial(edit_modus,self))
    self.maxHeight_label.editingFinished.connect(partial(edit_maxHeight,self))
    self.copy_clip.clicked.connect(partial(_handlecopy,self))
    self.paste_clip.clicked.connect(partial(_handlepaste,self))

def initiateBeautify(self):
    #icons
    icon = QIcon()
    icon.addPixmap(QPixmap(os.path.join(SCRIPT_DIR,"icons","hydret1D.ico")), QIcon.Normal,QIcon.Off)
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
    icon8 = QIcon()
    icon8.addPixmap(QPixmap(os.path.join(SCRIPT_DIR,"icons","save.ico")), QIcon.Normal,QIcon.Off)
    self.savep.setIcon(icon8)
    icon9 = QIcon()
    icon9.addPixmap(QPixmap(os.path.join(SCRIPT_DIR,"icons","runmodel.ico")), QIcon.Normal,QIcon.Off)
    self.run.setIcon(icon9)
    icon10 = QIcon()
    icon10.addPixmap(QPixmap(os.path.join(SCRIPT_DIR,"icons","copy.ico")), QIcon.Normal,QIcon.Off)
    self.copy_clip.setIcon(icon10)
    icon11 = QIcon()
    icon11.addPixmap(QPixmap(os.path.join(SCRIPT_DIR,"icons","Paste.ico")), QIcon.Normal,QIcon.Off)
    self.paste_clip.setIcon(icon11)
    icon12 = QIcon()
    icon12.addPixmap(QPixmap(os.path.join(SCRIPT_DIR,"icons","delete.ico")), QIcon.Normal,QIcon.Off)
    self.delete_rows.setIcon(icon12)
    icon13 = QIcon()
    icon13.addPixmap(QPixmap(os.path.join(SCRIPT_DIR,"icons","water.ico")), QIcon.Normal,QIcon.Off)
    self.qp_view.setIcon(icon13)
    icon14 = QIcon()
    icon14.addPixmap(QPixmap(os.path.join(SCRIPT_DIR,"icons","long.ico")), QIcon.Normal,QIcon.Off)
    self.ls_view.setIcon(icon14)
    icon15 = QIcon()
    icon15.addPixmap(QPixmap(os.path.join(SCRIPT_DIR,"icons","map.ico")), QIcon.Normal,QIcon.Off)
    self.lp_view.setIcon(icon15)
    
    #set table sizes for nicer display
    self.coords_table.horizontalHeader().setSectionResizeMode(QHeaderView.Stretch)
    self.wsp_table.horizontalHeader().setSectionResizeMode(QHeaderView.Stretch)
    
    self.p_wspdat.horizontalHeader().setSectionResizeMode(0,QHeaderView.Stretch)
    for i in range(5):
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