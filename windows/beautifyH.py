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

from dialogs.nodeRenum    import renumber
from dialogs.startEditor  import spreadwin
from dialogs.profLamellen import raumode
from dialogs.runHydret    import runModel
from dialogs.inflowdat    import datshow
from dialogs.settings     import settingswin
from dialogs.outViewer    import ergebnisViewer
from dialogs.textViewer   import textEditor
from dialogs.dbfEditor    import shpViewer
from dialogs.wqGraph      import wqDocker
from dialogs.HQExport     import HQPopup
from modules.loaddata     import dataadd,dataremove
from modules.plot3d       import plot3d,color_change_3d
from modules.savefiles    import (saveProject,
                                  saveHyd,
                                  saveStart,
                                  saveRun,
                                  bwToExcel,
                                  pro2shp,
                                  shp2pro)
from modules.plotting     import (updateViews,
                                  plot_update_coords,changeAR,
                                  plan_name,ulangPlot,
                                  xyMarker,xyUnmark,
                                  _handlecopy,
                                  _handlepaste,
                                  pointer_q1,pointer_q2,
                                  pointer_lang,
                                  looplzeit,fps_changed,
                                  l_looper,dt_changed,
                                  langPlotUpdate,langPlotBeautify)
from modules.nodePlot     import (nodeBeauty,nodePlot,nodesVisibility,
                                  nodePlotZ,pointer_node,nodePointer)
from modules.loadhyd      import (inflowNodes,
                                  printCurves,
                                  idown_change,
                                  normq,
                                  defWeirs,
                                  defGates,
                                  defJunctions,
                                  lateralInflows,
                                  ovfmode)
from windows.ProfilWindow import ProfW
from windows.lsWindow     import lsW

SCRIPT_DIR = os.path.dirname(os.path.realpath(sys.argv[0]))

def connections(self):
    
    #Menu commands
    self.openp.triggered.connect(self.OpenEnv)
    self.saveprojekt.triggered.connect(partial(saveProject,self))
    self.savehyd.triggered.connect(partial(saveHyd,self))
    self.savestart.triggered.connect(partial(saveStart,self))
    self.saverun.triggered.connect(partial(saveRun,self))
    if os.path.isfile(os.path.join(SCRIPT_DIR,'defaults','default_dict.pickle')):
        self.dpath = os.path.join(SCRIPT_DIR,'defaults','default_dict.pickle')
    else:
        self.dpath = 'None'
    self.settings.triggered.connect(partial(settingswin,self,self.dpath))
    self.checkver.triggered.connect(partial(self.checkForUpdates,clicked=True))
    self.proEditor.triggered.connect(partial(show_pro_window,self))
    self.rauEditor.triggered.connect(partial(show_rau_window,self))
    self.reload.triggered.connect(self.reloadModel)
    self.lk_view.triggered.connect(partial(lkdockv,self))
    self.closeall.triggered.connect(self.close)
    
    #dialog windows
    self.node_renumber.triggered.connect(partial(renumber,self))
    self.run.triggered.connect(partial(runModel,self))
    
    #convert
    self.p2s.triggered.connect(partial(pro2shp,self))
    self.s2p.triggered.connect(partial(shp2pro,self))
    
    #exports
    self.HQ_excel.triggered.connect(partial(HQPopup,self))
    self.BW_excel.triggered.connect(partial(bwToExcel,self))
    
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
    self.p_wsg_view.clicked.connect(partial(datshow,self,num=-3,idx=None))
    self.p_abg_view.clicked.connect(partial(datshow,self,num=-4,idx=None))
    
    #Profil Props
    self.qschnp  = ['VK','RK','VF','RF','DF','OF','H2','RS','ZS']
    self.qschnt  = [1,2,3,4,5,6,7,8,9]
    self.symbols = ['h','s','p','o','+','d','star'] 
    self.penstyle= [Qt.SolidLine,Qt.DashLine,Qt.DotLine,Qt.DashDotLine,Qt.DashDotDotLine]
    
    #data props
    self.timmod = ['IN','IM','ST','MA']
    self.latcom_schalt = ['0','1','2']
    
    self.knotenNr.currentIndexChanged.connect(self.idChange)
    self.station_label.currentIndexChanged.connect(self.idChange)
    self.schnittName_label.currentIndexChanged.connect(self.idChange)
    self.lang_ID.currentTextChanged.connect(partial(ulangPlot,self))
    self.coords_table.itemSelectionChanged.connect(partial(xyMarker,self))
    self.coords_table.itemChanged.connect(partial(plot_update_coords,self))
    
    #loading data (shapefiles)
    self.lp_adddata.clicked.connect(partial(dataadd,self))
    self.lp_removedata.clicked.connect(partial(dataremove,self))
    
    #plotting
    self.AspectRatio.valueChanged.connect(partial(changeAR,self))
    self.arbox.toggled.connect(partial(changeAR,self))
    self.p_plan.editingFinished.connect(partial(plan_name,self))
    self.langView.scene().sigMouseMoved.connect(partial(pointer_lang,self))
    self.nodeView.scene().sigMouseMoved.connect(partial(pointer_node,self))
    
    #längsschnitt und querschnitt viewer
    self.ls_listWidget.itemDoubleClicked.connect(partial(langPlotBeautify,self))
    self.ls_listWidget.itemChanged.connect(partial(langPlotUpdate,self))

    #3d viewer
    # self.gid3d.currentIndexChanged.connect(partial(plot3d,self))
    self.colorMap.currentIndexChanged.connect(partial(plot3d,self))
    self.color_dis.valueChanged.connect(partial(plot3d,self))
    self.drawEdges.stateChanged.connect(partial(plot3d,self))
    self.drawFaces.stateChanged.connect(partial(plot3d,self))
    self.maxwsp3d.stateChanged.connect(partial(plot3d,self))
    
    #lageplan widget
    self.lp_listWidget.itemDoubleClicked.connect(partial(nodeBeauty,self))
    self.lp_listWidget.itemChanged.connect(partial(nodesVisibility,self))
    model = self.lp_listWidget.model()
    model.rowsMoved.connect(partial(nodePlotZ,self))
    
    self.graphicsView.scene().sigMouseClicked.connect(partial(xyUnmark,self))
    self.nodeView.scene().sigMouseClicked.connect(partial(nodePointer,self))
    self.graphicsView.scene().sigMouseMoved.connect(partial(pointer_q1,self))
    self.graphicsView2.scene().sigMouseMoved.connect(partial(pointer_q2,self))
    self.graphicsView.getViewBox().sigResized.connect(partial(updateViews,self))
    
    #sliders
    self.play_lzeit.stateChanged.connect(partial(l_looper,self))
    self.fps.valueChanged.connect(partial(fps_changed,self))
    self.lzeitslider.valueChanged.connect(partial(looplzeit,self))
    self.sim_datetime.dateTimeChanged.connect(self.updateDateTimeRange)
    self.show_simdatetime.dateTimeChanged.connect(partial(dt_changed,self))

#    connect radio buttons
    self.buttonGroup.buttonToggled.connect(partial(nodePlot,self))
    self.buttonGroup_2.buttonToggled.connect(partial(nodePlot,self))
    self.buttonGroup_4.buttonToggled.connect(self.toggle)
    
    #under editing mode
    self.copy_clip.clicked.connect(partial(_handlecopy,self))
    
    #file browsers
    self.browse_fstart.clicked.connect(partial(spreadwin,self))
    self.browse_fpro.clicked.connect(partial(textEditor,self,F='PRO'))
    self.browse_hyd.clicked.connect(partial(textEditor,self,F='HYD'))
    self.browse_run.clicked.connect(partial(textEditor,self,F='RUN'))
    self.browse_achse.clicked.connect(partial(shpViewer,self))
    self.browse_out.clicked.connect(partial(textEditor,self,F='OUT'))
    self.browse_qch.clicked.connect(partial(ergebnisViewer,self,F='Q'))
    self.browse_hch.clicked.connect(partial(ergebnisViewer,self,F='H'))

    #docks
    self.statusbar.messageChanged.connect(partial(logging,self))
    self.qp_view.triggered.connect(partial(qdockv,self))
    self.lp_view.triggered.connect(partial(lpdockv,self))
    self.ls_view.triggered.connect(partial(lsdockv,self))
    self.wq_view.triggered.connect(partial(wqDocker,self))
    
def initiateBeautify(self):

    #icons
    icon = QIcon()
    icon.addPixmap(QPixmap(os.path.join(SCRIPT_DIR,"icons","hydret1D.ico")), QIcon.Normal,QIcon.Off)
    self.setWindowIcon(icon)

    icon6 = QIcon()
    icon6.addPixmap(QPixmap(os.path.join(SCRIPT_DIR,"icons","new.ico")), QIcon.Normal,QIcon.Off)
    self.newp.setIcon(icon6)
    icon7 = QIcon()
    icon7.addPixmap(QPixmap(os.path.join(SCRIPT_DIR,"icons","open32.ico")), QIcon.Normal,QIcon.Off)
    self.openp.setIcon(icon7)
    icon8 = QIcon()
    icon8.addPixmap(QPixmap(os.path.join(SCRIPT_DIR,"icons","savemodell.bmp")), QIcon.Normal,QIcon.Off)
    self.saveprojekt.setIcon(icon8)
    icon9 = QIcon()
    icon9.addPixmap(QPixmap(os.path.join(SCRIPT_DIR,"icons","runmodel.ico")), QIcon.Normal,QIcon.Off)
    self.run.setIcon(icon9)
    icon10 = QIcon()
    icon10.addPixmap(QPixmap(os.path.join(SCRIPT_DIR,"icons","copy.ico")), QIcon.Normal,QIcon.Off)
    self.copy_clip.setIcon(icon10)
    icon13 = QIcon()
    icon13.addPixmap(QPixmap(os.path.join(SCRIPT_DIR,"icons","water.ico")), QIcon.Normal,QIcon.Off)
    self.qp_view.setIcon(icon13)
    icon14 = QIcon()
    icon14.addPixmap(QPixmap(os.path.join(SCRIPT_DIR,"icons","long.ico")), QIcon.Normal,QIcon.Off)
    self.ls_view.setIcon(icon14)
    icon15 = QIcon()
    icon15.addPixmap(QPixmap(os.path.join(SCRIPT_DIR,"icons","map.ico")), QIcon.Normal,QIcon.Off)
    self.lp_view.setIcon(icon15)
    icon17 = QIcon()
    icon17.addPixmap(QPixmap(os.path.join(SCRIPT_DIR,"icons","save_hyd.bmp")), QIcon.Normal,QIcon.Off)
    self.savehyd.setIcon(icon17)
    icon18 = QIcon()
    icon18.addPixmap(QPixmap(os.path.join(SCRIPT_DIR,"icons","visibility.ico")), QIcon.Normal,QIcon.Off)
    self.p_wsg_view.setIcon(icon18)
    icon19 = QIcon()
    icon19.addPixmap(QPixmap(os.path.join(SCRIPT_DIR,"icons","visibility.ico")), QIcon.Normal,QIcon.Off)
    self.p_abg_view.setIcon(icon19)
    icon20 = QIcon()
    icon20.addPixmap(QPixmap(os.path.join(SCRIPT_DIR,"icons","adddata.ico")), QIcon.Normal,QIcon.Off)
    self.lp_adddata.setIcon(icon20)
    icon21 = QIcon()
    icon21.addPixmap(QPixmap(os.path.join(SCRIPT_DIR,"icons","delete.ico")), QIcon.Normal,QIcon.Off)
    self.lp_removedata.setIcon(icon21)
    icon22 = QIcon()
    icon22.addPixmap(QPixmap(os.path.join(SCRIPT_DIR,"icons","savestart.bmp")), QIcon.Normal,QIcon.Off)
    self.savestart.setIcon(icon22)
    icon23 = QIcon()
    icon23.addPixmap(QPixmap(os.path.join(SCRIPT_DIR,"icons","saverun.bmp")), QIcon.Normal,QIcon.Off)
    self.saverun.setIcon(icon23)
    icon24 = QIcon()
    icon24.addPixmap(QPixmap(os.path.join(SCRIPT_DIR,"icons","settings.ico")), QIcon.Normal,QIcon.Off)
    self.settings.setIcon(icon24)
    icon25 = QIcon()
    icon25.addPixmap(QPixmap(os.path.join(SCRIPT_DIR,"icons","save.ico")), QIcon.Normal,QIcon.Off)
    self.menuSave.setIcon(icon25)
    icon26 = QIcon()
    icon26.addPixmap(QPixmap(os.path.join(SCRIPT_DIR,"icons","resume.bmp")), QIcon.Normal,QIcon.Off)
    self.play_lzeit.setIcon(icon26)
    icon27 = QIcon()
    icon27.addPixmap(QPixmap(os.path.join(SCRIPT_DIR,"icons","wq.png")), QIcon.Normal,QIcon.Off)
    self.wq_view.setIcon(icon27)
    icon28 = QIcon()
    icon28.addPixmap(QPixmap(os.path.join(SCRIPT_DIR,"icons","reload.ico")), QIcon.Normal,QIcon.Off)
    self.reload.setIcon(icon28)
    icon29 = QIcon()
    icon29.addPixmap(QPixmap(os.path.join(SCRIPT_DIR,"icons","nodes.png")), QIcon.Normal,QIcon.Off)
    self.lk_view.setIcon(icon29)
    icon30 = QIcon()
    icon30.addPixmap(QPixmap(os.path.join(SCRIPT_DIR,"icons","proEditor.png")), QIcon.Normal,QIcon.Off)
    self.proEditor.setIcon(icon30)
    icon31 = QIcon()
    icon31.addPixmap(QPixmap(os.path.join(SCRIPT_DIR,"icons","rauEditor.png")), QIcon.Normal,QIcon.Off)
    self.rauEditor.setIcon(icon31)
    #set table sizes for nicer display
    self.coords_table.horizontalHeader().setSectionResizeMode(QHeaderView.Stretch)
    self.wsp_table.horizontalHeader().setSectionResizeMode(QHeaderView.Stretch)
    
    self.p_weir_table.horizontalHeader().setSectionResizeMode(9,QHeaderView.Stretch)
    for i in range(9):
        self.p_weir_table.horizontalHeader().setSectionResizeMode(i,QHeaderView.ResizeToContents)
        
    for i in [*range(11),12]:
        self.p_gate_table.horizontalHeader().setSectionResizeMode(i,QHeaderView.ResizeToContents)
    self.p_gate_table.horizontalHeader().setSectionResizeMode(11,QHeaderView.Stretch)
    
    self.p_lie_table.horizontalHeader().setSectionResizeMode(1,QHeaderView.Stretch)
    for i in [0,2,3,4,5]:
        self.p_lie_table.horizontalHeader().setSectionResizeMode(i,QHeaderView.ResizeToContents)

    self.p_nxj_table.horizontalHeader().setSectionResizeMode(QHeaderView.Stretch)
    self.p_dxlgam_table.horizontalHeader().setSectionResizeMode(QHeaderView.Stretch)
    
    self.p_nupe.horizontalHeader().setSectionResizeMode(1,QHeaderView.Stretch)
    for i in [0,2,3,4]:
        self.p_nupe.horizontalHeader().setSectionResizeMode(i,QHeaderView.ResizeToContents)

def logging(self,i):
#    if not i == 'Ready':self.logWindow.insertPlainText(i+'\n')
    self.logWindow.insertPlainText(i+'\n')
    
def qdockv(self):
    if not self.qp_dock.isVisible():self.qp_dock.setVisible(True)
        
def lpdockv(self):
    if not self.lp_dock.isVisible():self.lp_dock.setVisible(True)
    self.nodemapview.setChecked(True)
        
def lkdockv(self): 
    if not self.lp_dock.isVisible():self.lp_dock.setVisible(True)
    self.nodeplanview.setChecked(True)
        
def lsdockv(self):
    if not self.ls_dock.isVisible():self.ls_dock.setVisible(True)

def show_pro_window(self):
    if hasattr(self,'proWindow'):
        self.proWindow.close()
    if hasattr(self,'h1d'):
        if hasattr(self.h1d, 'propath'):
            self.proWindow=ProfW(self.version,propath=self.h1d.propath,sdf = self.df_start)
        else:
            self.proWindow = ProfW(self.version)
    else:
        self.proWindow = ProfW(self.version)
    self.proWindow.show()

def show_rau_window(self):
    if hasattr(self,'lsWindow'):
        self.lsWindow.close()
    if hasattr(self,'h1d'):
        self.lsWindow = lsW(self.version,self)
        self.lsWindow.show()