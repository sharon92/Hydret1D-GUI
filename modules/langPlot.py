# -*- coding: utf-8 -*-
'''
Created on Wed Jun 26 11:49:02 2019

@author: s.Shaji
'''

import os
import pickle
import sys
import numpy           as     np
import pyqtgraph       as     pg
from functools         import partial
from itertools         import groupby
from dialogs.plotPropL import changePropsL
from modules.riverbed  import riv_bed,cal_bank
from modules.plot3d    import plot3d
from PyQt5.QtWidgets   import QTableWidgetItem,QApplication
from PyQt5.QtGui       import QColor,QFont

# =============================================================================
# Initiate plotting, Plot all 4 Graphic windows and then update the data using
# update functions
# =============================================================================
SCRIPT_DIR = os.path.dirname(os.path.realpath(sys.argv[0]))

#strickler color gradient
kst_cd    = {}
kst_color = []

for i in range(255,0,-10):
    kst_color.append(QColor(i,255,0))
for i in range(0,256,10):
    kst_color.append(QColor(0,255,i))
for i in range(0,256,10):
    kst_color.append(QColor(255,i,0))
for i in range(0,256,-10):
    kst_color.append(QColor(0,i,255))
for n,i in enumerate(np.arange(0,len(kst_color)/2.,0.5)):
    kst_cd[i] = kst_color[n]

font = QFont()
font.setBold(True)
font.setPointSize(12)

sfont = QFont()
sfont.setBold(True)
sfont.setPointSize(10)

def plotcols(self):
    dpath = os.path.join(SCRIPT_DIR,'defaults','default_dict.pickle')
    pickle_in  = open(dpath,'rb')
    self.plotdefaults = pickle.load(pickle_in)
    pickle_in.close()

'''Plot the longitudnal section here'''
def langPlot(self):
    i=self.gewid_current
    self.langView.clear()
    self.langView.addLegend(offset = (-30,30))

    pw = self.plotdefaults['w'][3]
    pc = self.plotdefaults['w'][0]
    ps = self.penstyle[self.plotdefaults['w'][2]]
    wc = QColor(0,255,255,150)
    
    idx = (self.df_start['ID'] == i)
    
    '''sohle plot'''
    x = self.df_start['XL'].values[idx]
    y = self.df_start['ZO'].values[idx]
    pts = np.array(sorted(list(zip(x,y))))
    self.lsplot = self.langView.plot(pts, pen = pg.mkPen(color=self.plotdefaults['lp'][0],
                                                         width=self.plotdefaults['lp'][3],
                                                         style=self.penstyle[self.plotdefaults['lp'][2]]),
                                     fillLevel=np.nanmin(y)-self.plotdefaults['lf'][3],
                                     brush=self.plotdefaults['lf'][0],name='Flussbett')
    self.lsplot.setZValue(1000)
    self.lw_dictL['sohle'][2]    = self.lsplot
    self.lwopt_dictL['sohle']    = self.lsplot.opts
    
    '''anfangs wsp plot'''
    wsp_l = self.df_start['HZERO'].values[idx]
    self.anfangs_wspL = self.langView.plot(x,wsp_l,
                                 pen=pg.mkPen(width=2,color='m',style=2),name = 'Anfangs WSP')
    self.anfangs_wspL.setZValue(1)
    self.lw_dictL['Anfangs WSP'][2]     = self.anfangs_wspL
    self.lwopt_dictL['Anfangs WSP']     = self.anfangs_wspL.opts
    
    '''instat plot'''
    wsp = self.hz_df[self.lzeitslider.value()].values[idx]
    self.wspL = self.langView.plot(x,wsp,pen=pg.mkPen(width=pw,color=pc,style=ps),
                                   fillLevel=np.nanmin(y)-self.plotdefaults['lf'][3],
                                   brush =wc,name = 'instationär WSP')
    self.wspL.setZValue(0)
    self.lw_dictL['instationär WSP'][2] = self.wspL
    self.lwopt_dictL['instationär WSP'] = self.wspL.opts

    '''max plot'''
    wspmax = self.hz_df.transpose().max()[idx].values
    self.wspmaxL = self.langView.plot(x,wspmax,pen=pg.mkPen(width=2,color='b',style=2),
                                      name = 'max WSP')
    self.wspmaxL.setZValue(2)
    self.lw_dictL['max WSP'][2]         = self.wspmaxL
    self.lwopt_dictL['max WSP']         = self.wspmaxL.opts
 
    '''min plot'''
    wspmin = self.hz_df.transpose().min()[idx].values
    self.wspminL = self.langView.plot(x,wspmin,pen=pg.mkPen(width=2,color='r',style=2),
                                      name = 'min WSP')
    self.wspminL.setZValue(1)
    self.lw_dictL['min WSP'][2]         = self.wspminL
    self.lwopt_dictL['min WSP']         = self.wspminL.opts
    
    '''wsp-rauheit stufen'''
    wr_dict= { 'HR-1' :self.df_start['HZERO'].values[idx],
               'RNV-1':self.df_start['RNI'].values[idx] }
    for n,wstufe,rstufe in enumerate(zip(list(self.lw_dictL.keys())[6::2],list(self.lw_dictL.keys())[7::2])):
        h = self.df_start['HR'+str(n)].values[idx]
        r = self.df_start['RNI'].values[idx] if n== 0 else self.df_start['RNV'+str(n)].values[idx]
        
        novals = np.where(h==0)[0]
        if len(novals)>1:
            h[novals] = wr_dict['HR' +str(n-1)][novals]
            r[novals] = wr_dict['RNV'+str(n-1)][novals]
            wr_dict['HR' +str(n)]=h
            wr_dict['RNV'+str(n)]=r

    '''Q-Längsschnitt'''
    # if self.qls_check.isChecked():
        
    
    print('yo wtf')
    self.langView.invertX(True)
    self.langView.showGrid(x=True,y=True)
    self.langView.showAxis('right')
    self.langView.showAxis('top')
    self.langView.getAxis('left').setLabel('Höhe')
    self.langView.getAxis('right').setLabel('Höhe')
    self.langView.getAxis('bottom').setLabel('Station [m]')
    self.langView.getAxis('top').setLabel('Station [m]')

    pitems = [self.lsplot,self.wspL,self.wspmaxL,self.wspminL]
    
    self.langView.getViewBox().autoRange(items=pitems)

def l_looper(self):
    if self.play_lzeit.isChecked():
        self.timer = pg.QtCore.QTimer(self)
        self.timer.timeout.connect(partial(looplzeit,self))
        self.timer.start(1000/self.fps.value())
    else:
        if hasattr(self,'timer'):  self.timer.stop()

def fps_changed(self):
    if self.play_lzeit.isChecked():
        self.play_lzeit.setCheckState(0)
        self.play_lzeit.setCheckState(2)

def dt_changed(self,dt):
    m = self.show_simdatetime.minimumDateTime().toPyDateTime()
    c = dt.toPyDateTime()
    DT = (c-m).total_seconds()/60
    idx = np.abs(np.arange(self.h1d.lead)*self.h1d.tinc-DT).argmin()
    self.lzeitslider.setValue(idx)
    
def looplzeit(self):
    spos = self.lzeitslider.value()
    smax = self.lzeitslider.maximum()
    self.lzeitlcd.display(spos+1)
    
    self.show_simdatetime.blockSignals(True)
    mindt = self.show_simdatetime.minimumDateTime()
    currentdt = mindt.addSecs(spos*self.h1d.tinc*60)
    self.show_simdatetime.setDateTime(currentdt)
    self.show_simdatetime.blockSignals(False)
    
    '''Längsschnitt'''
    gi    = self.gewid_current
    idxS  = (self.df_start['ID'] == gi)
    stat  = self.df_start['XL'].values[idxS]
    wsp   = self.hz_df[spos].values[idxS]
    self.wspL.setData(stat,wsp)
    
    if hasattr(self,'timer'):
        if self.timer.isActive():
            self.lzeitslider.blockSignals(True)
            spos +=1
            if spos> smax: spos = 0
            self.lzeitslider.setValue(spos)
            self.lzeitslider.blockSignals(False)
    
def langPlotUpdate(self,i):
    pitem = self.lw_dictL[i.text()]
    if i.checkState() == 0:
        pitem[2].hide()
        self.langView.plotItem.legend.removeItem(pitem[2])
    elif i.checkState() == 2:
        pitem[2].show()
        self.langView.plotItem.legend.addItem(pitem[2],pitem[2].name())
        
def langPlotBeautify(self,i):
    p = self.lw_dictL[i.text()][2]

    ask = changePropsL(self,i.text())
    if ask is not None:
        pen,symbol,symbolPen,symbolBrush,symbolSize,fillLevel,fillBrush = ask
        p.opts['pen']         = pen
        p.opts['symbol']      = symbol
        p.opts['symbolPen']   = symbolPen
        p.opts['symbolBrush'] = symbolBrush
        p.opts['symbolSize']  = symbolSize
        if fillLevel:
            p.opts['fillLevel']   = self.lw_dictL['sohle'][2].getData()[1].min()
            p.opts['fillBrush']   = fillBrush
        else:
            p.opts['fillLevel']   = None
            p.opts['fillBrush']   = None
        p.update()
        p.updateItems()
        self.lwopt_dictL[i.text()] = p.opts
            
# =============================================================================
# Update all the plotting data here
# =============================================================================
'''Under Editing Mode, Update the ROI'''
def plotROI(self):
    self.editable_schnitt.setPoints(np.vstack((self.Node.X,self.Node.Y)).T)
    self.editable_schnitt.setZValue(50001)

def updateROI(self):
    self.editable_schnitt.blockSignals(True)
    self.editable_schnitt.setPoints(np.vstack((self.qplotD['node'][0].X,self.qplotD['node'][0].Y)).T)
    self.editable_schnitt.blockSignals(False)

def ulangPlot(self):
    i=int(self.lang_ID.currentText())
    self.gewid_current= i
    idx = (self.df_start['ID'] == i)
    
    x = self.df_start['XL'].values[idx]
    y = self.df_start['ZO'].values[idx]
    pts = np.array(sorted(list(zip(x,y))))
    self.lsplot.setData(pts,fillLevel=np.nanmin(y)-self.plotdefaults['lf'][3])
    
    wsp_l = self.df_start['HZERO'].values[idx]
    self.anfangs_wspL.setData(x,wsp_l)
    pitems = [self.lsplot,self.anfangs_wspL]
    
    if hasattr(self,'hz_df'):
        wsp = self.hz_df[self.lzeitslider.value()].values[idx]
        self.wspL.setData(x,wsp,fillLevel=np.nanmin(y)-self.plotdefaults['lf'][3])
        
        wspmin = self.hz_df.transpose().min()[idx].values
        self.wspminL.setData(x,wspmin)
    
        wspmax = self.hz_df.transpose().max()[idx].values
        self.wspmaxL.setData(x,wspmax)
        pitems = [self.lsplot,self.anfangs_wspL,self.wspL,self.wspmaxL,self.wspminL]
        
    self.langView.getViewBox().autoRange(items=pitems)
    plot3d(self)
# =============================================================================
#Highlighter Functions to show whats being viewed presently
# =============================================================================

'''Mark the selected Coordinates from Coordinates table into the Main Cross-section View'''
def xyMarker(self):
    self.row_c_idx = sorted(set([i.row() for i in self.coords_table.selectedIndexes()]))
    if self.Edit:
        self.delete_rows.setEnabled(True)
        color = 'y'
    else:color = 'b'

    Node = self.qplotD['node'][0]
    xs,ys = Node['X'][self.row_c_idx],Node['Y'][self.row_c_idx]
    
    if self._rquer.isChecked():
        if hasattr(self,'selection'):self.selection.setData(xs,ys,symbolPen=color,symbolBrush=color)
        else:
            self.selection = self.graphicsView.plot(xs,ys,pen=None,symbol='o',
                                                    symbolSize=10,symbolPen=color,symbolBrush=color)
            self.selection.setZValue(2000)

def xyUnmark(self,event):
    try: self.selection.clear()
    except: pass

# =============================================================================
# Update plots under Editing Mode
# =============================================================================
       
def undo_but(self):
    self.editable_schnitt.blockSignals(True)
    self.changes -=1
    self.df_copy.update(self.df_db[self.changes])
    
    update_nodes(self)
    uqplots(self,plist = [0],ROI=True)
    update_data(self)
    
    if self.changes == 0:
        self.undo.setEnabled(False)
    self.redo.setEnabled(True)
    self.editable_schnitt.blockSignals(False)

def redo_but(self):
    self.editable_schnitt.blockSignals(True)
    self.changes +=1
    self.df_copy.update(self.df_db[self.changes])
    
    update_nodes(self)
    uqplots(self,plist = [0],ROI=True)
    update_data(self)
    
    if self.changes == len(self.df_db)-1:
        self.redo.setEnabled(False)
    if self.changes > 0:
        self.undo.setEnabled(True)
    self.editable_schnitt.blockSignals(False)

def plot_update_coords(self):
    self.coords_table.blockSignals(True)
    if not len(self.df_db) == self.changes + 1:
        self.df_db = self.df_db[:self.changes+1]
        self.redo.setEnabled(False)
    self.changes +=1
    update_data(self,src='table')

    self.df_copy.update(self.df_db[self.changes])
    update_nodes(self)
    uqplots(self,plist = [0],ROI=False)
    self.undo.setEnabled(True)
    self.coords_table.blockSignals(False)

def update_schnitt(self):
    self.coords_table.blockSignals(True)
    _pts = self.editable_schnitt.getState()['points']
    self.qplotD['node'][0].Npoints =len(_pts)
    self.Punkte_label.display(len(_pts))
    self.coords_table.setRowCount(len(_pts))
    self.qplotD['node'][0].X = np.array([round(xi.x(),3) for xi in self.editable_schnitt.getState()['points']])
    self.qplotD['node'][0].Y = np.array([round(yi.y(),3) for yi in self.editable_schnitt.getState()['points']])
    for _i in range(len(_pts)):
        self.coords_table.setItem(_i,0,QTableWidgetItem(str(self.qplotD['node'][0].X[_i])))
        self.coords_table.setItem(_i,1,QTableWidgetItem(str(self.qplotD['node'][0].Y[_i])))
    uqplots(self,plist = [0],ROI=False)
#    updateROI(self)
    self.coords_table.blockSignals(False)

def update_data(self,src = 'db'):
    if src=='db':
        self.coords_table.blockSignals(True)
        _pts = self.qplotD['node'][0].Npoints
        self.Punkte_label.display(_pts)
        self.coords_table.setRowCount(_pts)
        for i in range(_pts):
            self.coords_table.setItem(i,0,QTableWidgetItem(str(self.qplotD['node'][0].X[i])))
            self.coords_table.setItem(i,1,QTableWidgetItem(str(self.qplotD['node'][0].Y[i])))
        self.coords_table.blockSignals(False)
    elif src == 'table':
        _pts = self.coords_table.rowCount()
        self.Punkte_label.display(_pts)
        x = np.array([float(self.coords_table.item(i,0).text()) for i in range(_pts)])
        y = np.array([float(self.coords_table.item(i,1).text()) for i in range(_pts)])
        df = self.df_copy.copy()
        if self._rquer.isChecked():
            df.iat[self.iloc,0] = len(x)
            df.iat[self.iloc,6] = x
            df.iat[self.iloc,7] = y
        elif self._rrau.isChecked():
            df.iat[self.qplotD['riloc'][0],0] = len(x)
            df.iat[self.qplotD['riloc'][0],6] = x
            df.iat[self.qplotD['riloc'][0],7] = y
        elif self._rschalter.isChecked():
            df.iat[self.qplotD['siloc'][0],0] = len(x)
            df.iat[self.qplotD['siloc'][0],6] = x
            df.iat[self.qplotD['siloc'][0],7] = y
        self.df_db.append(df)

# =============================================================================
# Mouse Movements on plots
# =============================================================================
    
def pointer_lang(self,evt):
    try:
        mousePoint = self.langView.getViewBox().mapSceneToView(evt)
        xi = mousePoint.x()
        yi = mousePoint.y()
        gid = int(self.lang_ID.currentText())
        gdx = (self.df_start['ID'] == gid)

        pt    = np.array([xi,yi])
        nodes = np.array(list(zip(self.df_start['XL'][gdx].values,self.df_start['ZO'][gdx].values)))
        dist  = np.linalg.norm(nodes - pt, ord=2, axis=1)
        
        nearest = sorted(list(zip(dist,self.df_start['XL'][gdx].values,self.df_start.index[gdx])))[0]
        
        if nearest[0] < 1.5:
            val = nearest[1]
            idx = nearest[2]
            
            sohle = self.df_start.loc[idx]['ZO']
            try:
                t = "{:>} {:>}\n".format('Schnitt:',self.df_pro.loc[idx]['PName'])
            except:
                t = '-\n'
            text  = t+"{:>} {:>}".format('Knoten:',idx)
            
            self.lang_label.show()
            self.lang_label.setText(text)
            self.lang_label.setPos(val,sohle)
        else:
            self.lang_label.hide()
    except:pass