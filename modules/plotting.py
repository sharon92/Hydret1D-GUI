# -*- coding: utf-8 -*-
"""
Created on Wed Jun 26 11:49:02 2019

@author: s.Shaji
"""

import numpy as np
import pandas as pd
import pyqtgraph as pg
import shapefile as shp
from shapely.geometry import LineString
from modules.loaddata import loadshp
from modules.riverbed import riv_bed,cal_bank
from PyQt5.QtWidgets import QTableWidgetItem,QFileDialog,QColorDialog
from PyQt5.QtCore import Qt
from PyQt5.QtGui import QColor

# =============================================================================
# Initiate plotting, Plot all 4 Graphic windows and then update the data using
# update functions
# =============================================================================

'''Plot the Main Cross-section View here'''
def qplot1(self):

    Node = self.Node
    View = self.graphicsView
    View.clear()
    riv_bed_y,riv_bed_idx,riv_bed_x = riv_bed(Node)
    self.plot_bottom                     = riv_bed_y-2
    
    View.addLegend()
    
    self.ax1 = View.plot(Node['X'],Node['Y'],pen='k',symbol='d',
                                   symbolSize=7,symbolPen='r',symbolBrush='r',
                                   fillLevel=self.plot_bottom,brush=(195,176,145),
                                   name="Querschnitt")
    
    self.ax1.setZValue(1000)
    self.axViewBox1 = self.ax1.getViewBox()
    self.axViewBox1.setAspectLocked(lock=True, ratio=self.ratio)
    View.getAxis('left').setLabel('Höhe')

    #plot lamellen
    if -1*Node.name in self.df_pro.index:
        View.showAxis('right')
        View.scene().addItem(self.p2)
        self.p2.setGeometry(View.getViewBox().sceneBoundingRect())
        self.p2.linkedViewChanged(View.getViewBox(), self.p2.XAxis)
        View.getAxis('right').linkToView(self.p2)
        self.p2.setXLink(View)
        View.getAxis('right').setLabel('kst', color='#0000ff')
        self.p2.setYRange(-100,100)
        self.rau_ax1 = View.plot(self.Node_R['X'],self.Node_R['Y'],pen='k',
                                   symbolSize=3,symbolPen=0.5,symbolBrush=0.5,
                                   fillLevel=self.Node_R['Y'].min(),brush=0.5,name='k-strickler')
        self.rau_ax1.setZValue(10001)
        self.p2.addItem(self.rau_ax1)
    else:
        try:
            self.rau_ax1.scene().removeItem(self.rau_ax1)
        except:
            pass
        View.showAxis('right',show=False)
    
    '''Annotate Info'''
    Text = textgen(Node,self)
    self.annotate1 = pg.TextItem(Text,anchor=(1,0.5),color='k',border='k',fill='w')
    View.addItem(self.annotate1)
    self.annotate1.setPos(Node.X.max(),Node.Y.max()+4)
    
    '''River Bed'''
    self.riverbed1 = View.plot([riv_bed_x,riv_bed_x],[riv_bed_y,self.plot_bottom],
                                           pen=pg.mkPen(width=1,color='b'))
    self.sohle1 = pg.TextItem(text='Tiefpunkt',anchor=(0.5,0.5),color='b',border='k',fill='w')
    View.addItem(self.sohle1)
    self.sohle1.setPos(riv_bed_x,self.plot_bottom)
    self.sohle1.setZValue(4000) 
    self.riverbed1.setZValue(3000)
    plan_name(self)
    LOB_x,LOB_y,ROB_x,ROB_y,idx_l,idx_r = cal_bank(Node,return_idx=True)
    bankXY = LOB_x,LOB_y,ROB_x,ROB_y
    plot_bank(self,bankXY,self.plot_bottom,Node['Mode'],Node,View)

    '''Pointer'''
    self.quer_label =pg.TextItem(color='k')
    self.graphicsView.addItem(self.quer_label)
    self.quer_label.setPos(Node.X[0],Node.Y[0])
    self.quer_label.setZValue(10000)
    
'''Plot the Secondary Cross-section View here'''
def qplot2(self):
    Node = self.Node2
    View = self.graphicsView2
    View.clear()
    riv_bed_y,riv_bed_idx,riv_bed_x = riv_bed(Node)
    plot_bottom                     = riv_bed_y-2

    View.addLegend()

    self.ax2 = View.plot(Node['X'],Node['Y'],pen='k',symbol='d',
                                   symbolSize=7,symbolPen='r',symbolBrush='r',
                                   fillLevel=plot_bottom,brush=(195,176,145),
                                   name="Querschnitt")
    self.ax2.setZValue(1000)
    self.axViewBox2 = self.ax2.getViewBox()
    self.axViewBox2.setAspectLocked(lock=True, ratio=self.ratio)
    View.getAxis('left').setLabel('Höhe')

    '''Annotate Info'''
    Text = textgen(Node,self)
    self.annotate2 = pg.TextItem(Text,anchor=(1,0.5),color='k',border='k',fill='w')
    View.addItem(self.annotate2)
    self.annotate2 .setPos(Node.X.max(),Node.Y.max()+4)
    
    '''River Bed'''
    self.riverbed2 = View.plot([riv_bed_x,riv_bed_x],[riv_bed_y,plot_bottom],
                                           pen=pg.mkPen(width=1,color='b'))
    self.sohle2 = pg.TextItem(text='Tiefpunkt',anchor=(0.5,0.5),color='b',border='k',fill='w')
    View.addItem(self.sohle2)
    self.sohle2.setPos(riv_bed_x,plot_bottom)
    self.sohle2.setZValue(4000) 
    self.riverbed2.setZValue(3000)
    plan_name(self)
    LOB_x,LOB_y,ROB_x,ROB_y,idx_l,idx_r = cal_bank(Node,return_idx=True)
    bankXY = LOB_x,LOB_y,ROB_x,ROB_y
    plot_bank(self,bankXY,plot_bottom,Node['Mode'],Node,View)

    '''Pointer'''
    self.quer2_label =pg.TextItem(color='k')
    self.graphicsView2.addItem(self.quer2_label)
    self.quer2_label.setPos(Node.X[0],Node.Y[0])
    self.quer2_label.setZValue(10000)
    self.graphicsView.getViewBox().autoRange(items=[self.ax1,self.annotate1])
    
'''Plot banks for the Cross-sections'''
def plot_bank(self,bankXY,plot_bottom,Mod,Node,View,ModeEdit = False):

    riv_bed_y,riv_bed_idx,riv_bed_x = riv_bed(Node)
    LOB_x,LOB_y,ROB_x,ROB_y = bankXY
    top_y = max(LOB_y,ROB_y) +2
    if (riv_bed_x <= LOB_x) or (riv_bed_x >= LOB_y):
        riv_bed_x = (LOB_x+ROB_x)/2.
    
    if not ModeEdit:
        if View == self.graphicsView2:
            self.bank_l2 = View.plot([LOB_x,LOB_x],[LOB_y,top_y],pen=pg.mkPen(width=2,color='k'))
            self.bank_r2 = View.plot([ROB_x,ROB_x],[ROB_y,top_y],pen=pg.mkPen(width=2,color='k')) 
            self.bank_t2 = View.plot([LOB_x,ROB_x],[top_y,top_y])
            
            self.a_l2 = pg.ArrowItem(angle=0,pen=None,brush='k')
            self.a_l2.setPos(LOB_x,top_y)
            View.addItem(self.a_l2)
            self.a_r2 = pg.ArrowItem(angle=-180,pen=None,brush='k')
            self.a_r2.setPos(ROB_x,top_y)
            View.addItem(self.a_r2)
            self.mode_item2 = pg.TextItem(text = Mod,anchor=(0.5,0.5),border='k',fill='k',color='w')
            View.addItem(self.mode_item2)
            self.mode_item2.setPos(riv_bed_x,top_y)
        else:
            self.bank_l1 = View.plot([LOB_x,LOB_x],[LOB_y,top_y],pen=pg.mkPen(width=2,color='k'))
            self.bank_r1 = View.plot([ROB_x,ROB_x],[ROB_y,top_y],pen=pg.mkPen(width=2,color='k')) 
            self.bank_t1 = View.plot([LOB_x,ROB_x],[top_y,top_y])
            
            self.a_l1 = pg.ArrowItem(angle=0,pen=None,brush='k')
            self.a_l1.setPos(LOB_x,top_y)
            View.addItem(self.a_l1)
            self.a_r1 = pg.ArrowItem(angle=-180,pen=None,brush='k')
            self.a_r1.setPos(ROB_x,top_y)
            View.addItem(self.a_r1)
            self.mode_item1 = pg.TextItem(text = Mod,anchor=(0.5,0.5),border='k',fill='k',color='w')
            View.addItem(self.mode_item1)
            self.mode_item1.setPos(riv_bed_x,top_y)
            
    else:
        self.bank_l1.setData([LOB_x,LOB_x],[plot_bottom,top_y])
        self.bank_r1.setData([ROB_x,ROB_x],[plot_bottom,top_y])
        self.bank_t1.setData([LOB_x,ROB_x],[top_y,top_y])
        self.mode_item1.setText(Mod)
        self.mode_item1.setPos(riv_bed_x,top_y)
        self.a_l1.setPos(LOB_x,top_y)
        self.a_r1.setPos(ROB_x,top_y)

'''Generate annotations'''
def textgen(Node,self):
    
    Q = 'Q: {:0.2f} m³/s'.format(self.df_start.loc[Node.name]['QZERO'])
    Text = Q
    if Node.name in self.h1d.nupe:
        _idx = self.h1d.nupe.index(Node.name)
        INF = '\n\nZufluss Mode: {}\nDatei: {}\nFaktor: {:0.2f}'.format(self.h1d.timmod[_idx],
                      self.h1d.quinf_[_idx],self.h1d.faktor[_idx])
        try:
            INF = INF+'\nStation: {:0.1f}\nQbasis: {:0.2f}'.format(self.h1d.q_station[_idx],
                                self.h1d.q_basis[_idx])
        except:
            pass
        Text = Text+INF
    
    if Node.name in self.h1d.lie:
        _idx = self.h1d.lie.index(Node.name)
        li   = self.h1d.zdat[_idx].split()
        LINF = '\n\nLateral Inflow Mode: {}\nDatei: {}\nFaktor: {}'.format(li[1],
                      li[0],li[2])
        try:
            LINF = LINF+'\nStation: {:0.1f}\nQ_basis: {:0.2f}'.format(li[3],li[4])
        except:
            pass
        Text = Text+LINF
    
    if Node.name in self.h1d.igate:
        _idx = self.h1d.igate.index(Node.name)
        gatd = self.h1d.gatdat[_idx]
        Gate = '\n\nGate Max Durchlassöffnung: {}\nMode: {}\nQmin: {}\nQmax: {}\nTol: {}\nµ: {}\nGatmin: {}\nGatwso: {}\nDatei: {}'.format(self.h1d.igate[_idx],*gatd)
        Text = Text + Gate
    
    weirs = [int(we.split()[0]) for we in self.h1d.weir_info]
    if Node.name in weirs:
        _idx = weirs.index(Node.name)
        wehr = '\n\nWehr Höhe,Breite,µ: {},{},{}'.format(*self.h1d.weir_info[_idx].split()[1:4])
        Text = Text+wehr
        
    return Text

'''Plot the longitudnal section here'''
def langPlot(self):
    i=self.gewid_current
    self.langView.clear()
    
    self.langView.addLegend(offset = (-30,30))
        
    idx = (self.df_start['ID'] == i)
    x = self.df_start['XL'].values[idx]
    y = self.df_start['ZO'].values[idx]
    pts = np.array(sorted(list(zip(x,y))))
    self.lsplot = self.langView.plot(pts, pen = 'k',symbolSize=2,
                                     symbolPen = 'b',symbolBrush = 'b',
                                     fillLevel=np.nanmin(y)-0.2, brush=(195,176,145),name='Flussbett')
    self.lsplot.setZValue(1000)
    self.langView.invertX(True)
    self.langView.showGrid(x=True,y=True)
    
    self.wsp_bankL = []
    for r in range(self.p_wspdat.rowCount()):
        c = self.p_wspdat.item(r,3).background().color()
        if self.p_wspdat.item(r,2).checkState() == 2:
            '''wsp plot at längschnitt'''
            wsp_l = self.df_wsp[self.p_wspdat.item(r,1).text()].values[idx]
            sohle = self.df_start['XL'].values[idx]
            leg   = self.p_wspdat.item(r,1).text()
            if self.p_wspdat.item(r,4).checkState() == 2:
                wsp = self.langView.plot(sohle,wsp_l,pen=pg.mkPen(width=1,color='k'),
                                     name = leg,fillLevel=self.df_start['ZO'][idx].min()-0.2,brush =c)
            else:
                wsp = self.langView.plot(sohle,wsp_l,pen=pg.mkPen(width=1,color=c),
                                     name = leg)
            self.wsp_bankL.append(wsp)
    self.langView.getViewBox().setXRange(x.max(),x.min())
    self.langView.getViewBox().setYRange(y.min(),self.df_wsp.max().max())
    gewMarker(self)

def nodePlot(self):
    self.nodeView.clear()
    #achse plot
    try:
        loadshp(self,self.achse)
    except:
        pass

    rshp  = shp.Reader(self.achse)
    s_gid,s_start = [],[]
    for n,i in enumerate(rshp.records()):
        s_gid.append(i['GEW_ID'])
        s_start.append(i['SSTART'])

    nTyp = self.df_start['ITYPE'].values
    nID  = self.df_start['ID'].values
    nXL  = self.df_start['XL'].values
    
    nX,nY = np.zeros(len(nXL)),np.zeros(len(nXL))
    
    for gid in np.unique(nID):
        try:
            idx      = s_gid.index(gid)
            offset   = s_start[idx]
            rec      = rshp.shapeRecords()[idx]
            line     = LineString(rec.shape.points)
            nidx     = np.where(nID == gid)
            seg      = nXL[nidx]-offset
            nX[nidx] = [line.interpolate(seg_i).x for seg_i in seg]
            nY[nidx] = [line.interpolate(seg_i).y for seg_i in seg]
        except:
            self.statusbar.showMessage('ID missing in Start.dat...')

        
    for i in sorted(set(self.df_start['ITYPE'].values)):
        if i not in [1,2,3,4,5,9]:
            idic = self.itype_dict[0]
            na = 'Sonstiges'
        else:
            idic = self.itype_dict[i]
            na = self.gi_ityp.itemText(i-1)
        i1 = np.where(nTyp == i)
        
        nplot = self.nodeView.plot(nX[i1],nY[i1],symbol=idic[0],pen=pg.mkPen(None),
                           symbolSize=idic[1],symbolPen=idic[2],symbolBrush=idic[2],name=na)
        nplot.setZValue(100)
    self.nplot = nplot
    self.nodeView.setAspectLocked(lock=True, ratio=1)


'''use when loading WSP'''
def loadresult(self):
    wspDat = QFileDialog.getOpenFileName(caption='WSP File)',filter="*.dat*")
    if wspDat[0] != '':
        try:
            self.wsp_view_dat.append((wspDat[0][-20:],True,QColor(28,163,236,150),True))
        except:
            self.wsp_view_dat.append((wspDat[0],True,QColor(28,163,236,150),True))
        wspdat = pd.read_csv(wspDat[0],sep=',',header=0)
        if 'WSP' in wspdat.columns:
            df_wsp = pd.read_csv(wspDat[0],sep=',',index_col = 1,header=0)
            wkey = 'WSP'
        elif 'HZERO' in wspdat.columns:
            df_wsp = pd.read_csv(wspDat[0],sep=',',index_col = 0,header=0)
            wkey = 'HZERO'
        
        coln   = 'HQXXX_N'
        try:
            w_iter = 1
            coln_bool = True
            while coln_bool:
                if coln in [k for k in self.wsp_dict]:
                    coln = 'HQXXX_N'+str(w_iter)
                    w_iter +=1
                else:
                    coln_bool = False
            self.wsp_dict.append(coln)
        except:
            self.wsp_dict.append(coln)
        
        self.df_wsp = self.df_wsp.assign(coln = df_wsp[wkey])
        self.df_wsp.rename(columns={'coln':coln},inplace=True)
        wsp_df_update(self)
        langPlot(self)

'''Use wsp_df_update when changing scrolling through Cross-sections'''
def wsp_df_update(self):
    self.p_wspdat.blockSignals(True)
    self.p_wspdat.setRowCount(len(self.wsp_view_dat))
    for x in range(len(self.wsp_view_dat)):
        self.p_wspdat.setItem(x,0,QTableWidgetItem(self.wsp_view_dat[x][0]))
        self.p_wspdat.setItem(x,1,QTableWidgetItem(self.df_wsp.columns[x]))
        item1 = QTableWidgetItem()
        item1.setFlags(Qt.ItemIsUserCheckable | Qt.ItemIsEnabled)
        if self.wsp_view_dat[x][1]:
            item1.setCheckState(Qt.Checked)
            self.p_wspdat.setItem(x,2,item1)
        else:
            item1.setCheckState(Qt.Unchecked)
            self.p_wspdat.setItem(x,2,item1)
        item2 = QTableWidgetItem()
        item2.setFlags(Qt.ItemIsSelectable|Qt.ItemIsEnabled)
        self.p_wspdat.setItem(x,3,item2)
        self.p_wspdat.item(x,3).setBackground(self.wsp_view_dat[x][2])
        item = QTableWidgetItem()
        item.setFlags(Qt.ItemIsUserCheckable | Qt.ItemIsEnabled)
        if self.wsp_view_dat[x][3]:
            item.setCheckState(Qt.Checked)
            self.p_wspdat.setItem(x,4,item)
        else:
            item.setCheckState(Qt.Unchecked)
            self.p_wspdat.setItem(x,4,item)
    plot_wsp(self)
    langPlot(self)
    self.p_wspdat.blockSignals(False)

'''use when changing entries in the WSP Table'''
def update_wsp(self):
    self.wsp_view_dat = []
    new_plan_names    = []
    for r in range(self.p_wspdat.rowCount()):
        c = self.p_wspdat.item(r,3).background().color()
        if self.p_wspdat.item(r,2).checkState() == 2:
            if self.p_wspdat.item(r,4).checkState() == 2:
                self.wsp_view_dat.append((self.p_wspdat.item(r,0).text(),True,c,True))
            else:
                self.wsp_view_dat.append((self.p_wspdat.item(r,0).text(),True,c,False))
        else:
            if self.p_wspdat.item(r,4).checkState() == 2:
                self.wsp_view_dat.append((self.p_wspdat.item(r,0).text(),False,c,True))
            else:
                self.wsp_view_dat.append((self.p_wspdat.item(r,0).text(),False,c,False))
        new_plan_names.append(self.p_wspdat.item(r,1).text())
    
    col_dic = {k:new_plan_names[i] for i,k in enumerate(self.wsp_dict)}
    self.df_wsp.rename(columns = col_dic,inplace=True)
    plot_wsp(self)
    langPlot(self)
    
'''Plot WSP for the Cross-Sections'''
def plot_wsp(self):
    self.wsp_bank1,self.wsp_bank2 = [],[]
    wsp_zval1,wsp_zval2 = [],[]
    for r in range(self.p_wspdat.rowCount()):
        c = self.p_wspdat.item(r,3).background().color()
        if self.p_wspdat.item(r,2).checkState() == 2:
            '''wsp plot at querschnitt'''
            for Node,View in [(self.Node,self.graphicsView),(self.Node2,self.graphicsView2)]:
                riv_bed_y,riv_bed_idx,riv_bed_x     = riv_bed(Node)
                plot_bottom                         = riv_bed_y-2
                LOB_x,LOB_y,ROB_x,ROB_y,idx_l,idx_r = cal_bank(Node,return_idx=True)
        
                '''Starting WSP'''
                if idx_l == riv_bed_idx:
                    wsp_l = Node['X'][idx_l]
                else:
                    wsp_l = Node['X'][idx_l:].min()
                if idx_r == riv_bed_idx:
                    wsp_r = Node['X'][idx_r]
                else:
                    try:
                        wsp_r = Node['X'][:idx_r+1].max()
                    except:
                        wsp_r = Node['X'].max()
                
                wsp = self.df_wsp.loc[Node.name][self.p_wspdat.item(r,1).text()]
                leg = self.p_wspdat.item(r,1).text() + ',WSP= '+str(round(wsp,2))+' m+NN'
                if self.p_wspdat.item(r,4).checkState() == 2:
                    wsp0 = View.plot([wsp_l,wsp_r],[wsp,wsp],pen=pg.mkPen(width=1,color='k'),
                                     fillLevel=plot_bottom,brush =c,name=leg)
                else:
                    wsp0 = View.plot([wsp_l,wsp_r],[wsp,wsp],pen=pg.mkPen(width=1,color=c),
                                     name=leg)

                label = pg.TextItem(text=self.p_wspdat.item(r,1).text(),anchor=(0.5,0.5),color='k',border='k',fill='w')
                View.addItem(label)
                label.setPos(riv_bed_x,wsp)
                
                if Node.name == self.Node.name:
                    self.wsp_bank1.append(wsp0)
                    self.wsp_bank1.append(label)
                    wsp_zval1.append(wsp)
                else:
                    self.wsp_bank2.append(wsp0)
                    self.wsp_bank2.append(label)
                    wsp_zval2.append(wsp)
            indexing1 = list(reversed([sorted(wsp_zval1).index(i) for i in wsp_zval1]))
            indexing2 = list(reversed([sorted(wsp_zval2).index(i) for i in wsp_zval2]))
            for p in range(0,len(self.wsp_bank1),2):
                self.wsp_bank1[p].setZValue(500+indexing1[int(p-p/2)])
                self.wsp_bank1[p+1].setZValue(5000+indexing1[int(p-p/2)]+1)
                self.wsp_bank2[p].setZValue(500+indexing2[int(p-p/2)])
                self.wsp_bank2[p+1].setZValue(5000+indexing2[int(p-p/2)]+1)
                
def colorpicker(self):
    if self.p_wspdat.currentColumn() == 3:
        color = QColorDialog.getColor(Qt.blue, self,'Choose WSP color', QColorDialog.ShowAlphaChannel)
        if color.isValid():
            self.p_wspdat.item(self.p_wspdat.currentRow(),3).setBackground(color)

# =============================================================================
# Update all the plotting data here
# =============================================================================
'''Under Editing Mode, Update the ROI'''
def plotROI(self):
    self.editable_schnitt.setPoints(np.vstack((self.Node.X,self.Node.Y)).T)
    self.editable_schnitt.setZValue(50001)
        
'''Update the Main Cross-section View here'''
def uqplot1(self,ROI=False):
    Node = self.Node
    View = self.graphicsView
    
    if ROI:
        Node = self.df_copy.loc[Node.name]

    riv_bed_y,riv_bed_idx,riv_bed_x = riv_bed(Node)
    plot_bottom                     = riv_bed_y-2
    
    self.ax1.setData(Node['X'],Node['Y'],fillLevel=plot_bottom,name="Querschnitt")
    
    #plot lamellen
    if -1*Node.name in self.df_pro.index:
        try:
            self.rau_ax1.clear()
        except:pass
        View.showAxis('right')
        View.scene().addItem(self.p2)
        self.p2.setGeometry(View.getViewBox().sceneBoundingRect())
        self.p2.linkedViewChanged(View.getViewBox(), self.p2.XAxis)
        View.getAxis('right').linkToView(self.p2)
        self.p2.setXLink(View)
        View.getAxis('right').setLabel('kst', color='#0000ff')
        self.p2.setYRange(-100,100)
        self.rau_ax1 = View.plot(self.Node_R['X'],self.Node_R['Y'],pen='k',
                                   symbolSize=3,symbolPen=0.5,symbolBrush=0.5,
                                   fillLevel=self.Node_R['Y'].min(),brush=0.5,
                                   name='k-strickler')
        self.rau_ax1.setZValue(10001)
        self.p2.addItem(self.rau_ax1)
        strickler=True
    else:
        strickler=False
        try:
            self.rau_ax1.clear()
        except:pass
        View.showAxis('right',show=False)
    
    if ROI:
        self.editable_schnitt.setPoints(np.vstack((Node.X,Node.Y)).T)
        
    '''update text annotations Info'''
    Text = textgen(Node,self)
    self.annotate1.setText(Text)
    self.annotate1.setPos(Node.X.max(),Node.Y.max()+4)
    
    '''update river Bed annotations'''
    self.riverbed1.setData([riv_bed_x,riv_bed_x],[riv_bed_y,plot_bottom])
    self.sohle1.setPos(riv_bed_x,plot_bottom)
    plan_name(self)

    #update banks here
    LOB_x,LOB_y,ROB_x,ROB_y,idx_l,idx_r = cal_bank(Node,return_idx=True)

    top_y = max(LOB_y,ROB_y) +2
    if (riv_bed_x <= LOB_x) or (riv_bed_x >= ROB_x):
        riv_bed_x = (LOB_x+ROB_x)/2.
        
    Mod = Node['Mode']
    self.bank_l1.setData([LOB_x,LOB_x],[plot_bottom,top_y])
    self.bank_r1.setData([ROB_x,ROB_x],[plot_bottom,top_y])
    self.bank_t1.setData([LOB_x,ROB_x],[top_y,top_y])
    self.mode_item1.setText(Mod)
    self.mode_item1.setPos(riv_bed_x,top_y)
    self.a_l1.setPos(LOB_x,top_y)
    self.a_r1.setPos(ROB_x,top_y)
            
    #update wsp here
    for r in range(0,len(self.wsp_bank1),2):
        '''Starting WSP'''
        if idx_l == riv_bed_idx:
            wsp_l = Node['X'][idx_l]
        else:
            wsp_l = Node['X'][idx_l:].min()
        if idx_r == riv_bed_idx:
            wsp_r = Node['X'][idx_r]
        else:
            try:
                wsp_r = Node['X'][:idx_r+1].max()
            except:
                wsp_r = Node['X'].max()
            
        wsp = self.df_wsp.loc[Node.name][self.p_wspdat.item(r,1).text()]
        leg = self.p_wspdat.item(r,1).text() + ',WSP= '+str(round(wsp,2))+' m+NN'
        self.wsp_bank1[r].setData([wsp_l,wsp_r],[wsp,wsp],fillLevel=plot_bottom,name=leg)
        self.wsp_bank1[r+1].setPos(riv_bed_x,wsp)

    View.getViewBox().autoRange(items=[self.ax1,self.annotate1])
    
'''View updater for Lamellen View'''
def updateViews(self):
    try:
        self.p2.setGeometry(self.graphicsView.getViewBox().sceneBoundingRect())
        self.p2.linkedViewChanged(self.graphicsView.getViewBox(),self.p2.XAxis)
    except: pass

'''Update the Secondary Cross-section View here'''
def uqplot2(self):
    
    Node = self.Node2
    View = self.graphicsView2

    riv_bed_y,riv_bed_idx,riv_bed_x = riv_bed(Node)
    plot_bottom                     = riv_bed_y-2
    
    
    self.q2_leg.items=[]
    try:
        while self.q2_leg.layout.count() >0:
            self.q2_leg.removeAt(0)
    except:pass
        
    self.ax2.setData(Node['X'],Node['Y'],fillLevel=plot_bottom)
    self.q2_leg.addItem(self.ax2,"Querschnitt")
    
    '''update text annotations Info'''
    Text = textgen(Node,self)
    self.annotate2.setText(Text)
    self.annotate2.setPos(Node.X.max(),Node.Y.max()+4)
    
    '''update river Bed annotations'''
    self.riverbed2.setData([riv_bed_x,riv_bed_x],[riv_bed_y,plot_bottom])
    self.sohle2.setPos(riv_bed_x,plot_bottom)
    plan_name(self)

    #update banks here
    LOB_x,LOB_y,ROB_x,ROB_y,idx_l,idx_r = cal_bank(Node,return_idx=True)
    top_y = max(LOB_y,ROB_y) +2
    if (riv_bed_x <= LOB_x) or (riv_bed_x >= ROB_x):
        riv_bed_x = (LOB_x+ROB_x)/2.
    Mod = Node['Mode']
    self.bank_l2.setData([LOB_x,LOB_x],[plot_bottom,top_y])
    self.bank_r2.setData([ROB_x,ROB_x],[plot_bottom,top_y])
    self.bank_t2.setData([LOB_x,ROB_x],[top_y,top_y])
    self.mode_item2.setText(Mod)
    self.mode_item2.setPos(riv_bed_x,top_y)
    self.a_l2.setPos(LOB_x,top_y)
    self.a_r2.setPos(ROB_x,top_y)

    #update wsp here
    for r in range(0,len(self.wsp_bank2),2):
        '''Starting WSP'''
        if idx_l == riv_bed_idx:
            wsp_l = Node['X'][idx_l]
        else:
            wsp_l = Node['X'][idx_l:].min()
        if idx_r == riv_bed_idx:
            wsp_r = Node['X'][idx_r]
        else:
            try:
                wsp_r = Node['X'][:idx_r+1].max()
            except:
                wsp_r = Node['X'].max()
            
        wsp = self.df_wsp.loc[Node.name][self.p_wspdat.item(r,1).text()]
        leg = self.p_wspdat.item(r,1).text() + ',WSP= '+str(round(wsp,2))+' m+NN'
        self.q2_leg.addItem(wsp,leg)
        self.wsp_bank2[r].setData([wsp_l,wsp_r],[wsp,wsp],fillLevel=plot_bottom)
        self.wsp_bank2[r+1].setPos(riv_bed_x,wsp)

    View.getViewBox().autoRange(items=[self.ax2,self.annotate2])

def ulangPlot(self):
    i=int(self.lang_ID.currentText())
    self.langView.clear()
    idx = (self.df_start['ID'] == i)
    x = self.df_start['XL'].values[idx]
    y = self.df_start['ZO'].values[idx]
    pts = np.array(sorted(list(zip(x,y))))
    self.lsplot.setData(pts)
    
    for r in range(0,len(self.wsp_bankL)):
        wsp_l = self.df_wsp[self.p_wspdat.item(r,1).text()].values[idx]
        sohle = self.df_start['XL'].values[idx]
        leg   = self.p_wspdat.item(r,1).text()
        self.wsp_bankL[r].setData(sohle,wsp_l,name = leg,fillLevel=self.df_start['ZO'][idx].min()-0.2)
    self.langView.getViewBox().setXRange(x.max(),x.min())
    self.langView.getViewBox().setYRange(y.min(),self.df_wsp.max().max())
    gewMarker(self)
    
# =============================================================================
#Highlighter Functions to show whats being viewed presently
# =============================================================================

'''Mark the selected Coordinates from Coordinates table into the Main Cross-section View'''
def xyMarker(self):
    
    self.row_c = self.coords_table.selectedIndexes()
    self.row_c_idx = [i.row() for i in self.row_c]
    
    if self.Edit:
        color = 'y'
    else:
        color = 'b'
        
    if self._rquer.isChecked():
        Node = self.Node
        xs,ys = Node['X'][self.row_c_idx],Node['Y'][self.row_c_idx]
        try:
            self.selection.clear()
            self.selection.setData(xs,ys)
        except:
            self.selection = self.graphicsView.plot(xs,ys,pen=None,symbol='o',
                                                    symbolSize=10,symbolPen=color,symbolBrush=color)
            self.selection.setZValue(2000)

    elif self._rrau.isChecked():
        Node = self.Node_R
        xs,ys = Node['X'][self.row_c_idx],Node['Y'][self.row_c_idx]
        try:
            self.selectionR.clear()
            self.selectionR.setData(xs,ys)
        except:
            self.selectionR = self.graphicsView.plot(xs,ys,pen=None,symbol='o',
                                                    symbolSize=10,symbolPen=color,symbolBrush=color)
            self.selectionR.setZValue(2000)
            self.p2.addItem(self.selectionR)
        
    elif self._rschalter.isChecked():
        Node = self.Node_S
        xs,ys = Node['X'][self.row_c_idx],Node['Y'][self.row_c_idx]
        try:
            self.selectionS.clear()
            self.selectionS.setData(xs,ys)
        except:
            self.selectionS = self.graphicsView.plot(xs,ys,pen=None,symbol='o',
                                                    symbolSize=10,symbolPen=color,symbolBrush=color)
            self.selectionS.setZValue(2000)
            self.p3.addItem(self.selectionS)

def xyUnmark(self,event):
    try: self.selection.clear()
    except: pass
    try: self.selectionR.clear()
    except: pass
    try: self.selectionS.clear()
    except: pass

def nodeMarker(self):
    try:
        self.pro_mark.clear()
        self.nodeView.removeItem(self.nodeitem)
        self.nodeView.removeItem(self.nodeitem2)
    except:
        pass
    xk1     = float(self.df_start.loc[self.loc]['X'])
    yk1     = float(self.df_start.loc[self.loc]['Y'])
    xk2     = float(self.df_start.loc[self.loc2]['X'])
    yk2     = float(self.df_start.loc[self.loc2]['Y'])
    try:
        self.pro_mark.setData([xk1,xk2],[yk1,yk2])
        self.nodeitem.setText(str(self.loc))
        self.nodeitem.setPos(xk1,yk1)
        self.nodeitem2.setText(str(self.loc2))
        self.nodeitem2.setPos(xk2,yk2)
    except:
        self.pro_mark = self.nodeView.plot([xk1,xk2],[yk1,yk2],pen=None,symbol='o',symbolSize='10',
                                           symbolPen='b',symbolBrush='b')
        self.nodeitem = pg.TextItem(text=str(self.loc),angle=0,border='k',fill='w',color='b')
        self.nodeView.addItem(self.nodeitem)
        self.nodeitem.setPos(xk1,yk1)
        self.nodeitem2 = pg.TextItem(text=str(self.loc2),angle=0,border='k',fill='w',color='b')
        self.nodeView.addItem(self.nodeitem2)
        self.nodeitem2.setPos(xk2,yk2)

#Mark Gewässer achse in Node View
def gewMarker(self):
    i= self.gewid_current
    try:
        self.highlight.clear()
    except:pass

    rshp  = shp.Reader(self.achse)
    for n,s in enumerate(rshp.records()):
        if s['GEW_ID'] == i:
            rec      = rshp.shapeRecords()[n]
            pts        = np.array(rec.shape.points)
            self.highlight = self.nodeView.plot(pts[:,0],pts[:,1],pen=pg.mkPen('y', width=3))
            self.highlight.setZValue(0)
            break

def gewUnmark(self,event):
    try: self.highlight.clear()
    except: pass

def changeAR(self):
    self.ratio = 1.0/self.AspectRatio.value()
    self.axViewBox1.setAspectLocked(lock=True, ratio=self.ratio)
    self.axViewBox2.setAspectLocked(lock=True, ratio=self.ratio)

def plan_name(self):
    if self.p_plan.text() != '':
        self.graphicsView.setTitle('Knoten Nr.: '+str(self.Node.name)+', Plan: ' +self.p_plan.text())
        self.graphicsView2.setTitle('Knoten Nr.: '+str(self.Node2.name)+', Plan: ' +self.p_plan.text())
    else:
        self.graphicsView.setTitle('Knoten Nr.: '+str(self.Node.name))
        self.graphicsView2.setTitle('Knoten Nr.: '+str(self.Node2.name))
    self.df_wsp.rename(columns={self.mod_plan:self.p_plan.text()},inplace=True)

# =============================================================================
# Update plots under Editing Mode
# =============================================================================
def update_data(self,src = 'db'):
    if src=='db':
        self.coords_table.setRowCount(self.Node.Npoints)
        for i in range(self.Node.Npoints):
            self.coords.setItem(i,0,QTableWidgetItem(str(self.Node.X[i])))
            self.coords.setItem(i,1,QTableWidgetItem(str(self.Node.Y[i])))
    elif src == 'table':
        x = np.array([float(self.coords_table.item(i,0).text()) for i in range(self.coords_table.rowCount())])
        y = np.array([float(self.coords_table.item(i,1).text()) for i in range(self.coords_table.rowCount())])
        df = self.df_copy.copy()
        if self._rquer.isChecked():
            df.iat[self.iloc,0] = len(x)
            df.iat[self.iloc,6] = x
            df.iat[self.iloc,7] = y
        elif self._rrau.isChecked():
            df.iat[self.iloc_r,0] = len(x)
            df.iat[self.iloc_r,6] = x
            df.iat[self.iloc_r,7] = y
        elif self._rschalter.isChecked():
            df.iat[self.iloc_s,0] = len(x)
            df.iat[self.iloc_s,6] = x
            df.iat[self.iloc_s,7] = y
        self.df_db.append(df)
        return x,y
        
       
def undo_but(self):
    self.editable_schnitt.blockSignals(True)
    self.changes -=1
    self.df_copy.update(self.df_db[self.changes])
    
    uqplot1(self,ROI=True)
    
    if self.changes == 0:
        self.undo.setEnabled(False)
    self.redo.setEnabled(True)
    self.editable_schnitt.blockSignals(False)

def redo_but(self):
    self.editable_schnitt.blockSignals(True)
    self.changes +=1
    self.df_copy.update(self.df_db[self.changes])
    
    uqplot1(self,ROI=True)
    
    if self.changes == len(self.df_db)-1:
        self.redo.setEnabled(False)
    if self.changes > 0:
        self.undo.setEnabled(True)
    self.editable_schnitt.blockSignals(False)
            
def update_schnitt(self):
    self.coords_table.blockSignals(True)
    _pts = self.editable_schnitt.getState()['points']
    self.Node['Npoints']=len(_pts)
    self.Punkte_label.display(len(_pts))
    self.coords_table.setRowCount(len(_pts))
    self.Node.X = np.array([round(xi.x(),3) for xi in self.editable_schnitt.getState()['points']])
    self.Node.Y = np.array([round(yi.y(),3) for yi in self.editable_schnitt.getState()['points']])
    for _i in range(len(_pts)):
        self.coords_table.setItem(_i,0,QTableWidgetItem(str(self.Node.X[_i])))
        self.coords_table.setItem(_i,1,QTableWidgetItem(str(self.Node.Y[_i])))
    uqplot1(self)
    self.coords_table.blockSignals(False)
    
def plot_update_coords(self):
    self.coords_table.blockSignals(True)
    if not len(self.df_db) == self.changes + 1:
        self.df_db = self.df_db[:self.changes+1]
        self.redo.setEnabled(False)
    self.changes +=1
    update_data(self,src='table')
    uqplot1(self)
    self.undo.setEnabled(True)
    self.coords_table.blockSignals(False)