# -*- coding: utf-8 -*-
"""
Created on Wed Jun 26 11:49:02 2019

@author: s.Shaji
"""

import numpy          as     np
import pandas         as     pd
import pyqtgraph      as     pg
import shapefile      as     shp
from shapely.geometry import LineString
from modules.loaddata import loadshp
from modules.riverbed import riv_bed,cal_bank
from PyQt5.QtWidgets  import QTableWidgetItem,QFileDialog,QColorDialog,QApplication
from PyQt5.QtCore     import Qt,QEvent
from PyQt5.QtGui      import QColor

# =============================================================================
# Initiate plotting, Plot all 4 Graphic windows and then update the data using
# update functions
# =============================================================================

'''Plot the Cross-section Views here'''
def qplots(self,plist=[0,1]):
    for n in plist:
        Node = self.qplotD['node'][n]
        View = self.qplotD['view'][n]
        View.clear()
        
        riv_bed_y,riv_bed_idx,riv_bed_x = riv_bed(Node)
        self.qplotD['plotbot'][n]       = riv_bed_y-2
        
        View.addLegend()
        
        self.qplotD['axis'][n] = View.plot(Node['X'],Node['Y'],pen='k',symbol='d',
                                           symbolSize=7,symbolPen='r',symbolBrush='r',
                                           fillLevel=self.qplotD['plotbot'][n],brush=(195,176,145),
                                           name="Querschnitt")
        
        self.qplotD['axis'][n].setZValue(1000)
        self.qplotD['axis'][n].getViewBox().setAspectLocked(lock=True, ratio=self.ratio)
        View.getAxis('left').setLabel('Höhe')
    
        #plot lamellen
        if not self.qplotD['rnode'][n] is None:
            View.showAxis('right')
            View.scene().addItem(self.qplotD['rvbox'][n])
            self.qplotD['rvbox'][n].setGeometry(View.getViewBox().sceneBoundingRect())
            self.qplotD['rvbox'][n].linkedViewChanged(View.getViewBox(), self.qplotD['rvbox'][n].XAxis)
            View.getAxis('right').linkToView(self.qplotD['rvbox'][n])
            self.qplotD['rvbox'][n].setXLink(View)
            View.getAxis('right').setLabel('kst', color='#0000ff')
            self.qplotD['rvbox'][n].setYRange(-100,100)
            self.qplotD['raxis'][n] = View.plot(self.qplotD['rnode'][n].X,self.qplotD['rnode'][n].Y,pen='k',
                                               symbolSize=3,symbolPen=0.5,symbolBrush=0.5,
                                               fillLevel=self.qplotD['rnode'][n].Y.min(),brush=0.5,name='k-strickler')
            self.qplotD['raxis'][n].setZValue(10001)
            self.qplotD['rvbox'][n].addItem(self.qplotD['raxis'][n])
        else:
            try:
                self.qplotD['raxis'][n].scene().removeItem(self.qplotD['raxis'][n])
                self.qplotD['raxis'][n] = None
            except:
                pass
            View.showAxis('right',show=False)
        
        '''Annotate Info'''
        Text = textgen(Node,self)
        self.qplotD['annotate'][n] = pg.TextItem(Text,anchor=(1,0.5),color='k',border='k',fill='w')
        View.addItem(self.qplotD['annotate'][n])
        self.qplotD['annotate'][n].setPos(Node.X.max(),Node.Y.max()+4)
        
        '''River Bed'''
        self.qplotD['riverbed'][n] = View.plot([riv_bed_x,riv_bed_x],[riv_bed_y,self.qplotD['plotbot'][n]],
                                               pen=pg.mkPen(width=1,color='b'))
        self.qplotD['riverbed'][n].setZValue(3000)
        self.qplotD['sohle'][n]    = pg.TextItem(text='Tiefpunkt',anchor=(0.5,0.5),color='b',border='k',fill='w')
        View.addItem(self.qplotD['sohle'][n])
        self.qplotD['sohle'][n].setPos(riv_bed_x,self.qplotD['plotbot'][n])
        self.qplotD['sohle'][n].setZValue(4000)
       
        '''Title'''
        plan_name(self)
        
        '''Plot Banks'''
        LOB_x,LOB_y,ROB_x,ROB_y,idx_l,idx_r = cal_bank(Node,return_idx=True)
    
        top_y = max(LOB_y,ROB_y) +2
        if (riv_bed_x <= LOB_x) or (riv_bed_x >= ROB_x):
            riv_bed_x = (LOB_x+ROB_x)/2.
            
        bank_l = View.plot([LOB_x,LOB_x],[LOB_y,top_y],pen=pg.mkPen(width=2,color='k'))
        bank_r = View.plot([ROB_x,ROB_x],[ROB_y,top_y],pen=pg.mkPen(width=2,color='k')) 
        bank_t = View.plot([LOB_x,ROB_x],[top_y,top_y])
        
        a_l = pg.ArrowItem(angle=0,pen=None,brush='k')
        a_l.setPos(LOB_x,top_y)
        View.addItem(a_l)
        a_r = pg.ArrowItem(angle=-180,pen=None,brush='k')
        a_r.setPos(ROB_x,top_y)
        View.addItem(a_r)
        mode_item = pg.TextItem(text = Node.Mode,anchor=(0.5,0.5),border='k',fill='k',color='w')
        View.addItem(mode_item)
        mode_item.setPos(riv_bed_x,top_y)
        self.qplotD['rbank'][n] = [bank_l,bank_r,bank_t,a_l,a_r,mode_item]
                
        '''Pointer'''
        self.qplotD['pointer'][n] =pg.TextItem(color='k')
        View.addItem(self.qplotD['pointer'][n])
        self.qplotD['pointer'][n].setPos(Node.X[0],Node.Y[0])
        self.qplotD['pointer'][n].setZValue(10000)

'''Generate annotations'''
def textgen(Node,self):
    
    Q = 'Q: {:0.2f} m³/s'.format(self.df_start.loc[Node.name]['QZERO'])
    Text = Q
    try:
        if Node.name in self.h1d.nupe:
            _idx = self.h1d.nupe.index(Node.name)
            INF = '\n\nZufluss Mode: {}\nDatei: {}\nFaktor: {:0.2f}'.format(self.h1d.timmod[_idx],
                          self.h1d.quinf_[_idx],self.h1d.faktor[_idx])
            try:
                INF = INF+'\nStation: {:0.1f}\nQbasis: {:0.2f}'.format(self.h1d.q_station[_idx],
                                    self.h1d.q_basis[_idx])
            except: pass
            Text = Text+INF
    except: pass

    try:
        if Node.name in self.h1d.lie:
            _idx = self.h1d.lie.index(Node.name)
            li   = self.h1d.zdat[_idx].split()
            LINF = '\n\nLateral Inflow Mode: {}\nDatei: {}\nFaktor: {}'.format(li[1],
                          li[0],li[2])
            try:
                LINF = LINF+'\nStation: {:0.1f}\nQ_basis: {:0.2f}'.format(li[3],li[4])
            except: pass
            Text = Text+LINF
    except: pass
    
    try:
        if Node.name in self.h1d.igate:
            _idx = self.h1d.igate.index(Node.name)
            gatd = self.h1d.gatdat[_idx]
            Gate = '\n\nGate Max Durchlassöffnung: {}\nMode: {}\nQmin: {}\nQmax: {}\nTol: {}\nµ: {}\nGatmin: {}\nGatwso: {}\nDatei: {}'.format(self.h1d.igate[_idx],*gatd)
            Text = Text + Gate
    except: pass

    try:
        weirs = [int(we.split()[0]) for we in self.h1d.weir_info]
        if Node.name in weirs:
            _idx = weirs.index(Node.name)
            wehr = '\n\nWehr Höhe,Breite,µ: {},{},{}'.format(*self.h1d.weir_info[_idx].split()[1:4])
            Text = Text+wehr
    except:pass
        
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

'''Plot the Aerial View here'''
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
    self.nodeView.addLegend()
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

    xk1     = float(self.df_s.loc[self.loc]['X'])
    yk1     = float(self.df_s.loc[self.loc]['Y'])
    xk2     = float(self.df_s.loc[self.loc2]['X'])
    yk2     = float(self.df_s.loc[self.loc2]['Y'])

    self.pro_mark = self.nodeView.plot([xk1,xk2],[yk1,yk2],pen=None,symbol='o',symbolSize='10',
                                       symbolPen='b',symbolBrush='b')
    self.nodeitem = pg.TextItem(text=str(self.loc),angle=0,border='k',fill='w',color='b')
    self.nodeView.addItem(self.nodeitem)
    self.nodeitem.setPos(xk1,yk1)
    self.nodeitem2 = pg.TextItem(text=str(self.loc2),angle=0,border='k',fill='w',color='b')
    self.nodeView.addItem(self.nodeitem2)
    self.nodeitem2.setPos(xk2,yk2)
        
    self.nodeView.setAspectLocked(lock=True, ratio=1)
    self.nodeView.getViewBox().setXRange(nX.min(),nX.max())
    self.nodeView.getViewBox().setYRange(nY.min(),nY.max())
    
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
    for n,View in enumerate([self.graphicsView,self.graphicsView2]):
        try:
            [View.plotItem.legend.removeItem(self.qplotD['wbank'][n][i]) for i in range(0,len(self.qplotD['wbank'][n]),2)]
            [View.removeItem(i) for i in self.qplotD['wbank'][n]]
        except:
            pass
        
    wsp_bank1,wsp_bank2 = [],[]
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
                    wsp_bank1.append(wsp0)
                    wsp_bank1.append(label)
                    wsp_zval1.append(wsp)
                else:
                    wsp_bank2.append(wsp0)
                    wsp_bank2.append(label)
                    wsp_zval2.append(wsp)
                    
    indexing1 = list(reversed([sorted(wsp_zval1).index(i) for i in wsp_zval1]))
    indexing2 = list(reversed([sorted(wsp_zval2).index(i) for i in wsp_zval2]))
    for p in range(0,len(wsp_bank1),2):
        wsp_bank1[p].setZValue(500+indexing1[int(p-p/2)])
        wsp_bank1[p+1].setZValue(5000+indexing1[int(p-p/2)]+1)
        wsp_bank2[p].setZValue(500+indexing2[int(p-p/2)])
        wsp_bank2[p+1].setZValue(5000+indexing2[int(p-p/2)]+1)
    self.qplotD['wbank'] = [wsp_bank1,wsp_bank2]
                
                
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
def uqplots(self,plist=[0,1],ROI=False):
    for n in plist:
        Node  = self.qplotD['node'][n]
        rNode = self.qplotD['rnode'][n]
#        sNode = self.qplotD['snode'][n]
        View  = self.qplotD['view'][n]
        
#        if ROI:
#            Node = self.df_copy.loc[Node.name]
    
        riv_bed_y,riv_bed_idx,riv_bed_x = riv_bed(Node)
        self.qplotD['plotbot'][n]       = riv_bed_y-2
        
        self.qplotD['axis'][n].setData(Node['X'],Node['Y'],fillLevel=self.qplotD['plotbot'][n])
        
        #plot lamellen
        if not rNode is None:
            if not self.qplotD['raxis'][n] is None:
                self.qplotD['raxis'][n].setData(rNode.X,rNode.Y)
            else:
                View.showAxis('right')
                View.scene().addItem(self.qplotD['rvbox'][n])
                self.qplotD['rvbox'][n].setGeometry(View.getViewBox().sceneBoundingRect())
                self.qplotD['rvbox'][n].linkedViewChanged(View.getViewBox(), self.qplotD['rvbox'][n].XAxis)
                View.getAxis('right').linkToView(self.qplotD['rvbox'][n])
                self.qplotD['rvbox'][n].setXLink(View)
                View.getAxis('right').setLabel('kst', color='#0000ff')
                self.qplotD['rvbox'][n].setYRange(-100,100)
                self.qplotD['raxis'][n] = View.plot(rNode.X,rNode.Y,pen='k',symbolSize=3,
                                                     symbolPen=0.5,symbolBrush=0.5,
                                                     fillLevel=rNode.Y.min(),brush=0.5,
                                                     name='k-strickler')
                self.qplotD['raxis'][n].setZValue(10001)
                self.qplotD['rvbox'][n].addItem(self.qplotD['raxis'][n])
        else:
            if not self.qplotD['raxis'][n] is None:
                self.qplotD['raxis'][n].clear()
                View.plotItem.legend.removeItem(self.qplotD['raxis'][n])
                self.qplotD['raxis'][n] = None
            View.showAxis('right',show=False)
        
        if ROI:
            self.editable_schnitt.setPoints(np.vstack((Node.X,Node.Y)).T)
            
        '''update text annotations Info'''
        Text = textgen(Node,self)
        self.qplotD['annotate'][n].setText(Text)
        self.qplotD['annotate'][n].setPos(Node.X.max(),Node.Y.max()+4)
        
        '''update river Bed annotations'''
        self.qplotD['riverbed'][n].setData([riv_bed_x,riv_bed_x],[riv_bed_y,self.qplotD['plotbot'][n]])
        self.qplotD['sohle'][n].setPos(riv_bed_x,self.qplotD['plotbot'][n])
        plan_name(self)
    
        #update banks here
        LOB_x,LOB_y,ROB_x,ROB_y,idx_l,idx_r = cal_bank(Node,return_idx=True)
        update_banks(self,n)
                
        #update wsp here
        for r in range(0,len(self.qplotD['wbank'][n]),2):
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
            self.qplotD['wbank'][n][r].setData([wsp_l,wsp_r],[wsp,wsp],fillLevel=self.qplotD['plotbot'][n])
            View.plotItem.legend.removeItem(self.qplotD['wbank'][n][r])
            View.plotItem.legend.addItem(self.qplotD['wbank'][n][r],leg)
            self.qplotD['wbank'][n][r+1].setPos(riv_bed_x,wsp)
    
        View.getViewBox().autoRange(items=[self.qplotD['axis'][n],self.qplotD['annotate'][n]])

'''Update banks for the Cross-sections'''
def update_banks(self,n):
    
    Node = self.qplotD['node'][n]
    riv_bed_y,riv_bed_idx,riv_bed_x = riv_bed(Node)
    LOB_x,LOB_y,ROB_x,ROB_y = cal_bank(Node)
    top_y = max(LOB_y,ROB_y) +2
    if (riv_bed_x <= LOB_x) or (riv_bed_x >= ROB_x):
        riv_bed_x = (LOB_x+ROB_x)/2.
        
    self.qplotD['rbank'][n][0].setData([LOB_x,LOB_x],[self.qplotD['plotbot'][n],top_y])
    self.qplotD['rbank'][n][1].setData([ROB_x,ROB_x],[self.qplotD['plotbot'][n],top_y])
    self.qplotD['rbank'][n][2].setData([LOB_x,ROB_x],[top_y,top_y])
    self.qplotD['rbank'][n][3].setPos(LOB_x,top_y)
    self.qplotD['rbank'][n][4].setPos(ROB_x,top_y)
    self.qplotD['rbank'][n][5].setText(self.Node.Mode)
    self.qplotD['rbank'][n][5].setPos(riv_bed_x,top_y)
    
'''View updater for Lamellen View'''
def updateViews(self):
    try:
        for n,v in [(0,self.graphicsView),(1,self.graphicsView2)]:
            self.qplotD['rvbox'][n].setGeometry(v.getViewBox().sceneBoundingRect())
            self.qplotD['rvbox'][n].linkedViewChanged(v.getViewBox(),self.qplotD['rvbox'][n].XAxis)
    except: pass

def ulangPlot(self):
    i=int(self.lang_ID.currentText())
    self.gewid_current= i
    idx = (self.df_start['ID'] == i)
    x = self.df_start['XL'].values[idx]
    y = self.df_start['ZO'].values[idx]
    pts = np.array(sorted(list(zip(x,y))))
    self.lsplot.setData(pts,fillLevel=np.nanmin(y)-0.2)
    
    for r in range(0,len(self.wsp_bankL)):
        wsp_l = self.df_wsp[self.p_wspdat.item(r,1).text()].values[idx]
        sohle = self.df_start['XL'].values[idx]
        leg   = self.p_wspdat.item(r,1).text()
        self.wsp_bankL[r].setData(sohle,wsp_l,fillLevel=self.df_start['ZO'][idx].min()-0.2)
        self.langView.plotItem.legend.removeItem(self.wsp_bankL[r])
        self.langView.plotItem.legend.addItem(self.wsp_bankL[r],leg)
    self.langView.getViewBox().setXRange(x.max(),x.min())
    self.langView.getViewBox().setYRange(y.min(),self.df_wsp.max().max())
    self.langView.scene().sigMouseClicked.emit(QEvent.MouseButtonPress)
    gewMarker(self)
    
# =============================================================================
#Highlighter Functions to show whats being viewed presently
# =============================================================================

'''Mark the selected Coordinates from Coordinates table into the Main Cross-section View'''
def xyMarker(self):

    self.row_c_idx = sorted(set([i.row() for i in self.coords_table.selectedIndexes()]))

    if self.Edit:
        self.delete_rows.setEnabled(True)
        color = 'y'
    else:
        color = 'b'

    Node = self.qplotD['node'][0]
    xs,ys = Node['X'][self.row_c_idx],Node['Y'][self.row_c_idx]
    
    if self._rquer.isChecked():
        if hasattr(self,'selection'):
            self.selection.setData(xs,ys,symbolPen=color,symbolBrush=color)
        else:
            self.selection = self.graphicsView.plot(xs,ys,pen=None,symbol='o',
                                                    symbolSize=10,symbolPen=color,symbolBrush=color)
            self.selection.setZValue(2000)

def xyUnmark(self,event):
    try: self.selection.clear()
    except: pass

def nodeMarker(self,df):
    if hasattr(self,'pro_mark'):
        xk1     = float(df.loc[self.loc]['X'])
        yk1     = float(df.loc[self.loc]['Y'])
        xk2     = float(df.loc[self.loc2]['X'])
        yk2     = float(df.loc[self.loc2]['Y'])
    
        self.pro_mark.setData([xk1,xk2],[yk1,yk2])
        self.nodeitem.setText(str(self.loc))
        self.nodeitem.setPos(xk1,yk1)
        self.nodeitem2.setText(str(self.loc2))
        self.nodeitem2.setPos(xk2,yk2)

#Mark Gewässer achse in Node View
def gewMarker(self):
    i= int(self.lang_ID.currentText())
    try:
        self.nodeView.plotItem.legend.removeItem(self.highlight)
        self.highlight.clear()
    except: pass
    rshp  = shp.Reader(self.achse)
    for n,s in enumerate(rshp.records()):
        if s['GEW_ID'] == i:
            rec      = rshp.shapeRecords()[n]
            pts        = np.array(rec.shape.points)
            self.highlight = self.nodeView.plot(pts[:,0],pts[:,1],pen=pg.mkPen('y', width=3),name='GEW ID: '+str(i))
            self.highlight.setZValue(0)
            break

def gewUnmark(self,event):
    if self.highlight.opts['pen'] is None:
        self.highlight.setData(pen=pg.mkPen('y', width=3))
        self.nodeView.plotItem.legend.addItem(self.highlight,self.highlight.name())
    else:
        self.highlight.setData(pen=None)
        self.nodeView.plotItem.legend.removeItem(self.highlight)

def changeAR(self):
    self.ratio = 1.0/self.AspectRatio.value()
    self.ax1.getViewBox().setAspectLocked(lock=True, ratio=self.ratio)
    self.ax2.getViewBox().setAspectLocked(lock=True, ratio=self.ratio)

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
       
def undo_but(self):
    self.editable_schnitt.blockSignals(True)
    self.changes -=1
    self.df_copy.update(self.df_db[self.changes])
    
    update_nodes(self)
    uqplots(self,plist = [0],ROI=True)
    
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
    
    if self.changes == len(self.df_db)-1:
        self.redo.setEnabled(False)
    if self.changes > 0:
        self.undo.setEnabled(True)
    self.editable_schnitt.blockSignals(False)

def del_but(self):
    self.editable_schnitt.blockSignals(True)
    self.coords_table.blockSignals(True)
    self.changes +=1
    
    [self.coords_table.removeRow(i) for i in reversed(sorted(set([r.row() for r in self.coords_table.selectedItems()])))]
    update_data(self,src='table')

    self.df_copy.update(self.df_db[self.changes])
    update_nodes(self)
    uqplots(self,plist = [0],ROI=True)
    
    self.selection.clear()
    if self.changes == len(self.df_db)-1:
        self.redo.setEnabled(False)
    if self.changes > 0:
        self.undo.setEnabled(True)
    self.delete_rows.setEnabled(False)
    self.coords_table.blockSignals(False)
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
    uqplots(self,plist = [0])
    self.undo.setEnabled(True)
    self.coords_table.blockSignals(False)
    
def update_nodes(self):
    #update nodes object
    self.qplotD['node'][0] = self.df_copy.loc[self.loc]
    if not self.qplotD['rnode'][0] == None:
        self.qplotD['rnode'][0] = self.df_copy.iloc[self.qplotD['riloc'][0]]
    if not self.qplotD['snode'][0] == None:
        self.qplotD['snode'][0] = self.df_copy.iloc[self.qplotD['siloc'][0]]
        
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
    uqplots(self,plist = [0])
    self.coords_table.blockSignals(False)

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
    
def _handlecopy(self):
    sidx = self.coords_table.selectedIndexes()
    text = ''
    rows = [i.row() for i in sidx]
    cols = [i.column() for i in sidx]
    
    if self._rquer.isChecked():
        Node = self.Node.copy()
    elif self._rrau.isChecked():
        Node = self.Node_R.copy()
    elif self._rschalter.isChecked():
        Node = self.Node_S.copy()
    for t in range(len(rows)):
        if cols[t] == 0:
            text = text+str(Node['X'][rows[t]])+'\t'
        elif cols[t] == 1:
            text = text+str(Node['Y'][rows[t]])
        try:
            if cols[t+1] == 0:
                text = text+'\n'
        except:
            pass
    QApplication.instance().clipboard().setText(text)

def _handlepaste(self):
    self.editable_schnitt.blockSignals(True)
    self.changes +=1
    clipboard_text =  QApplication.instance().clipboard().text()
    if hasattr(self,'selection'):
        self.selection.clear()
    if self._rquer.isChecked():
        Node = self.Node.copy()
    elif self._rrau.isChecked():
        Node = self.Node_R.copy()
    elif self._rschalter.isChecked():
        Node = self.Node_S.copy()
    if clipboard_text:
        self.coords_table.blockSignals(True)
        list_ = clipboard_text.split('\n')
        cols_ = list_[0].count('\t')+1
        data  = [None]*cols_
        for i in range(cols_):
            if cols_>1:
                ilist_  = [l.split('\t')[i] for l in list_[:-1]]
            else:
                ilist_  = list_[:-1]
            data[i] = ilist_

        data_len = len(data[0])
        cid = self.coords_table.currentRow()
        pos = self.coords_table.currentColumn()
        if data_len > Node['Npoints'] - cid:
            self.coords_table.setRowCount(cid+data_len)
            for n,i in enumerate(range(cid,cid+data_len)):
                if (pos == 0) & (cols_ == 1):
                    self.coords_table.setItem(i,0,QTableWidgetItem(data[0][n]))
                elif (pos == 0) & (cols_ > 1):
                    self.coords_table.setItem(i,0,QTableWidgetItem(data[0][n]))
                    self.coords_table.setItem(i,1,QTableWidgetItem(data[1][n]))
                elif pos == 1:
                    self.coords_table.setItem(i,1,QTableWidgetItem(data[0][n]))
        self.coords_table.blockSignals(False)
        
        update_data(self,src='table')
        self.df_copy.update(self.df_db[self.changes])
        update_nodes(self)
        uqplots(self,plist = [0],ROI=True)
        self.editable_schnitt.blockSignals(False)
        if self.changes == len(self.df_db)-1:
            self.redo.setEnabled(False)
        if self.changes > 0:
            self.undo.setEnabled(True)


# =============================================================================
# Mouse Movements on plots
# =============================================================================
    
def pointer_lang(self,evt):
    try:
        mousePoint = self.langView.getViewBox().mapSceneToView(evt)
        xi = mousePoint.x()
        #yi = mousePoint.y()
        gid = int(self.lang_ID.currentText())
        gdx = (self.df_start['ID'] == gid)
        val = min(self.df_start['XL'][gdx], key=lambda x: abs(x - xi))
        idx = self.df_start[gdx][self.df_start['XL'][gdx]==val].index[0]
        
        sohle = self.df_start.loc[idx]['ZO']
        try:
            t = "{:>} {:>}\n".format('Schnitt:',self.df_pro.loc[idx]['PName'])
        except:
            t = '-\n'
        text  = t+"{:>} {:>0.2f}\n{:>} {:>}\n{:>} {:>0.2f}".format('Station:',val,'Knoten:',idx,'Sohle:  ',sohle)
        
        for i in self.df_wsp.columns:
            t = "\n{:>} {:>0.2f}".format(i+':',self.df_wsp.loc[idx][i])
            text = text+t

        self.langView.addItem(self.lang_label)
        self.langView.addItem(self.vLine_lang, ignoreBounds=False)
        self.langView.addItem(self.hLine_lang, ignoreBounds=False)
        self.lang_label.setTextWidth(150)
        self.lang_label.setZValue(10000)
        self.vLine_lang.setZValue(9999)
        self.hLine_lang.setZValue(9999)
        self.lang_label.setText(text)
        self.lang_label.setPos(val,sohle)
        self.vLine_lang.setPos(val)
        self.hLine_lang.setPos(sohle)
    except:
        pass

def pointer_node(self,evt):
    try:
        mousePoint = self.nodeView.getViewBox().mapSceneToView(evt)
        xi = mousePoint.x()
        yi = mousePoint.y()
        
        pt    = np.array([xi,yi])
        nodes = np.array(list(zip(self.df_start['X'].values,self.df_start['Y'].values)))
        dist  = np.linalg.norm(nodes - pt, ord=2, axis=1)
        
        nearest = sorted(list(zip(dist,self.df_start.index)))[0]
        try:
            sname = self.df_pro.loc[nearest[1]]['PName']
        except:
            sname = '--'

        self.nodeView.addItem(self.node_label)
        self.nodeView.addItem(self.vLine_node, ignoreBounds=False)
        self.nodeView.addItem(self.hLine_node, ignoreBounds=False)
        self.node_label.setZValue(10000)
        self.vLine_node.setZValue(9999)
        self.hLine_node.setZValue(9999)
        

        self.node_label.setText('Node: '+ str(nearest[1])+'\nStation: '+str(self.df_start.loc[nearest[1]]['XL'])+
                                 '\nSchnitt: '+sname)
        self.node_label.setPos(self.df_start.loc[nearest[1]]['X'],self.df_start.loc[nearest[1]]['Y'])
            
        self.vLine_node.setPos(self.df_start.loc[nearest[1]]['X'])
        self.hLine_node.setPos(self.df_start.loc[nearest[1]]['Y'])
    except:
        pass

def pointer_q1(self,evt):
    try:
        mousePoint = self.graphicsView.getViewBox().mapSceneToView(evt)
        xi = mousePoint.x()
        yi = mousePoint.y()
        
        pt    = np.array([xi,yi])
        nx    = np.array([float(self.coords_table.item(_x,0).text()) for _x in range(self.coords_table.rowCount())])
        ny    = np.array([float(self.coords_table.item(_y,1).text()) for _y in range(self.coords_table.rowCount())])
        nodes = np.array(list(zip(nx,ny)))
        dist  = np.linalg.norm(nodes - pt, ord=2, axis=1)
        nearest = sorted(list(zip(dist,range(len(dist)))))[0]
        
        if nearest[0] < 0.5:
            self.qplotD['pointer'][0].setText("({:0.2f},{:0.2f})".format(nx[nearest[1]],ny[nearest[1]]))
            self.qplotD['pointer'][0].setPos(nx[nearest[1]],ny[nearest[1]])
        else:
            self.qplotD['pointer'][0].setText('')

    except:
        pass
    
def pointer_q2(self,evt):
    try:
        mousePoint = self.graphicsView2.getViewBox().mapSceneToView(evt)
        xi = mousePoint.x()
        yi = mousePoint.y()
        
        pt    = np.array([xi,yi])
        nodes = np.array(list(zip(self.df_pro.loc[self.loc2]['X'],self.df_pro.loc[self.loc2]['Y'])))
        dist  = np.linalg.norm(nodes - pt, ord=2, axis=1)
        nearest = sorted(list(zip(dist,range(len(dist)))))[0]
        
        if nearest[0] < 0.5:
            self.qplotD['pointer'][1].setText("({:0.2f},{:0.2f})".format(self.df_pro.loc[self.loc2]['X'][nearest[1]],
                                             self.df_pro.loc[self.loc2]['Y'][nearest[1]]))
            self.qplotD['pointer'][1].setPos(self.df_pro.loc[self.loc2]['X'][nearest[1]],
                                             self.df_pro.loc[self.loc2]['Y'][nearest[1]])
        else:
            self.qplotD['pointer'][1].setText('')
    except:
        pass