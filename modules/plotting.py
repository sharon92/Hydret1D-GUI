# -*- coding: utf-8 -*-
'''
Created on Wed Jun 26 11:49:02 2019

@author: s.Shaji
'''

import os
import pickle
import sys
import numpy          as     np
import pandas         as     pd
import pyqtgraph      as     pg
import shapefile      as     shp
from itertools        import groupby
from shapely.geometry import LineString
from modules.loaddata import loadshp
from modules.riverbed import riv_bed,cal_bank
from PyQt5.QtWidgets  import QTableWidgetItem,QFileDialog,QColorDialog,QApplication
from PyQt5.QtCore     import Qt
from PyQt5.QtGui      import QColor,QFont

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


'''Plot the Cross-section Views here'''
def qplots(self,plist=[0,1]):
    for n in plist:
        Node = self.qplotD['node'][n]
#        rNode= self.qplotD['rnode'][n]
        View = self.qplotD['view'][n]
        View.clear()
        
        riv_bed_y,riv_bed_idx,riv_bed_x = riv_bed(Node)
        self.qplotD['plotbot'][n]       = riv_bed_y-self.plotdefaults['qf'][3]
        
        View.addLegend()
        
        self.qplotD['axis'][n] = View.plot(Node['X'],Node['Y'],
                                           pen= pg.mkPen(color=self.plotdefaults['qp'][0],
                                                         width=self.plotdefaults['qp'][3],
                                                         style=self.penstyle[self.plotdefaults['qp'][2]]),
                                           symbol=self.symbols[self.plotdefaults['q'][2]],
                                           symbolSize=self.plotdefaults['q'][3],
                                           symbolPen=self.plotdefaults['q'][0],
                                           symbolBrush=self.plotdefaults['q'][0],
                                           fillLevel=self.qplotD['plotbot'][n],
                                           brush=self.plotdefaults['qf'][0],
                                           name="Querschnitt")
        
        self.qplotD['axis'][n].setZValue(1000)

        View.getAxis('left').setLabel('Höhe')
        View.showAxis('right')
        View.getAxis('right').setLabel('Höhe')
    
        kplot(self,n)
        
        '''Annotate Info'''
        Text = textgen(Node,self)
        self.qplotD['annotate'][n] = pg.TextItem(Text,anchor=(1,0.5),color='k',border='k',fill='w')
        View.addItem(self.qplotD['annotate'][n])
        self.qplotD['annotate'][n].setPos(Node.X.max(),Node.Y.max()+4)
        
        '''River Bed'''
        self.qplotD['riverbed'][n] = View.plot([riv_bed_x,riv_bed_x],[riv_bed_y,self.qplotD['plotbot'][n]],
                                               pen=pg.mkPen(width=1,color='b'))
        self.qplotD['riverbed'][n].setZValue(3000)
        self.qplotD['sohle'][n]    = pg.TextItem(text='Tiefpunkt',anchor=(0.5,1),color='b',border='k',fill='w')
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
        self.qplotD['pointer'][n] =pg.TextItem(color='k',anchor=(0.5,1),border='k',fill='w')
        View.addItem(self.qplotD['pointer'][n])
        self.qplotD['pointer'][n].setPos(Node.X[0],Node.Y[0])
        self.qplotD['pointer'][n].setZValue(10000)
        self.qplotD['pointer'][n].hide()
    self.forceplot = False

def kplot(self,n):

    View  = self.qplotD['view'][n]
    Node  = self.qplotD['node'][n]
    rNode = self.qplotD['rnode'][n]
    riv_bed_x = riv_bed(Node)[2]

    #remove existing lamellen plots
    try:
        [View.removeItem(i) for i in self.qplotD['lambank'][n]]
        [View.removeItem(i) for i in self.qplotD['lamtbank'][n]]
    except:
        pass
    
    #plot lamellen
    self.qplotD['lambank'][n] = []
    self.qplotD['lamtbank'][n] = []
    
    if self.qplotD['rnode'][n] is not None:
        k = [list(j) for l,j in groupby(rNode.Y)]
        sidx = 0
        for nk,ik in enumerate(k):
            #len > 1 lists
            eidx = sidx+len(ik)
            if not len(ik) == 1:
                kst = round(rNode.Y[sidx]*2)/2
                if not kst in kst_cd.keys():
                    kst_cd[kst] = QColor(150,150,150)
                plt = View.plot(rNode.X[sidx:eidx],np.full(len(rNode.X[sidx:eidx]),self.qplotD['plotbot'][n]),pen=None,
                            fillLevel=self.qplotD['plotbot'][n]-1,brush=kst_cd[kst])
                self.qplotD['lambank'][n].append(plt)
                kst_txt = pg.TextItem('', anchor=(0.5,1),color='w',fill='k')
                kst_txt.setHtml('k<SUB>'+str(kst)+'</SUB>')
                kst_txt.setPos((rNode.X[sidx]+rNode.X[eidx-1])/2.,self.qplotD['plotbot'][n]-1)
                kst_txt.setZValue(20000)
                kst_txt.setFont(sfont)
                self.qplotD['lamtbank'][n].append(kst_txt)
                
            else:
                if not nk == len(k)-1:
                    kst = round(np.mean(rNode.Y[sidx:eidx+1])*2)/2
                    if not kst in kst_cd.keys():
                        kst_cd[kst] = QColor(150,150,150)
                    plt = View.plot(rNode.X[sidx:eidx+1],np.full(len(rNode.X[sidx:eidx+1]),self.qplotD['plotbot'][n]),
                                    pen=pg.mkPen(color='k',width=3), fillLevel=self.qplotD['plotbot'][n]-1,brush=kst_cd[kst])
                    self.qplotD['lambank'][n].append(plt)
                    kst_txt = pg.TextItem('', anchor=(1,0.5),angle=90,color='w',fill='k')
                    kst_txt.setHtml('k<SUB>'+str(kst)+'</SUB>')
                    kst_txt.setPos((rNode.X[sidx]+rNode.X[eidx])/2.,self.qplotD['plotbot'][n])
                    kst_txt.setZValue(10000)
                    kst_txt.setFont(sfont)
                    self.qplotD['lamtbank'][n] .append(kst_txt)
                    
            if not nk == len(k)-1:
                if not rNode.X[eidx-1] == rNode.X[eidx]:
                    kst = round(np.mean(rNode.Y[eidx-1:eidx+1])*2)/2
                    if not kst in kst_cd.keys():
                        kst_cd[kst] = QColor(150,150,150)
                    plt = View.plot(rNode.X[eidx-1:eidx+1],np.full(len(rNode.X[eidx-1:eidx+1]),self.qplotD['plotbot'][n]),
                                    pen=pg.mkPen(color='k',width=3),fillLevel=self.qplotD['plotbot'][n]-1,brush=kst_cd[kst])
                    self.qplotD['lambank'][n].append(plt)
                    kst_txt = pg.TextItem('', anchor=(1,0.5),angle=90,color='w',fill='k')
                    kst_txt.setHtml('k<SUB>'+str(kst)+'</SUB>')
                    kst_txt.setPos((rNode.X[eidx-1]+rNode.X[eidx])/2.,self.qplotD['plotbot'][n])
                    kst_txt.setZValue(10000)
                    kst_txt.setFont(sfont)
                    self.qplotD['lamtbank'][n] .append(kst_txt)
            sidx = eidx

    else:
        if self.df_s.loc[Node.name]['RNI'] == 0:
            kst = 0
        else:
            kst = round(round(1./self.df_s.loc[Node.name]['RNI'],1)*2)/2
        if not kst in kst_cd.keys():
            kst_cd[kst] = QColor(150,150,150)
        self.qplotD['lambank'][n].append(View.plot(Node.X,np.full(len(Node.X),self.qplotD['plotbot'][n]),pen=None,
                  fillLevel = self.qplotD['plotbot'][n]-1,brush=kst_cd[kst]))
        kst_txt = pg.TextItem('', anchor=(0.5,1),color='w',fill='k')
        kst_txt.setHtml('kst<SUB>'+str(kst)+'</SUB>')
        kst_txt.setPos(riv_bed_x,self.qplotD['plotbot'][n]-1)
        kst_txt.setFont(font)
        kst_txt.setZValue(10000)
        self.qplotD['lamtbank'][n] .append(kst_txt)
    [View.addItem(i) for i in self.qplotD['lamtbank'][n]]
        
'''Generate annotations'''
def textgen(Node,self):
    try:
        Q = 'Q: {:0.2f} m³/s'.format(self.df_start.loc[Node.name]['QZERO'])
    except:
        Q = ''
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
    self.lsplot = self.langView.plot(pts, pen = pg.mkPen(color=self.plotdefaults['lp'][0],
                                                         width=self.plotdefaults['lp'][3],
                                                         style=self.penstyle[self.plotdefaults['lp'][2]]),
                                     symbol=self.symbols[self.plotdefaults['l'][2]],
                                     symbolSize=self.plotdefaults['l'][3],
                                     symbolPen = self.plotdefaults['l'][0],
                                     symbolBrush =self.plotdefaults['l'][0],
                                     fillLevel=np.nanmin(y)-self.plotdefaults['lf'][3],
                                     brush=self.plotdefaults['lf'][0],name='Flussbett')
    self.lsplot.setZValue(1000)
    self.langView.invertX(True)
    self.langView.showGrid(x=True,y=True)
    
    plot_wspL(self)
    gewMarker(self)

'''Plot the Aerial View here'''
def nodePlot(self):
    self.nodeView.clear()
    
    rshp  = shp.Reader(self.achse)
    s_gid,s_start = [],[]
    for n,i in enumerate(rshp.records()):
        s_gid.append(i['GEW_ID'])
        s_start.append(i['SSTART'])

    nTyp = self.df_start['ITYPE'].values
    nID  = self.df_start['ID'].values
    nXL  = self.df_start['XL'].values
    
    nX,nY = self.df_start.X.values,self.df_start.Y.values
    
    naughty_list = []
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
            naughty_list.append(gid)
            self.statusbar.showMessage('ID missing in Start.dat...')
    nplotbank = []
    if self.nodemapview.isChecked():
        #achse plot
        try:
            loadshp(self,self.achse)
        except:
            pass
    
        for i in sorted(set(self.df_start['ITYPE'].values)):
            if not i in naughty_list:
                if i not in [1,2,3,4,5,6,8,9]:
                    na = 'Sonstiges'
                else:
                    na = self.gi_ityp.itemText(i-1)
                i1 = np.where(nTyp == i)
                
                if self.plotdefaults['i'+str(i)][1]:
                    nplot = self.nodeView.plot(nX[i1],nY[i1],
                                               symbol=self.symbols[self.plotdefaults['i'+str(i)][2]],
                                               pen=pg.mkPen(None),
                                               symbolSize=self.plotdefaults['i'+str(i)][3],
                                               symbolPen=self.plotdefaults['i'+str(i)][0],
                                               symbolBrush=self.plotdefaults['i'+str(i)][0],
                                               name=na)
                    nplot.setZValue(100)
                    nplotbank.append(nplot)
        self.nodeView.invertX(False)
        if (self.df_s.loc[self.loc]['ID'] not in naughty_list) & (self.df_s.loc[self.loc2]['ID'] not in naughty_list):
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
    
    elif self.nodeplanview.isChecked():
        for idx in np.unique(nID):
            nidx     = np.where(nID == idx)
            pbank = []
            pbank.append(self.nodeView.plot(nXL[nidx],np.full(len(nXL[nidx]),idx),pen=pg.mkPen(width=2,color='k')))
            for i in sorted(set(nTyp[nidx])):
                if i not in [1,2,3,4,5,6,8,9]:
                    na = 'Sonstiges'
                else:
                    na = self.gi_ityp.itemText(i-1)
                i1 = np.where(nTyp[nidx] == i)
                if i not in pbank:
                    if self.plotdefaults['i'+str(i)][1]:
                        nplot = self.nodeView.plot(nXL[i1],np.full(len(nXL[i1]),idx),
                                                   symbol=self.symbols[self.plotdefaults['i'+str(i)][2]],
                                                   pen=pg.mkPen(None),
                                                   symbolSize=self.plotdefaults['i'+str(i)][3],
                                                   symbolPen=self.plotdefaults['i'+str(i)][0],
                                                   symbolBrush=self.plotdefaults['i'+str(i)][0],
                                                   name=na)
                        nplot.setZValue(100)
                        nplotbank.append(nplot)
                    pbank.append(i)
                else:
                    if self.plotdefaults['i'+str(i)][1]:
                        nplot = self.nodeView.plot(nXL[i1],np.full(len(nXL[i1]),idx),
                                                   symbol=self.symbols[self.plotdefaults['i'+str(i)][2]],
                                                   pen=pg.mkPen(None),
                                                   symbolSize=self.plotdefaults['i'+str(i)][3],
                                                   symbolPen=self.plotdefaults['i'+str(i)][0],
                                                   symbolBrush=self.plotdefaults['i'+str(i)][0])
                        nplot.setZValue(100)
                        nplotbank.append(nplot)
                        
        if (self.df_s.loc[self.loc]['ID'] not in naughty_list) & (self.df_s.loc[self.loc2]['ID'] not in naughty_list):
            xk1     = float(self.df_s.loc[self.loc]['XL'])
            yk1     = float(self.df_s.loc[self.loc]['ID'])
            xk2     = float(self.df_s.loc[self.loc2]['XL'])
            yk2     = float(self.df_s.loc[self.loc2]['ID'])
        
            self.pro_mark = self.nodeView.plot([xk1,xk2],[yk1,yk2],pen=None,symbol='o',symbolSize='10',
                                               symbolPen='b',symbolBrush='b')
            self.nodeitem = pg.TextItem(text=str(self.loc),angle=0,border='k',fill='w',color='b')
            self.nodeView.addItem(self.nodeitem)
            self.nodeitem.setPos(xk1,yk1)
            self.nodeitem2 = pg.TextItem(text=str(self.loc2),angle=0,border='k',fill='w',color='b')
            self.nodeView.addItem(self.nodeitem2)
            self.nodeitem2.setPos(xk2,yk2)
        self.nodeView.setAspectLocked(None)
        self.nodeView.invertX(True)
            
    self.nodeView.getViewBox().autoRange(items=nplotbank)

'''use when loading WSP'''
def loadresult(self):
    wspDat = QFileDialog.getOpenFileName(caption='WSP File)',filter="*.dat*")
    if wspDat[0] != '':
        self.wsp_view_dat.append((os.path.basename(wspDat[0]),True,QColor(28,163,236,150),True,QColor(255,0,255,255)))
        wspdat = pd.read_csv(wspDat[0],sep=',',header=0)
        if 'WSP' in wspdat.columns:
            df_wsp = pd.read_csv(wspDat[0],sep=',',index_col = 1,header=0)
        elif 'HZERO' in wspdat.columns:
            df_wsp = pd.read_csv(wspDat[0],sep=',',index_col = 0,header=0)

        coln   = 'HQXXX_N'
        try:
            w_iter = 1
            while True:
                if coln in [k for k in self.wsp_dict]:
                    coln = 'HQXXX_N'+str(w_iter)
                    w_iter +=1
                else:
                    break
            self.wsp_dict.append(coln)
        except:
            self.wsp_dict.append(coln)
        
        self.db_wsp.append((coln,df_wsp))
        wsp_df_update(self)

'''Use wsp_df_update when changing scrolling through Cross-sections'''
def wsp_df_update(self):
    self.p_wspdat.blockSignals(True)
    self.p_wspdat.setRowCount(len(self.wsp_view_dat))
    for x in range(len(self.wsp_view_dat)):
        self.p_wspdat.setItem(x,0,QTableWidgetItem(self.wsp_view_dat[x][0]))
        self.p_wspdat.setItem(x,1,QTableWidgetItem(self.db_wsp[x][0]))
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
        item3 = QTableWidgetItem()
        item3.setFlags(Qt.ItemIsSelectable|Qt.ItemIsEnabled)
        self.p_wspdat.setItem(x,5,item3)
        self.p_wspdat.item(x,5).setBackground(self.wsp_view_dat[x][4])
    plot_wsp(self)
    if hasattr(self,'lsplot'):
        plot_wspL(self)
    self.p_wspdat.blockSignals(False)

'''use when changing entries in the WSP Table'''
def update_wsp(self):
    self.wsp_view_dat = []
    for r in range(self.p_wspdat.rowCount()):
        c = self.p_wspdat.item(r,3).background().color()
        ce= self.p_wspdat.item(r,4).background().color()
        if self.p_wspdat.item(r,2).checkState() == 2:
            if self.p_wspdat.item(r,4).checkState() == 2:
                self.wsp_view_dat.append((self.p_wspdat.item(r,0).text(),True,c,True,ce))
            else:
                self.wsp_view_dat.append((self.p_wspdat.item(r,0).text(),True,c,False,ce))
        else:
            if self.p_wspdat.item(r,4).checkState() == 2:
                self.wsp_view_dat.append((self.p_wspdat.item(r,0).text(),False,c,True,ce))
            else:
                self.wsp_view_dat.append((self.p_wspdat.item(r,0).text(),False,c,False,ce))
        self.db_wsp[r] = (self.p_wspdat.item(r,1).text(),self.db_wsp[r][1])
    
    plot_wsp(self)
    plot_wspL(self)
    
'''Plot WSP for the Cross-Sections'''
def plot_wsp(self):
    for n,View in enumerate([self.graphicsView,self.graphicsView2]):
        try:
            [View.plotItem.legend.removeItem(i) for li in self.qplotD['wbank'][n].keys() for i in self.qplotD['wbank'][n][li][0]]
            [View.removeItem(i) for li in self.qplotD['wbank'][n].keys() for i in self.qplotD['wbank'][n][li][0]]
            [View.removeItem(self.qplotD['wbank'][n][i][1]) for i in self.qplotD['wbank'][n].keys()]
        except:
            pass
    
    wsp_bank1,wsp_bank2 = {},{}
    for r in range(self.p_wspdat.rowCount()):
        c = self.p_wspdat.item(r,3).background().color()
        ce = self.p_wspdat.item(r,5).background().color()
        
        if self.p_wspdat.item(r,2).checkState() == 2:
            '''wsp plot at querschnitt'''
            for Node,View in [(self.Node,self.graphicsView),(self.Node2,self.graphicsView2)]:
                riv_bed_y,riv_bed_idx,riv_bed_x     = riv_bed(Node)
                plot_bottom                         = riv_bed_y-self.plotdefaults['qf'][3]
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
                
                if 'HZERO' in self.db_wsp[r][1].columns:
                    wsp = self.db_wsp[r][1].loc[Node.name]['HZERO']
                else:
                    wsp = self.db_wsp[r][1].loc[Node.name]['WSP']
                    hel = self.db_wsp[r][1].loc[Node.name]['HEL']
                    
                leg = self.p_wspdat.item(r,1).text() + ',WSP= '+str(round(wsp,2))+' m+NN'
                label = pg.TextItem(text=self.p_wspdat.item(r,1).text(),anchor=(0.5,0.5),color='k',border='k',fill='w')
                View.addItem(label)
                label.setPos(riv_bed_x,wsp)
                
                pw = self.plotdefaults['w'][3]
                ps = self.penstyle[self.plotdefaults['w'][2]]
                pc = self.plotdefaults['w'][0]
                if self.p_wspdat.item(r,4).checkState() == 2:
                    wsp0 = View.plot([wsp_l,wsp_r],[wsp,wsp],pen=pg.mkPen(width=pw,color=pc,style=ps),
                                     fillLevel=plot_bottom,brush =c,name=leg)
                else:
                    wsp0 = View.plot([wsp_l,wsp_r],[wsp,wsp],pen=pg.mkPen(width=pw,color=pc,style=ps),
                                     name=leg)

                if not 'HZERO' in self.db_wsp[r][1].columns:
                    leg = 'Energielinie, '+self.p_wspdat.item(r,1).text()
                    hel0 = View.plot([wsp_l,wsp_r],[hel,hel],
                                     pen=pg.mkPen(width=self.plotdefaults['e'][3],
                                                  color=ce,
                                                  style=self.penstyle[self.plotdefaults['e'][2]]),
                                         name=leg)
                if Node.name == self.Node.name:
                    if 'HZERO' in self.db_wsp[r][1].columns:
                        wsp_bank1[self.db_wsp[r][0]] = [[wsp0],label,500-wsp]
                    else:
                        wsp_bank1[self.db_wsp[r][0]] = [[wsp0,hel0],label,500-wsp]
                else:
                    if 'HZERO' in self.db_wsp[r][1].columns:
                        wsp_bank2[self.db_wsp[r][0]] = [[wsp0],label,500-wsp]
                    else:
                        wsp_bank2[self.db_wsp[r][0]] = [[wsp0,hel0],label,500-wsp]
                    
    #setting Z Values
    for key in wsp_bank1.keys():
        [item.setZValue(wsp_bank1[key][2]+pn) for pn,item in enumerate(wsp_bank1[key][0])]
        [item.setZValue(wsp_bank2[key][2]+pn) for pn,item in enumerate(wsp_bank2[key][0])]
        wsp_bank1[key][1].setZValue(5000+wsp_bank1[key][2])
        wsp_bank2[key][1].setZValue(5000+wsp_bank2[key][2])
    self.qplotD['wbank'] = [wsp_bank1,wsp_bank2]

def plot_wspL(self):
    try:
        [self.langView.plotItem.removeItem(item) for i in self.wsp_bankL.keys() for item in self.wsp_bankL[i]]
    except: pass

    i = self.gewid_current
    idxS = (self.df_start['ID'] == i)
    idx = (self.df_wsp['ID'] == i)
    
    self.wsp_bankL = {}
    for r in range(self.p_wspdat.rowCount()):
        c = self.p_wspdat.item(r,3).background().color()
        ce= self.p_wspdat.item(r,5).background().color()
        
        if self.p_wspdat.item(r,2).checkState() == 2:

            '''wsp plot at längschnitt'''
            if 'HZERO' in self.db_wsp[r][1].columns:
                stat = self.df_start['XL'].values[idxS]
                wsp_l = self.df_start['HZERO'].values[idxS]
            else:
                stat = self.df_wsp['STAT'].values[idx]
                wsp_l = self.db_wsp[r][1]['WSP'].values[idx]
                hel_l = self.db_wsp[r][1]['HEL'].values[idx]
            
            leg   = self.p_wspdat.item(r,1).text()
            
            pw = self.plotdefaults['w'][3]
            pc = self.plotdefaults['w'][0]
            ps = self.penstyle[self.plotdefaults['w'][2]]
            
            if self.p_wspdat.item(r,4).checkState() == 2:
                wsp = self.langView.plot(stat,wsp_l,
                                         pen=pg.mkPen(width=pw,color=pc,style=ps),
                                         name = leg,
                                         fillLevel=self.df_start['ZO'][idxS].min()-self.plotdefaults['lf'][3],
                                         brush =c)
            else:
                wsp = self.langView.plot(stat,wsp_l,pen=pg.mkPen(width=pw,color=pc,style=ps),
                                     name = leg)
            wsp.setZValue(0)
            if not 'HZERO' in self.db_wsp[r][1].columns:
                leg = 'Energielinie, '+self.p_wspdat.item(r,1).text()
                hel = self.langView.plot(stat,hel_l,pen=pg.mkPen(width=self.plotdefaults['e'][3],
                                                                  color=ce,
                                                                  style=self.penstyle[self.plotdefaults['e'][2]]),
                                     name = leg)
                hel.setZValue(1)
                self.wsp_bankL[self.db_wsp[r][0]] = [wsp,hel]
            else:
                self.wsp_bankL[self.db_wsp[r][0]] = [wsp]
                
    pitems = [items for subitems in self.wsp_bankL.keys() for items in self.wsp_bankL[subitems]]
    self.langView.getViewBox().autoRange(items=pitems.append(self.lsplot))
    
def colorpicker(self):
    col = self.p_wspdat.currentColumn()
    if col in [3,5,6]:
        color = QColorDialog.getColor(parent = self,title = 'Choose WSP color',options= QColorDialog.ShowAlphaChannel)
        if color.isValid():
            self.p_wspdat.item(self.p_wspdat.currentRow(),col).setBackground(color)

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
#        rNode = self.qplotD['rnode'][n]
#        sNode = self.qplotD['snode'][n]
        View  = self.qplotD['view'][n]
    
        riv_bed_y,riv_bed_idx,riv_bed_x = riv_bed(Node)
        self.qplotD['plotbot'][n]       = riv_bed_y-self.plotdefaults['qf'][3]
        
        self.qplotD['axis'][n].setData(Node['X'],Node['Y'],fillLevel=self.qplotD['plotbot'][n])

        #plot lamellen
        kplot(self,n)
        
        if ROI:
            self.editable_schnitt.blockSignals(True)
            self.editable_schnitt.setPoints(np.vstack((Node.X,Node.Y)).T)
            self.editable_schnitt.blockSignals(False)
            
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
        '''Starting WSP'''
        if Node.name in self.df_start.index:
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
                    

        for r in self.db_wsp:
            if r[0] in self.qplotD['wbank'][n].keys():
                if Node.name in r[1].index:
                    if 'HZERO' in r[1].columns:
                        wsp = r[1].loc[Node.name]['HZERO']
                    else:
                        wsp = r[1].loc[Node.name]['WSP']
                        hel = r[1].loc[Node.name]['HEL']
                    self.qplotD['wbank'][n][r[0]][0][0].show()
                    leg = r[0] + ',WSP= '+str(round(wsp,2))+' m+NN'
                    self.qplotD['wbank'][n][r[0]][0][0].setData([wsp_l,wsp_r],[wsp,wsp],fillLevel=self.qplotD['plotbot'][n])
                    View.plotItem.legend.removeItem(self.qplotD['wbank'][n][r[0]][0][0])
                    View.plotItem.legend.addItem(self.qplotD['wbank'][n][r[0]][0][0],leg)
                    
                    if len(self.qplotD['wbank'][n][r[0]][0])>1:
                        self.qplotD['wbank'][n][r[0]][0][1].show()
                        self.qplotD['wbank'][n][r[0]][0][1].setData([wsp_l,wsp_r],[hel,hel])
                    
                    self.qplotD['wbank'][n][r[0]][1].show()
                    self.qplotD['wbank'][n][r[0]][1].setPos(riv_bed_x,wsp)
                else:
                    [witem.hide() for witem in self.qplotD['wbank'][n][r[0]][0]]
                    [View.plotItem.legend.removeItem(witem) for witem in self.qplotD['wbank'][n][r[0]][0]]
                    self.qplotD['wbank'][n][r[0]][1].hide()

        View.getViewBox().autoRange(items=[*self.qplotD['lambank'][n],self.qplotD['annotate'][n]])

'''Update banks for the Cross-sections'''
def update_banks(self,n):
    
    Node = self.qplotD['node'][n]
    riv_bed_y,riv_bed_idx,riv_bed_x = riv_bed(Node)
    LOB_x,LOB_y,ROB_x,ROB_y = cal_bank(Node)
    top_y = max(LOB_y,ROB_y) +2
    if (riv_bed_x <= LOB_x) or (riv_bed_x >= ROB_x):
        riv_bed_x = (LOB_x+ROB_x)/2.
        
    self.qplotD['rbank'][n][0].setData([LOB_x,LOB_x],[LOB_y,top_y])
    self.qplotD['rbank'][n][1].setData([ROB_x,ROB_x],[ROB_y,top_y])
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
    self.lsplot.setData(pts,fillLevel=np.nanmin(y)-self.plotdefaults['lf'][3])
    
    for r in self.db_wsp:
        if r[0] in self.wsp_bankL.keys():
            if 'HZERO' in r[1].columns:
                stat  = r[1]['XL'].values[idx]
                wsp_l = r[1]['HZERO'].values[idx]
                self.wsp_bankL[r[0]][0].setData(stat,wsp_l,fillLevel=np.nanmin(y)-self.plotdefaults['lf'][3])
            else:
                stat  = r[1]['STAT'].values[idx]
                wsp_l = r[1]['WSP'].values[idx]
                hel_l = r[1]['HEL'].values[idx]
                self.wsp_bankL[r[0]][0].setData(stat,wsp_l,fillLevel=np.nanmin(y)-self.plotdefaults['lf'][3])
                self.wsp_bankL[r[0]][1].setData(stat,hel_l)
    pitems = [items for subitems in self.wsp_bankL.keys() for items in self.wsp_bankL[subitems]]
    self.langView.getViewBox().autoRange(items=pitems.append(self.lsplot))
#    self.langView.scene().sigMouseClicked.emit(QEvent.MouseButtonPress)
#    gewMarker(self)
    
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
        if (self.loc in df.index) & (self.loc2 in df.index):
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
    if hasattr(self,'highlight'):
        self.nodeView.plotItem.legend.removeItem(self.highlight)
        self.highlight.clear()
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
    if self.arbox.isChecked():
        self.qplotD['axis'][0].getViewBox().setAspectLocked(lock=True, ratio=1.0/self.AspectRatio.value())
        self.qplotD['axis'][1].getViewBox().setAspectLocked(lock=True, ratio=1.0/self.AspectRatio.value())
    else:
        self.qplotD['axis'][0].getViewBox().setAspectLocked(None)
        self.qplotD['axis'][1].getViewBox().setAspectLocked(None)

def plan_name(self):
    if self.p_plan.text() != '':
        self.graphicsView.setTitle('Knoten Nr.: '+str(self.Node.name)+', Plan: ' +self.p_plan.text())
        self.graphicsView2.setTitle('Knoten Nr.: '+str(self.Node2.name)+', Plan: ' +self.p_plan.text())
    else:
        self.graphicsView.setTitle('Knoten Nr.: '+str(self.Node.name))
        self.graphicsView2.setTitle('Knoten Nr.: '+str(self.Node2.name))

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

def del_but(self):
    self.editable_schnitt.blockSignals(True)
    self.coords_table.blockSignals(True)
    if not len(self.df_db) == self.changes + 1:
        self.df_db = self.df_db[:self.changes+1]
        self.redo.setEnabled(False)
    self.changes +=1
    self.undo.setEnabled(True)
    
    [self.coords_table.removeRow(i) for i in reversed(sorted(set([r.row() for r in self.coords_table.selectedItems()])))]
    update_data(self,src='table')
    
    self.df_copy.update(self.df_db[self.changes])
    update_nodes(self)
    uqplots(self,plist = [0],ROI=True)
    
    self.selection.clear()
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
    uqplots(self,plist = [0],ROI=False)
    self.undo.setEnabled(True)
    self.coords_table.blockSignals(False)
    
def update_nodes(self):
    #update nodes object
    self.qplotD['node'][0] = self.df_copy.loc[self.loc]
    if not self.qplotD['riloc'][0] == None:
        self.qplotD['rnode'][0] = self.df_copy.iloc[self.qplotD['riloc'][0]]
    if not self.qplotD['siloc'][0] == None:
        self.qplotD['snode'][0] = self.df_copy.iloc[self.qplotD['siloc'][0]]
        
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
    if not len(self.df_db) == self.changes + 1:
        self.df_db = self.df_db[:self.changes+1]
        self.redo.setEnabled(False)
    self.undo.setEnabled(True)
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
        
#        for i in self.db_wsp.columns:
#            t = "\n{:>} {:>0.2f}".format(i+':',self.db_wsp.loc[idx][i])
#            text = text+t
        self.lang_label.show()
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
        
        if nearest[0] < 1.5:
            self.qplotD['pointer'][0].show()
            self.qplotD['pointer'][0].setText("{:0.2f},{:0.2f}".format(nx[nearest[1]],ny[nearest[1]]))
            self.qplotD['pointer'][0].setPos(nx[nearest[1]],ny[nearest[1]])
        else:
            self.qplotD['pointer'][0].hide()
            #self.qplotD['pointer'][0].setText('')

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
        
        if nearest[0] < 1.5:
            self.qplotD['pointer'][1].show()
            self.qplotD['pointer'][1].setText("{:0.2f},{:0.2f}".format(self.df_pro.loc[self.loc2]['X'][nearest[1]],
                                             self.df_pro.loc[self.loc2]['Y'][nearest[1]]))
            self.qplotD['pointer'][1].setPos(self.df_pro.loc[self.loc2]['X'][nearest[1]],
                                             self.df_pro.loc[self.loc2]['Y'][nearest[1]])
        else:
            self.qplotD['pointer'][1].hide()
    except:
        pass