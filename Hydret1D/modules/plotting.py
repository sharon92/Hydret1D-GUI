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

def colorpicker(myapp):
    if myapp.p_wspdat.currentColumn() == 3:
        color = QColorDialog.getColor(Qt.blue, myapp,'Choose WSP color', QColorDialog.ShowAlphaChannel)
        if color.isValid():
            myapp.p_wspdat.item(myapp.p_wspdat.currentRow(),3).setBackground(color)
                
def plot(myapp,Node,View):
    
    riv_bed_y,riv_bed_idx,riv_bed_x = riv_bed(Node)
    plot_bottom      = riv_bed_y-2
    
    View.clear()

    myapp.ax=View.plot(Node['X'],Node['Y'],pen='k',symbol='d',
                                   symbolSize=7,symbolPen='r',symbolBrush='r',
                                   fillLevel=plot_bottom,brush=(195,176,145),name = "Querschnitt")

    myapp.ax.setZValue(1000)
    myapp.axViewBox = myapp.ax.getViewBox()
    myapp.axViewBox.setAspectLocked(lock=True, ratio=myapp.ratio)
    View.getAxis('left').setLabel('Höhe')
    
    if View == myapp.graphicsView:
        myapp.ax1 = myapp.ax
        myapp.axViewBox1 = myapp.axViewBox
        myapp.plot_bottom= plot_bottom
    elif View == myapp.graphicsView2:
        myapp.ax2 = myapp.ax
        myapp.axViewBox2 = myapp.axViewBox
    
    #plot lamellen
    if View == myapp.graphicsView:
        if -1*Node.name in myapp.df_pro.index:
            View.showAxis('right')
            View.scene().addItem(myapp.p2)
            myapp.p2.setGeometry(View.getViewBox().sceneBoundingRect())
            myapp.p2.linkedViewChanged(View.getViewBox(), myapp.p2.XAxis)
            View.getAxis('right').linkToView(myapp.p2)
            myapp.p2.setXLink(View)
            View.getAxis('right').setLabel('kst', color='#0000ff')
            myapp.p2.setYRange(0,100)
            g = View.plot(myapp.Node_R['X'],myapp.Node_R['Y'],pen='k',
                                       symbolSize=3,symbolPen=0.5,symbolBrush=0.5,
                                       fillLevel=myapp.Node_R['Y'].min(),brush=0.5,
                                       name = 'k-strickler')
            g.setZValue(10001)
            myapp.p2.addItem(g)
        else:
            View.showAxis('right',show=False)
    
    '''Annotate Info'''
    
    Q = 'Q: {:0.2f} m³/s'.format(myapp.df_start.loc[Node.name]['QZERO'])
    Text = Q
    if Node.name in myapp.h1d.nupe:
        _idx = myapp.h1d.nupe.index(Node.name)
        INF = '\n\nZufluss Mode: {}\nDatei: {}\nFaktor: {:0.2f}'.format(myapp.h1d.timmod[_idx],
                      myapp.h1d.quinf_[_idx],myapp.h1d.faktor[_idx])
        try:
            INF = INF+'\nStation: {:0.1f}\nQbasis: {:0.2f}'.format(myapp.h1d.q_station[_idx],
                                myapp.h1d.q_basis[_idx])
        except:
            pass
        Text = Text+INF
    
    if Node.name in myapp.h1d.lie:
        _idx = myapp.h1d.lie.index(Node.name)
        li   = myapp.h1d.zdat[_idx].split()
        LINF = '\n\nLateral Inflow Mode: {}\nDatei: {}\nFaktor: {}'.format(li[1],
                      li[0],li[2])
        try:
            LINF = LINF+'\nStation: {:0.1f}\nQ_basis: {:0.2f}'.format(li[3],li[4])
        except:
            pass
        Text = Text+LINF
    
    if Node.name in myapp.h1d.igate:
        _idx = myapp.h1d.igate.index(Node.name)
        gatd = myapp.h1d.gatdat[_idx]
        Gate = '\n\nGate Max Durchlassöffnung: {}\nMode: {}\nQmin: {}\nQmax: {}\nTol: {}\nµ: {}\nGatmin: {}\nGatwso: {}\nDatei: {}'.format(myapp.h1d.igate[_idx],*gatd)
        Text = Text + Gate
    
    weirs = [int(we.split()[0]) for we in myapp.h1d.weir_info]
    if Node.name in weirs:
        _idx = weirs.index(Node.name)
        wehr = '\n\nWehr Höhe,Breite,µ: {},{},{}'.format(*myapp.h1d.weir_info[_idx].split()[1:4])
        Text = Text+wehr

    annotate = pg.TextItem(Text,anchor=(1,0.5),color='k',border='k',fill='w')
    View.addItem(annotate)
    annotate.setPos(Node.X.max(),Node.Y.max()+4)
    
    '''River Bed'''
    myapp.riverbed = View.plot([riv_bed_x,riv_bed_x],[riv_bed_y,plot_bottom],
                                           pen=pg.mkPen(width=1,color='b'))
    sohle = pg.TextItem(text='Tiefpunkt',anchor=(0.5,0.5),color='b',border='k',fill='w')
    View.addItem(sohle)
    sohle.setPos(riv_bed_x,plot_bottom)
    sohle.setZValue(4000) 
    myapp.riverbed.setZValue(3000)
    plan_name(myapp)
    LOB_x,LOB_y,ROB_x,ROB_y,idx_l,idx_r = cal_bank(Node,return_idx=True)
    bankXY = LOB_x,LOB_y,ROB_x,ROB_y
    plot_bank(myapp,bankXY,plot_bottom,Node['Mode'],Node,View)
    
def plot_bank(myapp,bankXY,plot_bottom,Mod,Node,View,ModeEdit = False):

    riv_bed_y,riv_bed_idx,riv_bed_x = riv_bed(Node)
    LOB_x,LOB_y,ROB_x,ROB_y = bankXY
    top_y = max(LOB_y,ROB_y) +2

    if not ModeEdit:
        if View == myapp.graphicsView2:
            View.plot([LOB_x,LOB_x],[LOB_y,top_y],pen=pg.mkPen(width=2,color='k'))
            View.plot([ROB_x,ROB_x],[ROB_y,top_y],pen=pg.mkPen(width=2,color='k')) 
            View.plot([LOB_x,ROB_x],[top_y,top_y])
            
            mode_item = pg.TextItem(text = Mod,anchor=(0.5,0.5),border='k',fill='k',color='w')
            View.addItem(mode_item)
            mode_item.setPos(riv_bed_x,top_y)
            a_l = pg.ArrowItem(angle=0,pen=None,brush='k')
            a_l.setPos(LOB_x,top_y)
            View.addItem(a_l)
            a_r = pg.ArrowItem(angle=-180,pen=None,brush='k')
            a_r.setPos(ROB_x,top_y)
            View.addItem(a_r)
        else:
            myapp.bank_l = View.plot([LOB_x,LOB_x],[LOB_y,top_y],pen=pg.mkPen(width=2,color='k'))
            myapp.bank_r = View.plot([ROB_x,ROB_x],[ROB_y,top_y],pen=pg.mkPen(width=2,color='k')) 
            myapp.bank_t = View.plot([LOB_x,ROB_x],[top_y,top_y])
            
            myapp.mode_item = pg.TextItem(text = Mod,anchor=(0.5,0.5),border='k',fill='k',color='w')
            View.addItem(myapp.mode_item)
            myapp.mode_item.setPos(riv_bed_x,top_y)
            myapp.a_l = pg.ArrowItem(angle=0,pen=None,brush='k')
            myapp.a_l.setPos(LOB_x,top_y)
            View.addItem(myapp.a_l)
            myapp.a_r = pg.ArrowItem(angle=-180,pen=None,brush='k')
            myapp.a_r.setPos(ROB_x,top_y)
            View.addItem(myapp.a_r)
    else:
        myapp.bank_l.setData([LOB_x,LOB_x],[plot_bottom,top_y])
        myapp.bank_r.setData([ROB_x,ROB_x],[plot_bottom,top_y])
        myapp.bank_t.setData([LOB_x,ROB_x],[top_y,top_y])
        myapp.mode_item.setText(Mod)
        myapp.mode_item.setPos(riv_bed_x,top_y)
        myapp.a_l.setPos(LOB_x,top_y)
        myapp.a_r.setPos(ROB_x,top_y)
        
def plot_wsp(myapp):
    for n,View in enumerate([myapp.graphicsView,myapp.graphicsView2]):
        try:
            [View.removeItem(i) for i in [myapp.wsp_bank,myapp.wsp_bank2][n]]
        except:
            pass
        
    myapp.wsp_bank,myapp.wsp_bank2 = [],[]
    
    wsp_zval1,wsp_zval2 = [],[]
    for r in range(myapp.p_wspdat.rowCount()):
        c = myapp.p_wspdat.item(r,3).background().color()
        if myapp.p_wspdat.item(r,2).checkState() == 2:
            '''wsp plot at querschnitt'''
            for Node,View in [(myapp.Node,myapp.graphicsView),(myapp.Node2,myapp.graphicsView2)]:
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
                
                wsp = myapp.df_wsp.loc[Node.name][myapp.p_wspdat.item(r,1).text()]
                leg = myapp.p_wspdat.item(r,1).text() + ',WSP= '+str(round(wsp,2))+' m+NN'
                if myapp.p_wspdat.item(r,4).checkState() == 2:
                    wsp0 = View.plot([wsp_l,wsp_r],[wsp,wsp],pen=pg.mkPen(width=1,color='k'),
                                     name = leg,fillLevel=plot_bottom,brush =c)
                else:
                    wsp0 = View.plot([wsp_l,wsp_r],[wsp,wsp],pen=pg.mkPen(width=1,color=c),
                                     name = leg)
                
                label = pg.TextItem(text=myapp.p_wspdat.item(r,1).text(),anchor=(0.5,0.5),color='k',border='k',fill='w')
                View.addItem(label)
                label.setPos(riv_bed_x,wsp)
                label.setZValue(5000) 
                
                if Node.name == myapp.Node.name:
                    myapp.wsp_bank.append(wsp0)
                    myapp.wsp_bank.append(label)
                    wsp_zval1.append(wsp)
                else:
                    myapp.wsp_bank2.append(wsp0)
                    myapp.wsp_bank2.append(label)
                    wsp_zval2.append(wsp)
            indexing1 = list(reversed([sorted(wsp_zval1).index(i) for i in wsp_zval1]))
            indexing2 = list(reversed([sorted(wsp_zval2).index(i) for i in wsp_zval2]))
            for p in range(0,len(myapp.wsp_bank),2):
                myapp.wsp_bank[p].setZValue(500+indexing1[int(p-p/2)])
                myapp.wsp_bank[p+1].setZValue(500+indexing1[int(p-p/2)]+1)
                myapp.wsp_bank2[p].setZValue(500+indexing2[int(p-p/2)])
                myapp.wsp_bank2[p+1].setZValue(500+indexing2[int(p-p/2)]+1)
            
def langPlot(myapp,i):
    myapp.langView.clear()
    idx = (myapp.df_start['ID'] == i)
    x = myapp.df_start['XL'].values[idx]
    y = myapp.df_start['ZO'].values[idx]
    pts = np.array(sorted(list(zip(x,y))))
    myapp.lsplot = myapp.langView.plot(pts, pen = 'k',symbolSize=2,
                                     symbolPen = 'b',symbolBrush = 'b',
                                     fillLevel=np.nanmin(y)-0.2, brush=(195,176,145),name='Flussbett')
    myapp.lsplot.setZValue(1000)
    myapp.langView.invertX(True)
    myapp.langView.showGrid(x=True,y=True)
    
    for r in range(myapp.p_wspdat.rowCount()):
        c = myapp.p_wspdat.item(r,3).background().color()
        if myapp.p_wspdat.item(r,2).checkState() == 2:
            '''wsp plot at längschnitt'''
            wsp_l = myapp.df_wsp[myapp.p_wspdat.item(r,1).text()].values[idx]
            sohle = myapp.df_start['XL'].values[idx]
            leg   = myapp.p_wspdat.item(r,1).text()
            if myapp.p_wspdat.item(r,4).checkState() == 2:
                myapp.langView.plot(sohle,wsp_l,pen=pg.mkPen(width=1,color='k'),
                                     name = leg,fillLevel=myapp.df_start['ZO'][idx].min()-0.2,brush =c)
            else:
                myapp.langView.plot(sohle,wsp_l,pen=pg.mkPen(width=1,color=c),
                                     name = leg)
    myapp.langView.getViewBox().setXRange(x.max(),x.min())
    myapp.langView.getViewBox().setYRange(y.min(),y.max())
    
    #highlight on nodeview
    try:
        try:
            myapp.highlight.clear()
        except:pass
    
        rshp  = shp.Reader(myapp.achse)
        for n,s in enumerate(rshp.records()):
            if s['GEW_ID'] == i:
                rec      = rshp.shapeRecords()[n]
                pts        = np.array(rec.shape.points)
                myapp.highlight = myapp.nodeView.plot(pts[:,0],pts[:,1],pen=pg.mkPen('y', width=3))
                myapp.highlight.setZValue(0)
                break
    except:
        myapp.statusbar.showMessage('ID missing in Start.dat...')

    return myapp
        
def nodePlot(myapp):
    myapp.nodeView.clear()
    #achse plot
    try:
        loadshp(myapp,myapp.achse)
    except:
        pass

    rshp  = shp.Reader(myapp.achse)
    s_gid,s_start = [],[]
    for n,i in enumerate(rshp.records()):
        s_gid.append(i['GEW_ID'])
        s_start.append(i['SSTART'])

    nTyp = myapp.df_start['ITYPE'].values
    nID  = myapp.df_start['ID'].values
    nXL  = myapp.df_start['XL'].values
    
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
            myapp.statusbar.showMessage('ID missing in Start.dat...')

        
    for i in sorted(set(myapp.df_start['ITYPE'].values)):
        if i not in [1,2,3,4,5,9]:
            idic = myapp.itype_dict[0]
            na = 'Sonstiges'
        else:
            idic = myapp.itype_dict[i]
            na = myapp.gi_ityp.itemText(i-1)
        i1 = np.where(nTyp == i)
        
        nplot = myapp.nodeView.plot(nX[i1],nY[i1],symbol=idic[0],pen=pg.mkPen(None),
                           symbolSize=idic[1],symbolPen=idic[2],symbolBrush=idic[2],name=na)
        nplot.setZValue(100)
    myapp.nodeView.setAspectLocked(lock=True, ratio=1)


def changeAR(myapp):
    myapp.ratio = 1.0/myapp.AspectRatio.value()
    try:
        myapp.axViewBox1.setAspectLocked(lock=True, ratio=myapp.ratio)
        myapp.axViewBox2.setAspectLocked(lock=True, ratio=myapp.ratio)
    except:
        pass

def plan_name(myapp):
    if myapp.p_plan.text() != '':
        myapp.graphicsView.setTitle('Knoten Nr.: '+str(myapp.Node.name)+', Plan: ' +myapp.p_plan.text())
        myapp.graphicsView2.setTitle('Knoten Nr.: '+str(myapp.Node2.name)+', Plan: ' +myapp.p_plan.text())
    else:
        myapp.graphicsView.setTitle('Knoten Nr.: '+str(myapp.Node.name))
        myapp.graphicsView2.setTitle('Knoten Nr.: '+str(myapp.Node2.name))
    myapp.df_wsp.rename(columns={myapp.mod_plan:myapp.p_plan.text()},inplace=True)
    
def loadresult(myapp):
    wspDat = QFileDialog.getOpenFileName(caption='WSP File)',filter="*.dat*")
    if wspDat[0] != '':
        try:
            myapp.wsp_view_dat.append((wspDat[0][-20:],True,QColor(28,163,236,150),True))
        except:
            myapp.wsp_view_dat.append((wspDat[0],True,QColor(28,163,236,150),True))
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
                if coln in [k for k in myapp.wsp_dict]:
                    coln = 'HQXXX_N'+str(w_iter)
                    w_iter +=1
                else:
                    coln_bool = False
            myapp.wsp_dict.append(coln)
        except:
            myapp.wsp_dict.append(coln)
        
        myapp.df_wsp = myapp.df_wsp.assign(coln = df_wsp[wkey])
        myapp.df_wsp.rename(columns={'coln':coln},inplace=True)
        wsp_df_update(myapp)
        langPlot(myapp,myapp.gewid_current)
        
def wsp_df_update(myapp):
    myapp.p_wspdat.blockSignals(True)
    myapp.p_wspdat.setRowCount(len(myapp.wsp_view_dat))
    for x in range(len(myapp.wsp_view_dat)):
        myapp.p_wspdat.setItem(x,0,QTableWidgetItem(myapp.wsp_view_dat[x][0]))
        myapp.p_wspdat.setItem(x,1,QTableWidgetItem(myapp.df_wsp.columns[x]))
        item1 = QTableWidgetItem()
        item1.setFlags(Qt.ItemIsUserCheckable | Qt.ItemIsEnabled)
        if myapp.wsp_view_dat[x][1]:
            item1.setCheckState(Qt.Checked)
            myapp.p_wspdat.setItem(x,2,item1)
        else:
            item1.setCheckState(Qt.Unchecked)
            myapp.p_wspdat.setItem(x,2,item1)
        item2 = QTableWidgetItem()
        item2.setFlags(Qt.ItemIsSelectable|Qt.ItemIsEnabled)
        myapp.p_wspdat.setItem(x,3,item2)
        myapp.p_wspdat.item(x,3).setBackground(myapp.wsp_view_dat[x][2])
        item = QTableWidgetItem()
        item.setFlags(Qt.ItemIsUserCheckable | Qt.ItemIsEnabled)
        if myapp.wsp_view_dat[x][3]:
            item.setCheckState(Qt.Checked)
            myapp.p_wspdat.setItem(x,4,item)
        else:
            item.setCheckState(Qt.Unchecked)
            myapp.p_wspdat.setItem(x,4,item)
    plot_wsp(myapp)
    langPlot(myapp,int(myapp.lang_ID.currentText()))
    myapp.p_wspdat.blockSignals(False)
    
def update_wsp(myapp):
    myapp.wsp_view_dat = []
    new_plan_names    = []
    for r in range(myapp.p_wspdat.rowCount()):
        c = myapp.p_wspdat.item(r,3).background().color()
        if myapp.p_wspdat.item(r,2).checkState() == 2:
            if myapp.p_wspdat.item(r,4).checkState() == 2:
                myapp.wsp_view_dat.append((myapp.p_wspdat.item(r,0).text(),True,c,True))
            else:
                myapp.wsp_view_dat.append((myapp.p_wspdat.item(r,0).text(),True,c,False))
        else:
            if myapp.p_wspdat.item(r,4).checkState() == 2:
                myapp.wsp_view_dat.append((myapp.p_wspdat.item(r,0).text(),False,c,True))
            else:
                myapp.wsp_view_dat.append((myapp.p_wspdat.item(r,0).text(),False,c,False))
        new_plan_names.append(myapp.p_wspdat.item(r,1).text())
    
    col_dic = {k:new_plan_names[i] for i,k in enumerate(myapp.wsp_dict)}
    myapp.df_wsp.rename(columns = col_dic,inplace=True)
    plot_wsp(myapp)
    langPlot(myapp,myapp.gewid_current)
    
def nodechange_inPRO(myapp):
    try:
        myapp.pro_mark.clear()
        myapp.nodeView.removeItem(myapp.nodeitem)
        myapp.nodeView.removeItem(myapp.nodeitem2)
    except:
        pass
    try:
        xk1     = float(myapp.df_start.loc[myapp.loc]['X'])
        yk1     = float(myapp.df_start.loc[myapp.loc]['Y'])
        xk2     = float(myapp.df_start.loc[myapp.loc2]['X'])
        yk2     = float(myapp.df_start.loc[myapp.loc2]['Y'])
        myapp.pro_mark = myapp.nodeView.plot([xk1,xk2],[yk1,yk2],pen=None,symbol='o',symbolSize='10',
                                           symbolPen='b',symbolBrush='b')
        myapp.nodeitem = pg.TextItem(text=str(myapp.loc),angle=0,border='k',fill='w',color='b')
        myapp.nodeView.addItem(myapp.nodeitem)
        myapp.nodeitem.setPos(xk1,yk1)
        myapp.nodeitem2 = pg.TextItem(text=str(myapp.loc2),angle=0,border='k',fill='w',color='b')
        myapp.nodeView.addItem(myapp.nodeitem2)
        myapp.nodeitem2.setPos(xk2,yk2)
    except:
        pass

def onXYselection(myapp):
    try:
        myapp.selection.clear()
    except:
        pass

    myapp.row_c = myapp.coords_table.selectedIndexes()
    myapp.row_c_idx = [i.row() for i in myapp.row_c]
    
    
    if myapp.Edit:
        color = 'y'
        myapp.npoints_delete.display(len(myapp.row_c_idx))
    else:
        color = 'b'
        
    if myapp._rquer.isChecked():
        Node = myapp.Node
        xs,ys = Node['X'][myapp.row_c_idx],Node['Y'][myapp.row_c_idx]
        myapp.selection = myapp.graphicsView.plot(xs,ys,pen=None,symbol='o',
                                                symbolSize=10,symbolPen=color,symbolBrush=color)
        myapp.selection.setZValue(2000)

    elif myapp._rrau.isChecked():
        Node = myapp.Node_R
        xs,ys = Node['X'][myapp.row_c_idx],Node['Y'][myapp.row_c_idx]
        myapp.selection = myapp.graphicsView.plot(xs,ys,pen=None,symbol='o',
                                                symbolSize=10,symbolPen=color,symbolBrush=color)
        myapp.selection.setZValue(2000)
        myapp.p2.addItem(myapp.selection)
        
    elif myapp._rschalter.isChecked():
        Node = myapp.Node_S
        
