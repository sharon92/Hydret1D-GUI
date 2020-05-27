# -*- coding: utf-8 -*-
'''
Created on Wed Jun 26 11:49:02 2019

@author: s.Shaji
'''

import os
import sys
import numpy            as np
import pyqtgraph        as pg
import pyqtgraph.opengl as gl
from PyQt5.QtWidgets   import QTableWidgetItem
from PyQt5.QtGui       import QColor
from modules.riverbed  import riv_bed,cal_bank

SCRIPT_DIR = os.path.dirname(os.path.realpath(sys.argv[0]))

Gradients = {
    'blue_to_red':[(0,(0,0,1)),(0.25,(0,1,1)),(0.5,(0,1,0)),(0.75,(1,1,0)),(1,(1,0,0))],
    'thermal' : [(0, (0, 0, 0)),(0.3333, (185/255, 0, 0)), (0.6666, (1, 220/255, 0)), (1, (1, 1, 1))],
    'flame'   : [(0.0, (0, 0, 0)),(0.2, (7/255, 0, 220/255)), (0.5, (236/255, 0, 134/255)), (0.8, (246/255, 246/255, 0)), (1.0, (1, 1, 1))],
    'yellowy' : [(0.0, (0, 0, 0)), (0.2328863796753704, (32/255, 0, 129/255)),(0.5257586450247, (115/255, 15/255, 1)), (0.8362738179251941, (1, 1, 0)), (1.0, (1, 1, 1))],
    'bipolar' : [(0.0, (0, 1, 1)),  (0.25, (0, 0, 1)),(0.5, (0, 0, 0)) , (0.75, (1, 0, 0)), (1.0, (1, 1, 0))],
    'grey'    : [(0.0, (0, 0, 0)), (1.0, (1, 1, 1))]
}

def plot3d(self):
    
    if hasattr(self,'items3d'):
        for item in self.items3d: self.viewer3d.removeItem(item)
    if hasattr(self,'wsp3d'):
        for item in self.wsp3d: self.viewer3d.removeItem(item)
    p = self.df_pro
    if self.df_start is not None:
        if hasattr(self,'gid3d'):
            gid   = int(self.gid3d.currentText())
        elif hasattr(self,'lang_ID'):
            gid   = int(self.lang_ID.currentText())
        idx   = self.df_start.ID==gid
        # s     = self.df_start[idx]
        p     = self.df_pro[idx]

    self.allY = p.Station.values.astype(np.float)
    self.allZ = np.unique(np.concatenate(p.Y.values).ravel())
    
    #color Gradient
    #blue to red
    grad = np.array(Gradients[self.colorMap.currentText()])
    rgb  = np.array([[*rgbt] for rgbt in grad[:,1]])
    ticks = np.linspace(0,1,self.color_dis.value())
    r = np.interp(ticks,grad[:,0].astype(np.float32),rgb[:,0])
    g = np.interp(ticks,grad[:,0].astype(np.float32),rgb[:,1])
    b = np.interp(ticks,grad[:,0].astype(np.float32),rgb[:,2])
    kst_cd = {n: (r[n],g[n],b[n]) for n,i in enumerate(ticks)}
    
    self.viewer3d.setBackgroundColor(255,255,255)
    self.viewer3d.setCameraPosition(distance=self.allY[0])
    
    de = True if self.drawEdges.isChecked() else False
    df = True if self.drawFaces.isChecked() else False
    
    #n+1 levels
    levels =  np.linspace(self.allZ.min(),self.allZ.max(),self.color_dis.value()+1)
    self.levelTable.clearContents()
    self.levelTable.setRowCount(self.color_dis.value()+1)
    self.items3d=[]
    for i in range(len(p)):
        p1 = p.iloc[i]
        try:
            p2 = float(p.iloc[i+1].Station)
        except:
            p2 = 0
        x = p1.X
        y = np.array([float(p1.Station),float(p1.Station),p2,p2])
        
        bot = np.full(len(x),[p1.Y.min()-1])
        if not all(sorted(x)==x):
            #left from center
            
            ixi = sorted(x) == x
            bot[~ixi] = p1.Y[~ixi]
        z = np.array([bot,p1.Y,p1.Y,bot])
        colors = np.full((4,len(x),4),0.05)

        #wsp
        if hasattr(self,'hz_df'):
            self.wsp3d =[]
            if self.maxwsp3d.isChecked():
                riv_bed_y,riv_bed_idx,riv_bed_x     = riv_bed(p1)
                LOB_x,LOB_y,ROB_x,ROB_y,idx_l,idx_r = cal_bank(p1,return_idx=True)
        
                '''WSP banks'''
                if idx_l == riv_bed_idx:
                    wsp_l = p1['X'][idx_l]
                else:
                    wsp_l = p1['X'][idx_l:].min()
                if idx_r == riv_bed_idx:
                    wsp_r = p1['X'][idx_r]
                else:
                    try:
                        wsp_r = p1['X'][:idx_r+1].max()
                    except:
                        wsp_r = p1['X'].max()
                
                x_wsp = np.array([wsp_l,wsp_r])
                wsp_bot = np.full(2,p1.Y.min())
                wsp = np.full(2,self.hz_df.loc[p1.name].max())
                z_wsp = np.array([wsp_bot,wsp,wsp,wsp_bot])
                wp = gl.GLSurfacePlotItem(y,x_wsp,z_wsp, colors=(0,0,1,1),shader=None,glOptions='opaque')
                self.wsp3d+=[wp]
                self.viewer3d.addItem(wp) 
            
        for k,l in enumerate(levels[:-1]):
            ix = np.logical_and(z>=l,z<levels[k+1])
            colors[...,0][ix] = kst_cd[k][0]
            colors[...,1][ix] = kst_cd[k][1]
            colors[...,2][ix] = kst_cd[k][2]
            self.levelTable.setItem(k,0,QTableWidgetItem())
            self.levelTable.item(k,0).setBackground(QColor(*[ci*255 for ci in kst_cd[k]]))
            self.levelTable.setItem(k,1,QTableWidgetItem('%.3f' %round(l,3)))
        self.levelTable.setItem(k+1,0,QTableWidgetItem())
        self.levelTable.item(k+1,0).setBackground(QColor(*[ci*255 for ci in kst_cd[k]]))
        self.levelTable.setItem(k+1,1,QTableWidgetItem('%.3f' %round(levels[-1],3)))
            
        sp = gl.GLSurfacePlotItem(y,x,z, colors=colors,drawEdges=de,drawFaces=df)
        self.items3d+=[sp]
        self.viewer3d.addItem(sp) 

def color_change_3d(self):
    
    grad = np.array(Gradients[self.colorMap.currentText()])
    rgb  = np.array([[*rgbt] for rgbt in grad[:,1]])
    ticks = np.linspace(0,1,self.color_dis.value())
    r = np.interp(ticks,grad[:,0].astype(np.float32),rgb[:,0])
    g = np.interp(ticks,grad[:,0].astype(np.float32),rgb[:,1])
    b = np.interp(ticks,grad[:,0].astype(np.float32),rgb[:,2])
    kst_cd = {n: (r[n],g[n],b[n]) for n,i in enumerate(ticks)}
    
    levels =  np.linspace(self.allZ.min(),self.allZ.max(),self.color_dis.value()+1)
    self.levelTable.clearContents()
    self.levelTable.setRowCount(self.color_dis.value()+1)
    
    for item in self.items3d:
        z = item.vertexes[...,2].reshape(item.colors.shape[0:2])
        colors = np.full((4,len(z[0]),4),0.05)
        for k,l in enumerate(levels[:-1]):
            ix = np.logical_and(z>=l,z<levels[k+1])
            colors[...,0][ix] = kst_cd[k][0]
            colors[...,1][ix] = kst_cd[k][1]
            colors[...,2][ix] = kst_cd[k][2]
            self.levelTable.setItem(k,0,QTableWidgetItem())
            self.levelTable.item(k,0).setBackground(QColor(*[ci*255 for ci in kst_cd[k]]))
            self.levelTable.setItem(k,1,QTableWidgetItem('%.3f' %round(l,3)))
        self.levelTable.setItem(k+1,0,QTableWidgetItem())
        self.levelTable.item(k+1,0).setBackground(QColor(*[ci*255 for ci in kst_cd[k]]))
        self.levelTable.setItem(k+1,1,QTableWidgetItem('%.3f' %round(levels[-1],3)))
        item.setData(colors=colors)
        item.update()
            
# def lang3d(self):
    
        #gegen fliessrichtung
        # try:
        #     p2i = p.iloc[i+1]
        #     p2 = float(p2i.Station)
        #     x2 = p2i.X
        #     sohle2 = np.abs(x2).argmin()
        #     lpts2 = len(x2[:sohle2])
        #     rpts2 = len(x2[sohle2:])
            
        #     l = lpts2 if lpts2<lpts else lpts
        #     r = rpts2 if rpts2<rpts else rpts
        #     botg[sohle-l:sohle] = p2i.Y[sohle2-l:sohle2]
        #     botg[sohle:sohle+r] = p2i.Y[sohle2:sohle2+r]
        # except:
        #     p2 = 0
        
        # y = np.array([float(p1.Station),float(p1.Station),p2,p2])

        # #mit fliessrichtung
        # if i!=0:
        #     p0i = p.iloc[i-1]
        #     x0 = p0i.X
        #     sohle0 = np.abs(x0).argmin()
        #     lpts0 = len(x0[:sohle0])
        #     rpts0 = len(x0[sohle0:])
            
        #     l = lpts0 if lpts0<lpts else lpts
        #     r = rpts0 if rpts0<rpts else rpts
        #     botg[sohle-l:sohle] = p0i.Y[sohle0-l:sohle0]
        #     botg[sohle:sohle+r] = p0i.Y[sohle0:sohle0+r]