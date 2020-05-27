# -*- coding: utf-8 -*-
"""
This example demonstrates the use of GLSurfacePlotItem.
"""

from pyqtgraph.Qt import QtCore, QtGui
import pyqtgraph as pg
import pyqtgraph.opengl as gl
import numpy as np
from modules.rawh1d      import HYDRET as h1d
from PyQt5.QtGui       import QColor
from colour import Color
hydp    = r"z:\GWOW_Hanauerland\M02_Hydret06\03179_Giesselbach\GBMW.run"

hm  = h1d(hydret_path=hydp)

start = hm.df_start
pro   = hm.df_pro

i = 18283

idx = start['ID']==i

s = start[idx]
p = pro[idx]

pts = 150

allX = np.unique(np.concatenate(p.X.values).ravel())
allY = s.XL.values
allZ = np.unique(np.concatenate(p.Y.values).ravel())

#color Gradient
#blue to red
color_dis= 30
c = list(Color("blue").range_to(Color("red"),color_dis))
kst_cd = {n: i.get_rgb() for n,i in enumerate(c)}

## Create a GL View widget to display data
app = QtGui.QApplication([])
w = gl.GLViewWidget()
w.setBackgroundColor(255,255,255)
w.show()
w.setWindowTitle('Hydret 3D')
# w.setCameraPosition(distance=allY[0])

# g = gl.GLGridItem()
# g.setSize(allY.max()*2,allX.max()+abs(allX.min()),allZ.max()-allZ.min())
# g.setSpacing(allY.max()/len(allY),(allX.max()-allX.min())/pts,(allZ.max()-allZ.min())/10)
# g.setDepthValue(130)
# w.addItem(g)
# levels =  np.linspace(allZ.min(),allZ.max(),color_dis)
#surface plot
for i in range(len(p)):
    p1 = p.iloc[i]
    try:
        p2 = float(p.iloc[i+1].Station)
    except:
        p2 = 0
    x = p1.X
    y = np.array([float(p1.Station),float(p1.Station),p2,p2])
    
    bot = np.full(len(x),[p1.Y.min()])
    if not all(sorted(x)==x):
        ixi = sorted(x) == x
        bot[~ixi] = p1.Y[~ixi]
    z = np.array([bot,p1.Y,p1.Y,bot])
    colors = np.full((4,len(x),4),0.05)
    
    d = np.empty((2,len(x),len(p1.Y),4),dtype=np.ubyte)
    d[...,0,0] = np.array([float(p1.Station)])
    d[...,0,1] = np.array([p2])
    d[...,1] = x
    d[...,2] = p1.Y
    # d[...,3] = p1.Y/
    levels =  np.linspace(z.min(),allZ.max(),color_dis)
    for k,l in enumerate(levels[:-1]):
        ix = np.logical_and(z>=l,z<levels[k+1])
        colors[...,0][ix] = kst_cd[k][0]
        colors[...,1][ix] = kst_cd[k][1]
        colors[...,2][ix] = kst_cd[k][2]
        
    sp = gl.GLVolumeItem(d)    
    # sp = gl.GLSurfacePlotItem(y,x,z, colors=colors)
    # sp.scale(1,1,10)
    w.addItem(sp) 

# Start Qt event loop unless running in interactive mode.
if __name__ == '__main__':
    import sys
    if not QtGui.QApplication.instance():
        app = QtGui.QApplication(sys.argv)
    else:
        app = QtGui.QApplication.instance().exec_()