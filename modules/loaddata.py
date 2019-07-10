# -*- coding: utf-8 -*-
"""
Created on Wed Jun 26 13:59:47 2019

@author: s.Shaji
"""
import shapefile as shp
import pyqtgraph as pg
from PyQt5.QtWidgets import QFileDialog

def lp_view_box(myapp,dataName):
    it = [myapp.lp_listWidget.item(x).text() for x in range(myapp.lp_listWidget.count())]
    if not dataName in it:
        myapp.lp_listWidget.addItem(dataName)
    
def dataadd(myapp):
    data = QFileDialog.getOpenFileName(caption='Choose Dataset',filter='*.shp')[0]
    if data != '':
        loadshp(myapp,data)

def dataremove(myapp):
    for i in myapp.rect_patch:
        i.clear()
    
def loadshp(myapp,data):
    try:
        ac = shp.Reader(data)
        for shape in ac.shapeRecords():
           pol_x = [h[0] for h in shape.shape.points[:]]
           pol_y = [h[1] for h in shape.shape.points[:]]
           myapp.nodeView.plot(pol_x,pol_y,pen=pg.mkPen('k', width=1))
        myapp.nodeView.plot(pol_x,pol_y,pen=pg.mkPen('k', width=1),name=data.split('\\')[-1][:-4])
        lp_view_box(myapp,data.split('/')[-1][:-4])
        myapp.nodeView.setAspectLocked(lock=True, ratio=1)
    except:
        myapp.statusbar.showMessage('Shapefile Error!')
            