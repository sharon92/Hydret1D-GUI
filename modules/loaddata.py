# -*- coding: utf-8 -*-
"""
Created on Wed Jun 26 13:59:47 2019

@author: s.Shaji
"""
import os
import shapefile        as     shp
import pyqtgraph        as     pg
import numpy            as     np
from dialogs.loadRaster import defineCatalog
from PIL                import Image
from PyQt5.QtWidgets    import QFileDialog,QListWidgetItem
from PyQt5.QtCore       import Qt,QRectF
from shapely.geometry   import Polygon,MultiPoint

def lp_view_box(self,dataName,check=True,insert=True):
    item = QListWidgetItem(str(dataName))
    if check: item.setCheckState(Qt.Checked)
    else    : item.setCheckState(Qt.Unchecked)
    if insert: self.lp_listWidget.insertItem(0,item)
    else     : self.lp_listWidget.addItem(item)
    
def dataadd(self):
    data,ext = QFileDialog.getOpenFileName(caption='Choose Dataset',filter='Shapefile *.shp;;Orthophotos Catalog *.dbf')
    if data != '':
        if os.path.splitext(os.path.basename(data))[1].lower() == '.shp':
            loadshpfile(self,data)
        elif os.path.splitext(os.path.basename(data))[1].lower() == '.dbf':
            loadras(self,data)
            
def dataremove(self):
    row = self.lp_listWidget.currentRow()
    i   = self.lp_listWidget.item(row)
    try:
        if ('Typ: ' not in i.text()) & ('GEW ID: ' not in i.text()) & (os.path.basename(self.achse) != i.text()):   
            self.lp_listWidget.takeItem(row)
            self.lp_listWidget.removeItemWidget(i)
            if type(self.lw_dict[i.text()]) == list:
                [self.nodeView.removeItem(it) for it in self.lw_dict[i.text()]]
            else: self.nodeView.removeItem(self.lw_dict[i.text()])
            del self.lw_dict[i.text()]
            del self.lwopt_dict[i.text()]
            try:
                del self.loadshp_dict[i.text()]
            except:pass
        else:
            i.setCheckState(Qt.Unchecked)
    except:pass

def loadshpfile(self,data):
    try:
        ac = shp.Reader(data)
        ac.encodingErrors = 'ignore'
        shpplot = []
        if ac.shapeType in [3,13,23,31]:
            for n,shape in enumerate(ac.shapeRecords()):
                pol_x = [h[0] for h in shape.shape.points[:]]
                pol_y = [h[1] for h in shape.shape.points[:]]
                shpplot.append(self.nodeView.plot(pol_x,pol_y,pen='k'))
        elif ac.shapeType in [5,15,25]:
            color=tuple(np.random.choice(range(256), size=3))
            for n,shape in enumerate(ac.shapeRecords()):
                pol_x = [h[0] for h in shape.shape.points[:]]
                pol_y = [h[1] for h in shape.shape.points[:]]
                shpplot.append(self.nodeView.plot(pol_x,pol_y,pen='k',fillLevel=min(pol_y),fillBrush=color))
        else:
            pol_x,pol_y = [],[]
            for n,shape in enumerate(ac.shapeRecords()):
                pol_x.append(*[h[0] for h in shape.shape.points])
                pol_y.append(*[h[1] for h in shape.shape.points])
            shpplot.append(self.nodeView.plot(pol_x,pol_y,pen=pg.mkPen(None),symbolPen='k',symbolSize=5))
        self.nodeView.setAspectLocked(lock=True, ratio=1)
        lpname = os.path.basename(data)
        self.lw_dict[lpname]      = shpplot
        self.loadshp_dict[lpname] = (data,None)
        self.lwopt_dict[lpname]  = shpplot[0].opts
        lp_view_box(self,lpname,insert=True)
    except:
        self.statusbar.showMessage('Shapefile Error! '+data)
    
def loadshp(self,data):
    try:
        ac = shp.Reader(data)
        ac.encodingErrors = 'ignore'
        self.achseplot = []
        for n,i in enumerate(ac.records()):
            if i['GEW_ID'] in self.df_start['ID'].unique():
                shape = ac.shapeRecords()[n]
                pol_x = [h[0] for h in shape.shape.points[:]]
                pol_y = [h[1] for h in shape.shape.points[:]]
                self.achseplot.append(self.nodeView.plot(pol_x,pol_y,pen=pg.mkPen('b', width=1.5)))
        self.nodeView.setAspectLocked(lock=True, ratio=1)
        lpname = os.path.basename(data)
        self.lw_dict[lpname] = self.achseplot
        self.lwopt_dict[lpname] = self.achseplot[0].opts
        lp_view_box(self,lpname,insert=False)
    except: self.statusbar.showMessage('Shapefile Error! '+data)

def loadras(self,data,props=None):
    
    self.statusbar.showMessage('Loading Catalog...')
    ra = shp.Reader(data)
    ra.encodingErrors = 'ignore'
    
    if props is None:
        fields = [i[0] for i in ra.fields[1:]]
        props = defineCatalog(self,fields)
    
    if props is not None:
        imt,xmit,ymit,xmat,ymat,red,fil,dist = props
            
        imgs,polys = [],[]
        for i in ra.records():
            imgs.append(i[imt])
            xmi = i[xmit]
            xma = i[xmat]
            ymi = i[ymit]
            yma = i[ymat]
            
            polys.append(Polygon(((xmi,ymi),(xmi,yma),(xma,yma),(xma,ymi),(xmi,ymi))))
        
    #    mp = []
        
        #GEW
    #    ac = shp.Reader(self.achse)
    #    ac.encodingErrors = 'ignore'
    #    for n,i in enumerate(ac.records()):
    #        if i['GEW_ID'] in self.df_start['ID'].unique():
    #            shape = ac.shapeRecords()[n]
    #            mp.append(shape.shape.points)
    #    mp = [l for i in mp for l in i]
    #    PointCloud = MultiPoint(mp)
        
        PointCloud = MultiPoint(list(zip(self.df_start.X.values,self.df_start.Y.values)))
        
        img_stack = []
        for n,p in enumerate(polys):
            if p.distance(PointCloud) < dist:
                img_stack.append((n,imgs[n]))
        
        self.img_bank = []
        for n,i in img_stack:
            self.statusbar.showMessage('Loading '+os.path.basename(i)+' ...')
            im = Image.open(i)
            w,h = im.size
            im = im.resize((int(w/red),int(h/red)),Image.__dict__[fil])
            im = np.rot90(np.array(im.getdata()).reshape(im.size[0], im.size[1], 3),3)
            img = pg.ImageItem(im)
            x,y = polys[n].exterior.coords.xy
            img.setRect(QRectF(min(x),min(y),max(x)-min(x),max(y)-min(y)))
            img.setZValue(-100)
            self.img_bank.append(img)
            self.nodeView.addItem(img)
            self.statusbar.showMessage('Plotting...')
        
        lpname = os.path.basename(data)
        self.lw_dict[lpname] = self.img_bank
        self.loadshp_dict[lpname] = (data,(imt,xmit,ymit,xmat,ymat,red,fil,dist))
        lp_view_box(self,lpname,insert=False) 
        self.statusbar.showMessage('Ready')
    
    else:
        self.statusbar.showMessage('Ready')
