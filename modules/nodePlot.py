# -*- coding: utf-8 -*-
'''
Created on Wed Jun 26 11:49:02 2019

@author: s.Shaji
'''

import os
import sys
import numpy             as     np
import pyqtgraph         as     pg
from dialogs.plotProp    import changeProps
from modules.loaddata    import loadshp,lp_view_box

# =============================================================================
# Initiate plotting, Plot all 4 Graphic windows and then update the data using
# update functions
# =============================================================================

SCRIPT_DIR = os.path.dirname(os.path.realpath(sys.argv[0]))

'''Plot the Aerial View here'''
def nodePlot(self):
    if not hasattr(self,'h1d'): return
    
    self.buttonGroup.blockSignals(True)
    self.buttonGroup_2.blockSignals(True)
    self.lp_listWidget.blockSignals(True)
    self.lp_listWidget.clear()
    
    self.nplotbank    = []
    
    if len(self.h1d.naughty_list)>0:
        self.nodeplanview.setChecked(True)
        self.statusbar.showMessage('ID(s) missing in Gewässerachse to properly display Map View')
        
    if self.achse is None:
        self.nodemapview.setEnabled(False)
        self.nodeplanview.setChecked(True)
        plotNode(self,typ='node')
    
    elif self.nodeplanview.isChecked():plotNode(self,typ='node')
        
    else: plotNode(self,typ='map')
    
    self.lp_listWidget.blockSignals(False)
    self.buttonGroup.blockSignals(False)
    self.buttonGroup_2.blockSignals(False)
                    
def plotNode(self,typ = 'map'):
    
    self.nodeView.clear()
    if   typ == 'map':
        self.lp_adddata.setEnabled(True)
        self.lp_removedata.setEnabled(True)
        self.nodeView.setAspectLocked(lock=True,ratio=1)
        #check for existing data
        for key in self.lw_dict.keys():
            if ('Typ: ' not in key) & ('GEW ID: ' not in key):
                if type(self.lw_dict[key]) == list:
                    [self.nodeView.addItem(i) for i in self.lw_dict[key]]
                    lp_view_box(self,key,insert=False)
                else:
                    self.nodeView.addItem(self.lw_dict[key])
                    lp_view_box(self,key,insert=False)
            else:
                if type(self.lw_dict[key]) == list:[self.nodeView.removeItem(i) for i in self.lw_dict[key]]
                else:self.nodeView.removeItem(self.lw_dict[key])
                    
        if not os.path.basename(self.achse) in self.lw_dict.keys():loadshp(self,self.achse)
        df = self.df_start
        self.nodeView.invertY(False)
        
    elif typ == 'node':
        self.lp_adddata.setEnabled(False)
        self.lp_removedata.setEnabled(False)
        df = self.h1d.df_nodes
        self.nodeView.setAspectLocked(lock=False)
        self.nodeView.invertY(True)
    
    nTyp = df['ITYPE']
    nID  = df['ID']
    nX,nY = df.X.values, df.Y.values

    for g in nID.unique():
        g1 = df[nID == g]
        nplot= self.nodeView.plot(g1.X.values,g1.Y.values,pen=pg.mkPen(color='k',width=1))
        self.nplotbank.append(nplot)
        
    if self.ityp.isChecked():
        for i in nTyp.unique():
            if   i == -4                   : na = 'Junction Station'
            elif i not in [1,2,3,4,5,6,8,9]: na = 'Sonstiges'
            else                           : na = self.gi_ityp.itemText(i-1)
            i1 = df[nTyp == i]
            if 'i'+str(i) in self.plotdefaults.keys():
                nplot = self.nodeView.plot(i1.X.values,i1.Y.values,
                                           symbol=self.symbols[self.plotdefaults['i'+str(i)][2]],
                                           pen=pg.mkPen(None),
                                           symbolSize=self.plotdefaults['i'+str(i)][3],
                                           symbolPen=self.plotdefaults['i'+str(i)][0],
                                           symbolBrush=self.plotdefaults['i'+str(i)][0])
                
                if not self.plotdefaults['i'+str(i)][1]: 
                    nplot.hide()
                    lp_view_box(self,'Typ: '+na,check=False,insert=True)
                else: 
                    nplot.show()
                    lp_view_box(self,'Typ: '+na,check=True,insert=True)
            elif i == -4:
                nplot = self.nodeView.plot(i1.X.values,i1.Y.values,pen=pg.mkPen(None),symbol='o',
                                           symbolSize=8,symbolPen='r',symbolBrush='r')
                lp_view_box(self,'Typ: '+na,check=True,insert=True)
            nplot.setZValue(100)
            self.nplotbank.append(nplot)
            self.lw_dict['Typ: '+na]    = nplot
            self.lwopt_dict['Typ: '+na] = nplot.opts

    elif self.gewid.isChecked():
        for fakex,idx in enumerate(nID.unique()):
            _nplotbank = []
            _t       = nTyp[nID == idx]
            for i in _t.unique():
                i1 = np.where((nID == idx) & (nTyp == i))
                if 'i'+str(i) in self.plotdefaults.keys():
                    nplot = self.nodeView.plot(nX[i1],nY[i1],
                                               symbol=self.symbols[self.plotdefaults['i'+str(i)][2]],
                                               pen=pg.mkPen(None),
                                               symbolSize=self.plotdefaults['i'+str(i)][3],
                                               symbolPen=self.plotdefaults['i'+str(i)][0],
                                               symbolBrush=self.plotdefaults['i'+str(i)][0])
                    if not self.plotdefaults['i'+str(i)][1]: nplot.hide()
                    else: nplot.show()
                elif i == -4:
                    nplot = self.nodeView.plot(nX[i1],nY[i1],pen=pg.mkPen(None),symbol='o',
                                               symbolSize=8,symbolPen='r',symbolBrush='r')
                nplot.setZValue(100)
                self.nplotbank.append(nplot)
                _nplotbank.append(nplot)
            self.lw_dict['GEW ID: '+str(idx)] = _nplotbank
            self.lwopt_dict['GEW ID: '+str(idx)] = _nplotbank[0].opts
            lp_view_box(self,'GEW ID: '+str(idx),insert=True)
    self.nodeView.invertX(False)
    self.nodeView.showAxis('top')
    self.nodeView.showAxis('bottom')

def nodePlotZ(self):
    Z=100
    for i in range(self.lp_listWidget.count()):
        item = self.lw_dict[self.lp_listWidget.item(i).text()]
        if type(item) == list:
            for zi in item:
                try: zi.setZValue(Z)
                except: print('Cant set Z Value')
            Z -=1
        else:
            try: zi.setZValue(Z)
            except: print('Cant set Z Value')
            Z -=1
            
def nodeBeauty(self,i):
    pitem = self.lw_dict[i.text()]
    if type(pitem) == list: p = pitem[0]
    else: p = pitem
    
    if hasattr(p,'opts'):
        ask = changeProps(self,i.text())
        if ask is not None:
            pen,symbol,symbolPen,symbolBrush,symbolSize,fillLevel,fillBrush = ask
            
            for p in pitem:
                p.opts['pen']         = pen
                p.opts['symbol']      = symbol
                p.opts['symbolPen']   = symbolPen
                p.opts['symbolBrush'] = symbolBrush
                p.opts['symbolSize']  = symbolSize
                if fillLevel:
                    p.opts['fillLevel']   = p.getData()[1].min()
                    p.opts['fillBrush']   = fillBrush
                p.update()
                p.updateItems()
            
def nodesVisibility(self,i):
    plotb = self.lw_dict[i.text()]
    if i.checkState() == 0:
        if type(plotb) is list:
            for p in plotb: p.hide()
        else: plotb.hide()
    elif i.checkState() == 2:
        if type(plotb) is list:
            for p in plotb: p.show()
        else: plotb.show()

# =============================================================================
# Mouse Movements on plots
# =============================================================================

def nodePointer(self,evt):
    if (self.lp_listWidget.count()>0) & (hasattr(self,'node_label')):
        if self.node_label.isVisible():
            self.node_label.hide()
            self.vLine_node.hide()
            self.hLine_node.hide()
        else:
            self.node_label.show()
            self.vLine_node.show()
            self.hLine_node.show()
    
def pointer_node(self,evt):
    if self.lp_listWidget.count()>0:
        try:
            self.nodeView.addItem(self.node_label)
            self.nodeView.addItem(self.vLine_node, ignoreBounds=False)
            self.nodeView.addItem(self.hLine_node, ignoreBounds=False)
            
            mousePoint = self.nodeView.getViewBox().mapSceneToView(evt)
            xi = mousePoint.x()
            yi = mousePoint.y()
            
            pt    = np.array([xi,yi])
            if   self.nodemapview.isChecked():  df = self.df_start
            elif self.nodeplanview.isChecked(): df = self.h1d.df_nodes
            nodes = np.array(list(zip(df['X'].values,df['Y'].values)))
            dist  = np.linalg.norm(nodes - pt, ord=2, axis=1)
            
            nearest = sorted(list(zip(dist,df.index)))[0]
            try:
                sname = self.df_pro.loc[nearest[1]]['PName']
            except:sname = '--'
            
            try:
                self.node_label.setText('Node: '+ str(nearest[1])+'\nStation: '+str(df.loc[nearest[1]]['XL'])+
                                         '\nSchnitt: '+sname)
            except:
                self.node_label.setText('Node: '+ str(nearest[1])+'\nSchnitt: '+sname)
            self.node_label.setPos(df.loc[nearest[1]]['X'],df.loc[nearest[1]]['Y'])
                
            self.vLine_node.setPos(df.loc[nearest[1]]['X'])
            self.hLine_node.setPos(df.loc[nearest[1]]['Y'])
        except: pass