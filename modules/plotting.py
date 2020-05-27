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
from PyQt5.QtWidgets   import QTableWidgetItem,QApplication
from PyQt5.QtGui       import QColor,QFont
from modules.plot3d    import plot3d
# =============================================================================
# Initiate plotting, Plot all 4 Graphic windows and then update the data using
# update functions
# =============================================================================
SCRIPT_DIR = os.path.dirname(os.path.realpath(sys.argv[0]))

#strickler color gradient
kst_color = []
for i in range(255,0,-10): kst_color +=[QColor(i,255,0)]
for i in range(0,256,10) : kst_color +=[QColor(0,255,i)]
for i in range(0,256,10) : kst_color +=[QColor(255,i,0)]
for i in range(0,256,-10): kst_color +=[QColor(0,i,255)]
kst_cd = {i: kst_color[n] for n,i in enumerate(np.arange(0,len(kst_color)/2.,0.5))}

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
        Mode = self.qplotD['node'][n].Mode
#        rNode= self.qplotD['rnode'][n]
        View = self.qplotD['view'][n]
        View.clear()
        if Node.Npoints < 1: 
            self.statusbar.showMessage('Knoten '+str(self.loc)+': X&Y nicht vorhanden!')
            return
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
        if Text != '':
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
        mode_item = pg.TextItem(text = Mode,anchor=(0.5,0.5),border='k',fill='k',color='w')
        View.addItem(mode_item)
        mode_item.setPos(riv_bed_x,top_y)
        self.qplotD['rbank'][n] = [bank_l,bank_r,bank_t,a_l,a_r,mode_item]
                
        '''Pointer'''
        self.qplotD['pointer'][n] =pg.TextItem(color='k',anchor=(0.5,1),border='k',fill='w')
        View.addItem(self.qplotD['pointer'][n])
        self.qplotD['pointer'][n].setPos(Node.X[0],Node.Y[0])
        self.qplotD['pointer'][n].setZValue(10000)
        self.qplotD['pointer'][n].hide()
    if hasattr(self,'lw_dictL'):
        self.lw_dictL['sohle'][0:2] = [*self.qplotD['axis']]
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
        if hasattr(self,'df_s'):
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
    for i in self.qplotD['lamtbank'][n]:View.addItem(i) 
        
'''Generate annotations'''
def textgen(Node,self):
    if hasattr(self,'qz_df'):
        Q = 'Q: {:0.2f} m³/s'.format(self.qz_df.loc[Node.name][self.lzeitslider.value()])
    else:
        try:
            Q = 'Q: {:0.2f} m³/s'.format(self.df_start.loc[Node.name]['QZERO'])
        except:
            Q = ''
    Text = Q
    
    if hasattr(self,'h1d'):
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
    self.qlangView.clear()
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
    self.langView.showAxis('right')
    self.langView.showAxis('top')
    self.lw_dictL['sohle'][2]    = self.lsplot
    self.lwopt_dictL['sohle']    = self.lsplot.opts
    self.langView.getAxis('left').setLabel('Höhe [m]')
    self.langView.getAxis('right').setLabel('Höhe [m]')
    self.langView.getAxis('bottom').setLabel('Station [m]')
    self.langView.getAxis('top').setLabel('Station [m]')
    plot_wspL(self)
    plot_qL(self)

'''Plot WSP for the Cross-Sections'''
def plot_wsp(self):
    c = QColor(0,255,255,150)
    '''wsp plot at querschnitt'''
    for n,(Node,View) in enumerate([(self.Node,self.graphicsView),(self.Node2,self.graphicsView2)]):
        riv_bed_y,riv_bed_idx,riv_bed_x     = riv_bed(Node)
        plot_bottom                         = riv_bed_y-self.plotdefaults['qf'][3]
        LOB_x,LOB_y,ROB_x,ROB_y,idx_l,idx_r = cal_bank(Node,return_idx=True)

        '''WSP banks'''
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
        
        wspA = self.df_start.loc[Node.name]['HZERO']
        legA = 'Anfangs WSP= '+str(round(wspA,2))+' m+NN'
        
        if hasattr(self,'hz_df'):
            wsp  = self.hz_df.loc[Node.name][self.lzeitslider.value()]
            wspmin = self.hz_df.loc[Node.name].min()
            wspmax = self.hz_df.loc[Node.name].max()
   
            legW = 'WSP= '+str(round(wspA,2))+' m+NN'
            legMi = 'min WSP= '+str(round(wspmin,2))+' m+NN'
            legMa = 'max WSP= '+str(round(wspmax,2))+' m+NN'
        
        pw = self.plotdefaults['w'][3]
        ps = self.penstyle[self.plotdefaults['w'][2]]
        pc = self.plotdefaults['w'][0]
        
        if n == 0:
            self.anfangs_wspQ1 = View.plot([wsp_l,wsp_r],[wspA,wspA],pen=pg.mkPen(width=2,color='m',style=2),
                             name=legA)
            self.anfangs_wspQ1.setZValue(1)
            
            if hasattr(self,'hz_df'):         
                self.wspQ1 = View.plot([wsp_l,wsp_r],[wsp,wsp],pen=pg.mkPen(width=pw,color=pc,style=ps),
                     fillLevel=plot_bottom,brush =c,name=legW)
                self.wspQ1.setZValue(0)
                
                self.wspT1 = View.plot([riv_bed_x,riv_bed_x],[riv_bed_y,wsp],color='k')
                self.wspT1.setZValue(5)
                
                self.wspTT1 = pg.TextItem('%.2f m' %(wsp-riv_bed_y),anchor=(0.5,0.5),color='w',fill='k')
                self.wspTT1.setPos(riv_bed_x,(wsp+riv_bed_y)/2)
                self.wspTT1.setZValue(5000)
                View.addItem(self.wspTT1)
                
                self.wspmaxQ1 = View.plot([wsp_l,wsp_r],[wspmax,wspmax],pen=pg.mkPen(width=2,color='b',style=2),
                                 name=legMa)
                self.wspmaxQ1.setZValue(2)

                self.wspminQ1 = View.plot([wsp_l,wsp_r],[wspmin,wspmin],pen=pg.mkPen(width=2,color='r',style=2),
                                 name=legMi)
                self.wspminQ1.setZValue(2)
            
        elif n == 1:
            self.anfangs_wspQ2 = View.plot([wsp_l,wsp_r],[wspA,wspA],pen=pg.mkPen(width=2,color='m',style=2),
                             name=legA)
            self.anfangs_wspQ2.setZValue(1)
            
            if hasattr(self,'hz_df'):
                self.wspQ2 = View.plot([wsp_l,wsp_r],[wsp,wsp],pen=pg.mkPen(width=pw,color=pc,style=ps),
                     fillLevel=plot_bottom,brush =c,name=legW)
                self.wspQ2.setZValue(0)
                
                self.wspT2 = View.plot([riv_bed_x,riv_bed_x],[riv_bed_y,wsp],color='k')
                self.wspT2.setZValue(5)
                
                self.wspTT2 = pg.TextItem('%.2f m' %(wsp-riv_bed_y),anchor=(0.5,0.5),color='w',fill='k')
                self.wspTT2.setPos(riv_bed_x,(wsp+riv_bed_y)/2)
                self.wspTT2.setZValue(5000)
                View.addItem(self.wspTT2)
                
                self.wspmaxQ2 = View.plot([wsp_l,wsp_r],[wspmax,wspmax],pen=pg.mkPen(width=2,color='b',style=2),
                     name=legMa)
                self.wspmaxQ2.setZValue(2)

                self.wspminQ2 = View.plot([wsp_l,wsp_r],[wspmin,wspmin],pen=pg.mkPen(width=2,color='r',style=2),
                                 name=legMi)
                self.wspminQ2.setZValue(2)
                
    self.lw_dictL['Anfangs WSP'][0:2]     = [self.anfangs_wspQ1,self.anfangs_wspQ2]
    
    if hasattr(self,'hz_df'):
        self.lw_dictL['min WSP'][0:2]         = [self.wspminQ1,self.wspminQ2]
        self.lw_dictL['max WSP'][0:2]         = [self.wspmaxQ1,self.wspmaxQ2]
        self.lw_dictL['instationär WSP'][0:2] = [self.wspQ1,self.wspQ2]
            
def plot_wspL(self):

    i    = self.gewid_current
    idxS = (self.df_start['ID'] == i)
    
    pw = self.plotdefaults['w'][3]
    pc = self.plotdefaults['w'][0]
    ps = self.penstyle[self.plotdefaults['w'][2]]
    wc = QColor(0,255,255,150)

    stat = self.df_start['XL'].values[idxS]
    wsp_l = self.df_start['HZERO'].values[idxS]
    
    '''Zeitlich wsp plot at längschnitt'''
    '''anfangs wsp plot at längschnitt'''
    self.anfangs_wspL = self.langView.plot(stat,wsp_l,
                                 pen=pg.mkPen(width=2,color='m',style=2),
                                 fillLevel=self.df_start['ZO'][idxS].min()-self.plotdefaults['lf'][3],
                                 brush =None,name = 'Anfangs WSP')
    self.anfangs_wspL.setZValue(1)
    pitems = [self.lsplot,self.anfangs_wspL]
    self.lw_dictL['Anfangs WSP'][2]     = self.anfangs_wspL
    self.lwopt_dictL['Anfangs WSP']     = self.anfangs_wspL.opts
    
    if hasattr(self,'hz_df'):
        wsp = self.hz_df[self.lzeitslider.value()].values[idxS]
        self.wspL = self.langView.plot(stat,wsp,pen=pg.mkPen(width=pw,color=pc,style=ps),
                                       fillLevel=self.df_start['ZO'][idxS].min()-self.plotdefaults['lf'][3],
                                       brush =wc,name = 'instationär WSP')
        self.wspL.setZValue(0)
    
        '''max plot'''
        wspmax = self.hz_df.transpose().max()[idxS].values
        self.wspmaxL = self.langView.plot(stat,wspmax,pen=pg.mkPen(width=2,color='b',style=2),
                                          name = 'max WSP')
        self.wspmaxL.setZValue(2)
     
        '''min plot'''
        wspmin = self.hz_df.transpose().min()[idxS].values
        self.wspminL = self.langView.plot(stat,wspmin,pen=pg.mkPen(width=2,color='r',style=2),
                                          name = 'min WSP')
        self.wspminL.setZValue(1)
    
        pitems = [*pitems,self.wspL,self.wspmaxL,self.wspminL]
        
        self.lw_dictL['min WSP'][2]         = self.wspminL
        self.lw_dictL['max WSP'][2]         = self.wspmaxL
        self.lw_dictL['instationär WSP'][2] = self.wspL
        self.lwopt_dictL['min WSP']         = self.wspminL.opts
        self.lwopt_dictL['max WSP']         = self.wspmaxL.opts
        self.lwopt_dictL['instationär WSP'] = self.wspL.opts
            
    self.langView.getViewBox().autoRange(items=pitems)
    
'''Plot Discharge Q for the längsschnitt'''
def plot_qL(self):
    
    l,q = self.splitter_3.sizes()
    if not hasattr(self,'qz_df'):
        self.statusbar.showMessage('QCH Datei nicht vorhanden!')
        self.splitter_3.sizes([l+q,0])
        return
    
    self.splitter_3.setSizes([(l+q)*0.8,(l+q)*0.2])
    i    = self.gewid_current
    idxS = (self.df_start['ID'] == i)

    stat = self.df_start['XL'].values[idxS]
    q_l  = self.qz_df[self.lzeitslider.value()].values[idxS]
    
    indexs = self.df_start.index.values[idxS]
    qmin   = self.qz_df.loc[indexs].min().min()
    fl     = np.round(qmin-1,2) if qmin-1>0 else 0
    
    self.q_plot = self.qlangView.plot(stat,q_l,pen=pg.mkPen(width=3,color='k'),
                                      fillLevel=fl,brush=QColor(0,255,255,150),
                                      name='instationär Abfluss')
    self.q_plot.setZValue(1)
    
    self.qlangView.getViewBox().setXLink(self.langView.getViewBox())
    self.qlangView.invertX(True)
    self.qlangView.showGrid(x=True,y=True)
    self.qlangView.showAxis('right')
    self.qlangView.getAxis('left').setLabel('Abfluss Q [m³/s]')
    self.qlangView.getAxis('right').setLabel('Abfluss Q  [m³/s]')
    self.qlangView.getAxis('bottom').setLabel('Station [m]')
    self.statusbar.showMessage('Ready')
    
    self.lw_dictL['instationär Abfluss'][2] = self.q_plot
    self.lwopt_dictL['instationär Abfluss'] = self.q_plot.opts

def l_looper(self):
    if hasattr(self,'hz_df') & self.play_lzeit.isChecked():
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
    if hasattr(self,'hz_df'):
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
        if hasattr(self,'q_plot'):
            q= self.qz_df[spos].values[idxS]
            self.q_plot.setData(stat,q)
        
        '''Querschnitt'''
        wspq1 = self.hz_df.loc[self.Node.name][spos]
        b1    = self.wspQ1.getData()[0]
        wspq2 = self.hz_df.loc[self.Node2.name][spos]
        b2    = self.wspQ2.getData()[0]
        wt1   = self.wspT1.getData()[1][0]
        b3    = self.wspT1.getData()[0]
        wt2   = self.wspT2.getData()[1][0]
        b4    = self.wspT2.getData()[0]
    
        legW1 = 'WSP= '+str(round(wspq1,2))+' m+NN'
        legW2 = 'WSP= '+str(round(wspq2,2))+' m+NN'
        self.wspQ1.setData(b1,[wspq1,wspq1])
        self.wspT1.setData(b3,[wt1,wspq1])
        self.wspTT1.setPos(b3[0],(wt1+wspq1)/2)
        self.wspTT1.setText('%.2f m' %(wspq1-wt1))
        self.wspT2.setData(b4,[wt2,wspq2])
        self.wspTT2.setPos(b4[0],(wt2+wspq2)/2)
        self.wspTT2.setText('%.2f m' %(wspq2-wt2))
        self.graphicsView.plotItem.legend.removeItem(self.wspQ1)
        self.graphicsView.plotItem.legend.addItem(self.wspQ1,legW1)
        self.wspQ2.setData(b2,[wspq2,wspq2])
        self.graphicsView2.plotItem.legend.removeItem(self.wspQ2)
        self.graphicsView2.plotItem.legend.addItem(self.wspQ2,legW2)
        
        #annotation
        Text1 = textgen(self.Node,self)
        Text2 = textgen(self.Node2,self)
        self.qplotD['annotate'][0].setText(Text1)
        self.qplotD['annotate'][1].setText(Text2)
        
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
        if not i.text() =='instationär Abfluss':
            for p in pitem: p.hide()
            self.graphicsView.plotItem.legend.removeItem(pitem[0])
            self.graphicsView2.plotItem.legend.removeItem(pitem[1])
            self.langView.plotItem.legend.removeItem(pitem[2])
        else: pitem[2].hide() 
    elif i.checkState() == 2:
        if not i.text() =='instationär Abfluss':
            for p in pitem: p.show()
            self.graphicsView.plotItem.legend.addItem(pitem[0],pitem[0].name())
            self.graphicsView2.plotItem.legend.addItem(pitem[1],pitem[1].name())
            self.langView.plotItem.legend.addItem(pitem[2],pitem[2].name())
        else: pitem[2].show() 
        
def langPlotBeautify(self,i):
    pitems = self.lw_dictL[i.text()]
    
    pitems = [pitems[2]] if i.text() == 'instationär Abfluss' else pitems
    ask = changePropsL(self,i.text())
    if ask is not None:
        pen,symbol,symbolPen,symbolBrush,symbolSize,fillLevel,fillBrush = ask
        for n,p in enumerate(pitems):
            p.opts['pen']         = pen
            p.opts['symbol']      = symbol
            p.opts['symbolPen']   = symbolPen
            p.opts['symbolBrush'] = symbolBrush
            p.opts['symbolSize']  = symbolSize
            if fillLevel:
                if len(pitems)>1:
                    p.opts['fillLevel']   = self.lw_dictL['sohle'][n].getData()[1].min()
                else:
                    gc     = self.gewid_current
                    indexs = self.df_start[self.df_start['ID'] == gc].index.values
                    qmin   = self.qz_df.loc[indexs].min().min()
                    p.opts['fillLevel']  = np.round(qmin-1,2) if qmin-1>0 else 0
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
    
'''Update the Main Cross-section View here'''
def uqplots(self,plist=[0,1],ROI=False):
    for n in plist:
        Node  = self.qplotD['node'][n]
#        rNode = self.qplotD['rnode'][n]
#        sNode = self.qplotD['snode'][n]
        View  = self.qplotD['view'][n]
        
        if Node.Npoints <1:
            self.statusbar.showMessage('Knoten '+str(self.loc)+': X&Y nicht vorhanden!')
            return
        riv_bed_y,riv_bed_idx,riv_bed_x = riv_bed(Node)
        self.qplotD['plotbot'][n]       = riv_bed_y-self.plotdefaults['qf'][3]
        
        self.qplotD['axis'][n].setData(Node['X'],Node['Y'],fillLevel=self.qplotD['plotbot'][n])

        #plot lamellen
        kplot(self,n)
        
        if ROI: updateROI(self)

        '''update text annotations Info'''
        Text = textgen(Node,self)
        if self.qplotD['annotate'][n] is not None:
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
        if hasattr(self,'df_start'):
            if self.df_start is not None:
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

                wspA   = self.df_start.loc[Node.name]['HZERO']
                leg = 'Anfangs WSP= '+str(round(wspA,2))+' m+NN'
        if hasattr(self,'hz_df'):
            wsp    = self.hz_df.loc[Node.name][self.lzeitslider.value()]
            wspmin = self.hz_df.loc[Node.name].min()
            wspmax = self.hz_df.loc[Node.name].max()
            legW = 'instationär WSP= '+str(round(wsp,2))+' m+NN'
            legMi = 'min WSP= '+str(round(wspmin,2))+' m+NN'
            legMa = 'max WSP= '+str(round(wspmax,2))+' m+NN'
            
        if n == 0:
            if hasattr(self,'df_start'):
                if self.df_start is not None:
                    self.anfangs_wspQ1.setData([wsp_l,wsp_r],[wspA,wspA])
                    View.plotItem.legend.removeItem(self.anfangs_wspQ1)
                    View.plotItem.legend.addItem(self.anfangs_wspQ1,leg)
            if hasattr(self,'hz_df'):
                self.wspQ1.setData([wsp_l,wsp_r],[wsp,wsp],fillLevel = self.qplotD['plotbot'][n])
                self.wspT1.setData([riv_bed_x,riv_bed_x],[riv_bed_y,wsp])
                self.wspTT1.setPos(riv_bed_x,(wsp+riv_bed_y)/2)
                self.wspTT1.setText('%.2f m' %(wsp-riv_bed_y))
                self.wspminQ1.setData([wsp_l,wsp_r],[wspmin,wspmin])
                self.wspmaxQ1.setData([wsp_l,wsp_r],[wspmax,wspmax])
                View.plotItem.legend.removeItem(self.wspQ1)
                View.plotItem.legend.addItem(self.wspQ1,legW)
                View.plotItem.legend.removeItem(self.wspmaxQ1)
                View.plotItem.legend.addItem(self.wspmaxQ1,legMa)
                View.plotItem.legend.removeItem(self.wspminQ1)
                View.plotItem.legend.addItem(self.wspminQ1,legMi)

            
        elif n==1:
            if hasattr(self,'df_start'):
                if self.df_start is not None:
                    self.anfangs_wspQ2.setData([wsp_l,wsp_r],[wspA,wspA])
                    View.plotItem.legend.removeItem(self.anfangs_wspQ2)
                    View.plotItem.legend.addItem(self.anfangs_wspQ2,leg)
            if hasattr(self,'hz_df'):
                self.wspQ2.setData([wsp_l,wsp_r],[wsp,wsp],fillLevel = self.qplotD['plotbot'][n])
                self.wspT2.setData([riv_bed_x,riv_bed_x],[riv_bed_y,wsp])
                self.wspTT2.setPos(riv_bed_x,(wsp+riv_bed_y)/2)
                self.wspTT2.setText('%.2f m' %(wsp-riv_bed_y))
                self.wspminQ2.setData([wsp_l,wsp_r],[wspmin,wspmin])
                self.wspmaxQ2.setData([wsp_l,wsp_r],[wspmax,wspmax])
                View.plotItem.legend.removeItem(self.wspQ2)
                View.plotItem.legend.addItem(self.wspQ2,legW)
                View.plotItem.legend.removeItem(self.wspmaxQ2)
                View.plotItem.legend.addItem(self.wspmaxQ2,legMa)
                View.plotItem.legend.removeItem(self.wspminQ2)
                View.plotItem.legend.addItem(self.wspminQ2,legMi)
    plotted_items = [pit for pit in [*self.qplotD['lambank'][n],self.qplotD['annotate'][n]] if pit is not None]
    if len(plotted_items)>1: View.getViewBox().autoRange(items=plotted_items)

'''Update banks for the Cross-sections'''
def update_banks(self,n):
    Node = self.qplotD['node'][n]
    if not self.Edit:
        Mode = self.qplotD['node'][n].Mode
        MaxHt = self.qplotD['node'][n]['Max Height']
    else:
        Mode  = self.modus_label.currentText()
        MaxHt = self.maxHeight_label.value()
    riv_bed_y,riv_bed_idx,riv_bed_x = riv_bed(Node)
    LOB_x,LOB_y,ROB_x,ROB_y = cal_bank(Node,Mode = Mode, MaxHt = MaxHt)
    top_y = max(LOB_y,ROB_y) +2
    if (riv_bed_x <= LOB_x) or (riv_bed_x >= ROB_x):
        riv_bed_x = (LOB_x+ROB_x)/2.
        
    self.qplotD['rbank'][n][0].setData([LOB_x,LOB_x],[LOB_y,top_y])
    self.qplotD['rbank'][n][0].update()
    self.qplotD['rbank'][n][0].updateItems()
    self.qplotD['rbank'][n][1].setData([ROB_x,ROB_x],[ROB_y,top_y])
    self.qplotD['rbank'][n][1].update()
    self.qplotD['rbank'][n][1].updateItems()
    self.qplotD['rbank'][n][2].setData([LOB_x,ROB_x],[top_y,top_y])
    self.qplotD['rbank'][n][2].update()
    self.qplotD['rbank'][n][2].updateItems()
    self.qplotD['rbank'][n][3].setPos(LOB_x,top_y)
    self.qplotD['rbank'][n][3].update()
    self.qplotD['rbank'][n][4].setPos(ROB_x,top_y)
    self.qplotD['rbank'][n][4].update()
    self.qplotD['rbank'][n][5].setText(Mode)
    self.qplotD['rbank'][n][5].setPos(riv_bed_x,top_y)
    self.qplotD['rbank'][n][5].updateTextPos()
    
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
    
    wsp_l = self.df_start['HZERO'].values[idx]
    self.anfangs_wspL.setData(x,wsp_l)
    pitems = [self.lsplot,self.anfangs_wspL]
    ymax = wsp_l.max()
    
    if hasattr(self,'hz_df'):
        wsp = self.hz_df[self.lzeitslider.value()].values[idx]
        self.wspL.setData(x,wsp,fillLevel=np.nanmin(y)-self.plotdefaults['lf'][3])
        
        wspmin = self.hz_df.transpose().min()[idx].values
        self.wspminL.setData(x,wspmin)
    
        wspmax = self.hz_df.transpose().max()[idx].values
        self.wspmaxL.setData(x,wspmax)
        pitems = [*pitems,self.wspL,self.wspmaxL,self.wspminL]
        ymax = wspmax.max()
        
    if hasattr(self,'q_plot'):
        q = self.qz_df[self.lzeitslider.value()].values[idx]
        indexs = self.df_start.index.values[idx]
        qmin   = self.qz_df.loc[indexs].min().min()
        fl     = np.round(qmin-1,2) if qmin-1>0 else 0
        self.q_plot.setData(x,q,fillLevel=fl)
        self.q_plot.show()
        
    # self.langView.getViewBox().autoRange(items=pitems)
    self.langView.getViewBox().setRange(xRange=(x.min(),x.max()),yRange=(y.min(),ymax))
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

def nodeMarker(self,df):
    if hasattr(self,'pro_mark'):
        if (self.loc in df.index) & (self.loc2 in df.index):
            if self.nodemapview.isChecked():
                xk1     = float(df.loc[self.loc]['X'])
                yk1     = float(df.loc[self.loc]['Y'])
                xk2     = float(df.loc[self.loc2]['X'])
                yk2     = float(df.loc[self.loc2]['Y'])
                
            elif self.nodeplanview.isChecked():
                yk1     = float(self.df_s.loc[self.loc]['XL'])
                id1     = float(self.df_s.loc[self.loc]['ID'])
                yk2     = float(self.df_s.loc[self.loc2]['XL'])
                id2     = float(self.df_s.loc[self.loc2]['ID'])
                
                xk1     = (list(df['ID'].unique()).index(id1)+1)*100
                xk2     = (list(df['ID'].unique()).index(id2)+1)*100
            
            self.pro_mark.setData([xk1,xk2],[yk1,yk2])
            self.nodeitem.setText(str(self.loc))
            self.nodeitem.setPos(xk1,yk1)
            self.nodeitem2.setText(str(self.loc2))
            self.nodeitem2.setPos(xk2,yk2)
            

def changeAR(self):
    if self.arbox.isChecked():
        self.qplotD['axis'][0].getViewBox().setAspectLocked(lock=True, ratio=1.0/self.AspectRatio.value())
        self.qplotD['axis'][1].getViewBox().setAspectLocked(lock=True, ratio=1.0/self.AspectRatio.value())
    else:
        self.qplotD['axis'][0].getViewBox().setAspectLocked(None)
        self.qplotD['axis'][1].getViewBox().setAspectLocked(None)

def plan_name(self):
    if hasattr(self,'p_plan'):
        if self.p_plan.text() != '':
            self.graphicsView.setTitle('Knoten Nr.: '+str(self.Node.name)+', Szenario: ' +self.p_plan.text())
            self.graphicsView2.setTitle('Knoten Nr.: '+str(self.Node2.name)+', Szenario: ' +self.p_plan.text())
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