# -*- coding: utf-8 -*-
"""
Created on Wed Jun 26 13:32:34 2019

@author: s.Shaji
"""
import os
import pickle
import numpy             as     np
import pyqtgraph         as     pg
import pandas            as     pd
import shapefile         as     shp
from modules.rawh1d      import writePRO,writeStart,writeHYD,writeRUN
from modules.loadhyd     import updateHyd
from modules.loaddata    import loadshpfile,loadras
from PyQt5.QtWidgets     import QFileDialog,QMessageBox,QListWidgetItem
from PyQt5.QtGui         import QColor

def saveProject(self):
    if hasattr(self,'h1d'):
        proj = {}
        proj['Plan']        = self.p_plan.text()
        proj['Runpath']     = self.h1drunpath
        proj['DateTime']    = self.sim_datetime.dateTime().toString()
        proj['nodeview']    = self.buttonGroup.checkedButton().objectName()
        proj['nodemethod']  = self.buttonGroup_2.checkedButton().objectName()
        proj['listOfItems'] = [(self.lp_listWidget.item(i).text(),self.lp_listWidget.item(i).checkState()) for i in range(self.lp_listWidget.count())]
        proj['lqstate']     = [(self.ls_listWidget.item(i).text(),self.ls_listWidget.item(i).checkState()) for i in range(self.ls_listWidget.count())]
        opt_dict = {}
        for k in self.lwopt_dict.keys():
            opt_dict[k] = {}
            if self.lwopt_dict[k]['pen'] is not None:
                if type(self.lwopt_dict[k]['pen']) in [str,tuple]:
                    opt_dict[k]['pen'] = ((self.lwopt_dict[k]['pen']),1,1)
                else:
                    opt_dict[k]['pen'] = (self.lwopt_dict[k]['pen'].color().getRgb(),
                                       self.lwopt_dict[k]['pen'].width(),
                                       self.lwopt_dict[k]['pen'].style())
            else: opt_dict[k]['pen'] = None
            
            if self.lwopt_dict[k]['symbol'] is not None:
                opt_dict[k]['symbol'] = self.lwopt_dict[k]['symbol']
            else: opt_dict[k]['symbol'] = None

            if self.lwopt_dict[k]['symbolPen'] is not None:
                if type(self.lwopt_dict[k]['symbolPen']) in [str,tuple]:
                    opt_dict[k]['symbolPen'] = self.lwopt_dict[k]['symbolPen']
                else:
                    opt_dict[k]['symbolPen'] = self.lwopt_dict[k]['symbolPen'].getRgb()
            else: opt_dict[k]['symbolPen'] = None
            
            if self.lwopt_dict[k]['symbolBrush'] is not None:
                if type(self.lwopt_dict[k]['symbolBrush']) in [str,tuple]:
                    opt_dict[k]['symbolBrush'] = self.lwopt_dict[k]['symbolBrush']
                else:
                    opt_dict[k]['symbolBrush'] = self.lwopt_dict[k]['symbolBrush'].getRgb()
            else: opt_dict[k]['symbolBrush'] = None
            
            if self.lwopt_dict[k]['symbolSize'] is not None:
                opt_dict[k]['symbolSize'] = self.lwopt_dict[k]['symbolSize']
            else: opt_dict[k]['symbolSize'] = None
            
            if self.lwopt_dict[k]['fillLevel'] is not None:
                opt_dict[k]['fillLevel'] = self.lwopt_dict[k]['fillLevel']
            else: opt_dict[k]['fillLevel'] = None

            if self.lwopt_dict[k]['fillBrush'] is not None:
                if type(self.lwopt_dict[k]['fillBrush']) in [str,tuple]:
                    opt_dict[k]['fillBrush'] = self.lwopt_dict[k]['fillBrush']
                else:
                    opt_dict[k]['fillBrush'] = self.lwopt_dict[k]['fillBrush'].getRgb()
            else: opt_dict[k]['fillBrush'] = None
        proj['nodeoptdict'] = opt_dict
        proj['addedData']   = self.loadshp_dict
        suggestfile = self.projektname.text()
        
        lqopt_dict = {}
        for k in self.lwopt_dictL.keys():
            lqopt_dict[k] = {}
            p = self.lwopt_dictL[k]
            if p['pen'] is not None:
                if type(p['pen']) in [str,tuple]:
                    lqopt_dict[k]['pen'] = ((p['pen']),1,1)
                else:
                    lqopt_dict[k]['pen'] = (p['pen'].color().getRgb(),
                                       p['pen'].width(),
                                       p['pen'].style())
            else: lqopt_dict[k]['pen'] = None
            
            if p['symbol'] is not None:
                lqopt_dict[k]['symbol'] = p['symbol']
            else: lqopt_dict[k]['symbol'] = None

            if p['symbolPen'] is not None:
                if type(p['symbolPen']) in [str,tuple]:
                    lqopt_dict[k]['symbolPen'] = p['symbolPen']
                else:
                    lqopt_dict[k]['symbolPen'] = p['symbolPen'].getRgb()
            else: lqopt_dict[k]['symbolPen'] = None
            
            if p['symbolBrush'] is not None:
                if type(p['symbolBrush']) in [str,tuple]:
                    lqopt_dict[k]['symbolBrush'] = p['symbolBrush']
                else:
                    lqopt_dict[k]['symbolBrush'] = p['symbolBrush'].getRgb()
            else: lqopt_dict[k]['symbolBrush'] = None
            
            if p['symbolSize'] is not None:
                lqopt_dict[k]['symbolSize'] = p['symbolSize']
            else: lqopt_dict[k]['symbolSize'] = None
            
            if p['fillLevel'] is not None:
                lqopt_dict[k]['fillLevel'] = p['fillLevel']
            else: lqopt_dict[k]['fillLevel'] = None

            if p['fillBrush'] is not None:
                if type(p['fillBrush']) in [str,tuple]:
                    lqopt_dict[k]['fillBrush'] = p['fillBrush']
                else:
                    lqopt_dict[k]['fillBrush'] = p['fillBrush'].getRgb()
            else: lqopt_dict[k]['fillBrush'] = None
        proj['lqoptdict'] = lqopt_dict
        
        file,ext = QFileDialog.getSaveFileName(caption='Save Project',directory = suggestfile,filter='Hydret Projekt *.hgui')
        if not file == '':
            self.statusbar.showMessage('Saving PROJEKT...')
            with open(file,'wb') as outfile:
                pickle.dump(proj,outfile)
            self.statusbar.showMessage('Ready')

def savePro(self):
    if hasattr(self,'propath'):
        if os.path.isfile(self.propath):
            pp = self.propath if len(self.propath)<15 else '...'+self.propath[-15:]
            text = pp+ ' existiert! Überschreiben?'
            q = QMessageBox.question(self,'Save Pro-Datei',text,
                                     QMessageBox.Yes|QMessageBox.No|QMessageBox.Cancel)
            
            if q == QMessageBox.Yes:
                self.statusbar.showMessage('Saving PRO...')
                writePRO(self.propath,self.df_pro)
                self.statusbar.showMessage('Ready')
                
            elif  q == QMessageBox.No:
                path,ext = QFileDialog.getSaveFileName(caption='Save Pro-Datei',
                                                   filter='*.pro')
                if path != '':
                    self.statusbar.showMessage('Saving PRO...\nPath: '+path)
                    writePRO(path,self.df_pro)
                    self.statusbar.showMessage('Ready')
            else:return

            
def saveHyd(self):
    if hasattr(self,'h1d'):
        updateHyd(self)
        if os.path.isfile(self.h1d.hyd_p):
            text = self.h1d.hyd_f+ ' existiert! Überschreiben?'
            q = QMessageBox.question(self,'Save Hyd',text,QMessageBox.Yes,QMessageBox.No)
            
            if q == QMessageBox.Yes:
                self.statusbar.showMessage('Saving HYD...')
                writeHYD(self.h1d.hyd_p,self.h1d)
                self.statusbar.showMessage('Ready')
        else:
            self.statusbar.showMessage('Saving HYD...')
            writeHYD(self.h1d.hyd_p,self.h1d)
            self.statusbar.showMessage('Ready')

def saveStart(self):
    if hasattr(self,'h1d'):
        if os.path.isfile(self.h1d.startpath):
            text = self.h1d.startdat+ ' existiert! Überschreiben?'
            q = QMessageBox.question(self,'Save Start',text,QMessageBox.Yes,QMessageBox.No)
            
            if q == QMessageBox.Yes:
                self.statusbar.showMessage('Saving START...')
                writeStart(self.h1d.startpath,self.df_start,self.h1d._dform)
                self.statusbar.showMessage('Ready')
        else:
            self.statusbar.showMessage('Saving START...')
            writeStart(self.h1d.startpath,self.df_start,self.h1d._dform)
            self.statusbar.showMessage('Ready')

def saveRun(self):
    if hasattr(self,'h1d'):
        if os.path.isfile(self.h1drun):
            text = self.h1drun+ ' existiert! Überschreiben?'
            q = QMessageBox.question(self,'Save Run',text,QMessageBox.Yes,QMessageBox.No)
            
            if q == QMessageBox.Yes:
                self.statusbar.showMessage('Saving RUN...')
                writeRUN(self.h1drun,self.h1d)
                self.statusbar.showMessage('Ready')
        else:
            self.statusbar.showMessage('Saving RUN...')
            writeRUN(self.h1drun,self.h1d)
            self.statusbar.showMessage('Ready')

def bwToExcel(self):
    if not hasattr(self,'h1d'): 
        self.statusbar.showMessage('Hyd Datei nicht vorhanden')
        return
    
    outexcel,ext = QFileDialog.getSaveFileName(caption='BW und Zufluesse Exportieren',filter='*.xlsx')
    
    if outexcel =='': return
    bw = {}
    #zufluesse
    for nn,n in enumerate(self.h1d.nxj):
        ID    = int(self.df_start.loc[n[0]].ID)
        label = str(int(self.df_start.loc[n[2]].ID))
        x  = self.h1d.dxl[nn]
        y  = self.df_start.loc[n[0]].HZERO if not hasattr(self,'hz_df') else round(self.hz_df.loc[n[0]].max(),2)
        q  = self.df_start.loc[n[0]].QZERO if not hasattr(self,'qz_df') else round(self.qz_df.loc[n[0]].max(),2)
        if not ID in bw.keys(): bw[ID] = {'Zufluesse':[],
                                          'Wehre': [],
                                          'Gates': [],
                                          'Lateral flows': []}
        bw[ID]['Zufluesse'] += [[x,y,q,'Zufluss GEW ID: '+label]]
    
    #Weirs
    for w in self.h1d.weir_info:
        k   = int(w.split()[0])
        mue = abs(float(w.split()[3]))
        ID  = int(self.df_start.loc[k].ID)
        x   = self.df_start.loc[k].XL
        y  = self.df_start.loc[k].HZERO if not hasattr(self,'hz_df') else round(self.hz_df.loc[k].max(),2)
        q  = self.df_start.loc[k].QZERO if not hasattr(self,'qz_df') else round(self.qz_df.loc[k].max(),2)
        if not ID in bw.keys(): bw[ID] = {'Zufluesse':[],
                                          'Wehre': [],
                                          'Gates': [],
                                          'Lateral flows': []}
        bw[ID]['Wehre'] += [[x,y,q,'Wehr K'+str(k)+', μ='+str(mue)]]
    
    #Gates
    for n,g in enumerate(self.h1d.igate):
        ID = int(self.df_start.loc[g].ID)
        x  = self.df_start.loc[g].XL
        y  = round(self.df_start.loc[k].ZO+self.h1d.iaga[n],2)
        q  = self.df_start.loc[k].QZERO if not hasattr(self,'qz_df') else round(self.qz_df.loc[k].max(),2)
        if not ID in bw.keys(): bw[ID] = {'Zufluesse':[],
                                          'Wehre': [],
                                          'Gates': [],
                                          'Lateral flows': []}
        bw[ID]['Gates'] += [[x,y,q,'Schuetz K'+str(g)+', μ='+self.h1d.gatdat[n][4]]]
    
    #lateral inflows/outflows
    for l in self.h1d.lie:
        ID = int(self.df_start.loc[l].ID)
        x  = self.df_start.loc[l].XL
        y  = self.df_start.loc[l].HZERO if not hasattr(self,'hz_df') else round(self.hz_df.loc[l].max(),2)
        q  = self.df_start.loc[l].QZERO if not hasattr(self,'qz_df') else round(self.qz_df.loc[l].max(),2)
        if not ID in bw.keys(): bw[ID] = {'Zufluesse':[],
                                          'Wehre': [],
                                          'Gates': [],
                                          'Lateral flows': []}
        bw[ID]['Lateral flows'] += [[x,y,q,'seitlicher Zufluss K'+str(l)]]         
    
    with pd.ExcelWriter(outexcel) as writer: 
        for k in bw.keys():
            df = [pd.DataFrame(bw[k][t],columns=['x','y','q',t]) for t in bw[k].keys()]
            pd.concat(df,axis=1).to_excel(writer,sheet_name=str(k),index=False)
            
def shp2pro(self):
    s,ext = QFileDialog.getOpenFileName(caption='Shapefile im Pro Datei',filter='*.shp')
    
    r = shp.Reader(s)
    if [f[0] for f in r.fields[1:]] != ['KNO', 'MOD', 'CTAB', 'MAXHT', 'STAT', 'PNAM']:
        r.close()
        self.statusbar.showMessage('Header im dbf. sollte KNO, MOD, CTAB, MAXHT, STAT, PNAM sein')
        return
    
    df_pro = pd.DataFrame(index=np.arange(0,r.numRecords),
                                          columns = ('Node','Npoints','Mode','CTAB',
                                                     'Max Height','Station',
                                                     'PName','X','Y'))
    for n,ri in enumerate(r.records()):
        df_pro.iloc[n].Node          = ri['KNO']
        df_pro.iloc[n].Mode          = ri['MOD']
        df_pro.iloc[n].CTAB          = ri['CTAB']
        df_pro.iloc[n]['Max Height'] = ri['MAXHT']
        df_pro.iloc[n].Station       = ri['STAT']
        df_pro.iloc[n].PName         = ri['PNAM']
        df_pro.iloc[n].X             = np.array(r.shapeRecord(n).shape.points)[:,0]
        df_pro.iloc[n].Y             = np.array(r.shapeRecord(n).shape.z)
        df_pro.iloc[n].Npoints       = len(df_pro.iloc[n].X)
    r.close()
    df_pro.set_index(keys = 'Node',drop=True, inplace= True)
    
    pro,ext = QFileDialog.getSaveFileName(caption='Shape zum Pro',filter='*.pro')
    if not pro == '':  writePRO(pro,df_pro)

def pro2shp(self):
    if not hasattr(self,'df_pro'):
        self.statusbar.showMessage('Pro Datei nicht vorhanden')
        return
    pro,ext = QFileDialog.getSaveFileName(caption='Pro Datein im Shape',filter='*.shp')
    if pro=='': return
    w = shp.Writer(pro)
    w.fields = [('KNO','N',5,0),
                ('MOD','C',2,0),
                ('CTAB','N',1,0),
                ('MAXHT','N',5,1),
                ('STAT','N',10,1),
                ('PNAM','C',30,0)]
    for k in range(len(self.df_pro)):
        df = self.df_pro.iloc[k]
        try:    stat=float(df['Station'])
        except: stat=0.0
        ctab =  0 if df['CTAB'] == '   ' else int(df['CTAB'])
        mxht = float(df['Max Height'])
        line = list(zip(df.X.tolist(),[stat]*len(df.X),df.Y.tolist()))
        w.linez([line])
        w.record(int(df.name),df['Mode'],ctab,mxht,stat,df['PName'])
    w.close()
# =============================================================================
#             Load JSON Data here                                             
# =============================================================================
def loadgui(self,proj):
    if self.nodemapview.isChecked():
        self.lp_listWidget.blockSignals(True)
        
        for n,(t,state) in enumerate(proj['listOfItems']):
            if t in proj['addedData'].keys():
                if proj['addedData'][t][0].lower().endswith('.shp'):
                    loadshpfile(self,proj['addedData'][t][0])
                    if proj['nodeoptdict'][t]['pen'] is not None: 
                        color = QColor(*proj['nodeoptdict'][t]['pen'][0])
                        width = proj['nodeoptdict'][t]['pen'][1]
                        style = proj['nodeoptdict'][t]['pen'][2]
                        proj['nodeoptdict'][t]['pen'] = pg.mkPen(color=color,
                            width=width,style=style)
                    dic = self.lw_dict[t][0].opts
                    for nk in proj['nodeoptdict'][t].keys():
                        dic[nk] = proj['nodeoptdict'][t][nk]
                    for o in self.lw_dict[t]:
                        o.opts = dic
                        self.lwopt_dict[t] = dic
                        o.update()
                        o.updateItems()
                elif proj['addedData'][t][0].lower().endswith('.dbf'):
                    loadras(self,proj['addedData'][t][0],props=proj['addedData'][t][1])
                [z.setZValue((n-1)*-1) for z in self.lw_dict[t]]
                
            elif t in self.lw_dict.keys():
                [p.setZValue((n-1)*-1) for p in self.lw_dict[t]]
                if proj['nodeoptdict'][t]['pen'] is not None: 
                    color = QColor(*proj['nodeoptdict'][t]['pen'][0])
                    width = proj['nodeoptdict'][t]['pen'][1]
                    style = proj['nodeoptdict'][t]['pen'][2]
                    proj['nodeoptdict'][t]['pen'] = pg.mkPen(color=color,
                        width=width,style=style)
                dic = self.lw_dict[t][0].opts
                for nk in proj['nodeoptdict'][t].keys():
                    dic[nk] = proj['nodeoptdict'][t][nk]
                for o in self.lw_dict[t]:
                    o.opts = dic
                    self.lwopt_dict[t] = dic
                    o.update()
                    o.updateItems()
                        
        self.lp_listWidget.clear()
        for t,state in proj['listOfItems']:
            item = QListWidgetItem(t)
            item.setCheckState(state)
            if state == 0:
                [p.hide() for p in self.lw_dict[t]]
            self.lp_listWidget.addItem(item)
        self.lp_listWidget.blockSignals(False)
   
    if 'lqoptdict' in proj.keys():
        self.ls_listWidget.blockSignals(True)
        
        for t in proj['lqoptdict'].keys():
            if t in self.lw_dictL.keys():
                if proj['lqoptdict'][t]['pen'] is not None: 
                    color = QColor(*proj['lqoptdict'][t]['pen'][0])
                    width = proj['lqoptdict'][t]['pen'][1]
                    style = proj['lqoptdict'][t]['pen'][2]
                    proj['lqoptdict'][t]['pen'] = pg.mkPen(color=color,
                        width=width,style=style)
                dic = self.lw_dictL[t][0].opts
                for nk in proj['lqoptdict'][t].keys():
                    dic[nk] = proj['lqoptdict'][t][nk]
                for o in self.lw_dictL[t]:
                    o.opts = dic
                    self.lwopt_dictL[t] = dic
                    o.update()
                    o.updateItems()

        for t,state in proj['lqstate']:
            for i in range(self.ls_listWidget.count()):
                if self.ls_listWidget.item(i).text() == t:
                    self.ls_listWidget.item(i).setCheckState(state)
                    if state == 0: 
                        for p in self.lw_dictL[t]: p.hide()
                    else: 
                        for p in self.lw_dictL[t]: p.show()
                    
        self.ls_listWidget.blockSignals(False)