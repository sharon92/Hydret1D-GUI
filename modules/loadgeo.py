# -*- coding: utf-8 -*-
"""
Created on Fri May 31 11:28:40 2019

@author: s.Shaji
"""
import numpy          as     np
from modules.riverbed import riv_bed
from PyQt5.QtWidgets  import QTableWidgetItem,QAbstractItemView
from PyQt5.QtGui      import QFont,QColor

# =============================================================================
# load Querschnitt Display info
# =============================================================================
def load_qinfo(self,df,i):
    
    #enabling radio buttons
    for n,loc in enumerate([self.loc,self.loc2]):
        if -1*loc in df.index:
            _iloc = np.argwhere(df.index==-1*loc)[0]
            for il in _iloc:
                if df.iloc[il].Mode == 'H2':
                    self.qplotD['riloc'][n] = il
                    self.qplotD['rnode'][n] = df.iloc[il]
            for il in _iloc:
                if df.iloc[il].Mode == 'ZS':
                    self.iloc_s = il
                    self.qplotD['siloc'][n] = il
                    self.qplotD['snode'][n] = df.iloc[il]
        else:
            for key_ in ['rnode','riloc','snode','siloc']:
                self.qplotD[key_][n] = None
    
    if not self.qplotD['riloc'][0] is None:
        self._rrau.setEnabled(True)
    else:
        self._rrau.setEnabled(False)
        
    if not self.qplotD['siloc'][0] is None:
        self._rschalter.setEnabled(True)
    else:
        self._rschalter.setEnabled(False)
    
    if self._rrau.isChecked():
        if not self._rrau.isEnabled():
            self._rquer.setChecked(True)
    if self._rschalter.isChecked():
        if not self._rschalter.isEnabled():
            self._rquer.setChecked(True)
        
    #lcd
    self.Punkte_label.display(self.Node['Npoints'])

    try:
        self.modus_label.setText(self.Node['Mode'])
    except:
        self.modus_label.setText(self.h1d.xsecmo)
        self.Node['Mode'] = self.h1d.xsecmo
    
    if self.Node['CTAB'] == 1:
        self.ctab_label.setText('ON')
    
    elif self.Node['CTAB'] == 0:
        self.ctab_label.setText('OFF')
    
    self.maxHeight_label.setText(str(self.Node['Max Height']))
    self.station_label.setCurrentIndex(i)
    self.schnittName_label.setCurrentIndex(i)

    #set coords from .pro
    self.coords_table.blockSignals(True)
    
    try:
        self.coords_table.clear()
    except: pass
    
    #radio buttons check
    if self._rquer.isChecked():
        self.coords_table.setRowCount(self.Node['Npoints'])
        self.coords_table.setHorizontalHeaderLabels(("X","Y"))
        for table_i in range(self.Node['Npoints']):
            self.coords_table.setItem(table_i,0,QTableWidgetItem(str(self.Node['X'][table_i])))
            self.coords_table.setItem(table_i,1,QTableWidgetItem(str(self.Node['Y'][table_i])))
        #rivbed color
        '''River Bed'''
        riv_bed_y,riv_bed_idx,riv_bed_x = riv_bed(self.Node)
        
        rfont = QFont()
        rfont.setBold(True)
        self.coords_table.item(riv_bed_idx,0).setForeground(QColor(255,99,71))
        self.coords_table.item(riv_bed_idx,1).setForeground(QColor(255,99,71))
        self.coords_table.item(riv_bed_idx,0).setFont(rfont)
        self.coords_table.item(riv_bed_idx,1).setFont(rfont)
        self.coords_table.scrollToItem(self.coords_table.item(riv_bed_idx,1),QAbstractItemView.PositionAtCenter)
        self.coords_table.selectRow(riv_bed_idx)
    
    elif self._rrau.isChecked():
        self.Node_R = self.qplotD['rnode'][0]
        self.coords_table.setRowCount(self.Node_R['Npoints'])
        self.coords_table.setHorizontalHeaderLabels(("X","Strickler"))
        for table_i in range(self.Node_R['Npoints']):
            self.coords_table.setItem(table_i,0,QTableWidgetItem(str(self.Node_R['X'][table_i])))
            self.coords_table.setItem(table_i,1,QTableWidgetItem(str(self.Node_R['Y'][table_i])))

    elif self._rschalter.isChecked():
        self.Node_S = self.qplotD['snode'][0]
        self.coords_table.setRowCount(self.Node_S['Npoints'])
        self.coords_table.setHorizontalHeaderLabels(("X","Y"))
        for table_i in range(self.Node_S['Npoints']):
            self.coords_table.setItem(table_i,0,QTableWidgetItem(str(self.Node_S['X'][table_i])))
            self.coords_table.setItem(table_i,1,QTableWidgetItem(str(self.Node_S['Y'][table_i])))
    
    self.coords_table.blockSignals(False)
    
# =============================================================================
# load Querschnitt Display info in Editing Mode
# =============================================================================
def load_qinfo2(self,df,i):
    
    #enabling radio buttons
    for n,loc in enumerate([self.loc,self.loc2]):
        if -1*loc in df.index:
            _iloc = np.argwhere(df.index==-1*loc)[0]
            for il in _iloc:
                if df.iloc[il].Mode == 'H2':
                    self.qplotD['riloc'][n] = il
                    self.qplotD['rnode'][n] = df.iloc[il]
            for il in _iloc:
                if df.iloc[il].Mode == 'ZS':
                    self.iloc_s = il
                    self.qplotD['siloc'][n] = il
                    self.qplotD['snode'][n] = df.iloc[il]
        else:
            for key_ in ['rnode','riloc','snode','siloc']:
                self.qplotD[key_][n] = None
    
    if not self.qplotD['riloc'][0] is None:
        self._rrau.setEnabled(True)
    else:
        self._rrau.setEnabled(False)
        
    if not self.qplotD['siloc'][0] is None:
        self._rschalter.setEnabled(True)
    else:
        self._rschalter.setEnabled(False)
    
    if self._rrau.isChecked():
        if not self._rrau.isEnabled():
            self._rquer.setChecked(True)
    if self._rschalter.isChecked():
        if not self._rschalter.isEnabled():
            self._rquer.setChecked(True)
        
    #lcd
    self.Punkte_label.display(self.Node['Npoints'])

    try:
        self.modus_label.setCurrentIndex(self.qschnp.index(self.Node['Mode']))
    except:
        self.modus_label.setCurrentIndex(self.qschnp.index(self.h1d.xsecmo))
        self.Node['Mode'] = self.h1d.xsecmo
    
    if self.Node['CTAB'] == 1:
        self.ctab_label.setCurrentIndex(1)
    
    elif self.Node['CTAB'] == 0:
        self.ctab_label.setCurrentIndex(0)
    
    self.station_label.setCurrentIndex(i)
    self.schnittName_label.setCurrentIndex(i)

    #set coords from .pro
    self.coords_table.blockSignals(True)
    
    try:
        self.coords_table.clear()
    except: pass
    
    #radio buttons check
    if self._rquer.isChecked():
        self.coords_table.setRowCount(self.Node['Npoints'])
        self.coords_table.setHorizontalHeaderLabels(("X","Y"))
        for table_i in range(self.Node['Npoints']):
            self.coords_table.setItem(table_i,0,QTableWidgetItem(str(self.Node['X'][table_i])))
            self.coords_table.setItem(table_i,1,QTableWidgetItem(str(self.Node['Y'][table_i])))
        #rivbed color
        '''River Bed'''
        if self.Node['Npoints'] >0:
            riv_bed_y,riv_bed_idx,riv_bed_x = riv_bed(self.Node)
        
            rfont = QFont()
            rfont.setBold(True)
            self.coords_table.item(riv_bed_idx,0).setForeground(QColor(255,99,71))
            self.coords_table.item(riv_bed_idx,1).setForeground(QColor(255,99,71))
            self.coords_table.item(riv_bed_idx,0).setFont(rfont)
            self.coords_table.item(riv_bed_idx,1).setFont(rfont)
            self.coords_table.scrollToItem(self.coords_table.item(riv_bed_idx,1),QAbstractItemView.PositionAtCenter)
            self.coords_table.selectRow(riv_bed_idx)
    
    elif self._rrau.isChecked():
        self.Node_R = self.qplotD['rnode'][0]
        self.coords_table.setRowCount(self.Node_R['Npoints'])
        self.coords_table.setHorizontalHeaderLabels(("X","Strickler"))
        for table_i in range(self.Node_R['Npoints']):
            self.coords_table.setItem(table_i,0,QTableWidgetItem(str(self.Node_R['X'][table_i])))
            self.coords_table.setItem(table_i,1,QTableWidgetItem(str(self.Node_R['Y'][table_i])))

    elif self._rschalter.isChecked():
        self.Node_S = self.qplotD['snode'][0]
        self.coords_table.setRowCount(self.Node_S['Npoints'])
        self.coords_table.setHorizontalHeaderLabels(("X","Y"))
        for table_i in range(self.Node_S['Npoints']):
            self.coords_table.setItem(table_i,0,QTableWidgetItem(str(self.Node_S['X'][table_i])))
            self.coords_table.setItem(table_i,1,QTableWidgetItem(str(self.Node_S['Y'][table_i])))
    
    self.coords_table.blockSignals(False)
    
# =============================================================================
# load geometrie info
# =============================================================================
def load_start(self):
    
    try:
        self.Node_S = self.df_s.loc[self.loc]
        try:
            self.gi_ityp.setCurrentIndex(self.qschnt.index(int(self.Node_S['ITYPE'])))
        except:
            self.qschnt.append(int(self.Node_S['ITYPE']))
            self.gi_ityp.addItem(str(int(self.Node_S['ITYPE'])))
            self.gi_ityp.setCurrentIndex(self.qschnt.index(int(self.Node_S['ITYPE'])))
    except:
        pass
    
    try:
        self.gi_id.setText(str(int(self.Node_S['ID'])))
    except:
         self.gi_id.setText('n/a')
         
    try:
        self.gi_width.setText(str(self.Node_S['WIDTH']))
    except:
        self.gi_width.setText('n/a')
    
    try:
        self.gi_heit.setText(str(self.Node_S['HEIT']))
    except:
        self.gi_heit.setText('n/a')
        
    try:
        self.gi_zo.setText(str(self.Node_S['ZO']))
    except:
        self.gi_zo.setText('n/a')
    
    try:
        self.gi_rni.setText(str(self.Node_S['RNI']))
    except:
        self.gi_rni.setText('n/a')
    
    try:
        self.gi_dzero.setText(str(self.Node_S['DZERO']))
    except:
         self.gi_dzero.setText('n/a')
    
    try:
        self.gi_qzero.setText(str(self.Node_S['QZERO']))
    except:
        self.gi_qzero.setText('n/a')
        
    try:
        self.gi_zs.setText(str(self.Node_S['ZS']))
    except:
        self.gi_zs.setText('n/a')
    
    try:
        self.gi_sf.setText(str(self.Node_S['SF']))
    except:
        self.gi_sf.setText('n/a')
        
    try:
        self.gi_xl.setText(str(self.Node_S['XL']))
    except:
        self.gi_xl.setText('n/a')
        
    try:
        self.gi_ztr.setText(str(self.Node_S['ZTR']))
    except:
         self.gi_ztr.setText('n/a')
    
    try:
        self.gi_ztl.setText(str(self.Node_S['ZTL']))
    except:
        self.gi_ztl.setText('n/a')
        
    try:
        self.wsp_ckm.setText(str(self.Node_S['CKS']))
    except:
        self.wsp_ckm.setText('n/a')
        
    try:
        self.wsp_hr0.setText(str(self.Node_S['HR0']))
    except:
        self.wsp_hr0.setText('n/a')
        
    for wsp_t in range(7):
        try:
            self.wsp_table.setItem(wsp_t,0,QTableWidgetItem(str(self.Node_S['HR'+str(wsp_t+1)])))
        except:
            self.wsp_table.setItem(wsp_t,0,QTableWidgetItem('n/a'))
        try:
            self.wsp_table.setItem(wsp_t,1,QTableWidgetItem(str(self.Node_S['RNV'+str(wsp_t+1)])))
        except:
            self.wsp_table.setItem(wsp_t,1,QTableWidgetItem('n/a'))
    
    try:
        self.pro_rw.setText(str(self.Node_S['X']))
    except:
        self.pro_rw.setText('n/a')
        
    try:
        self.pro_hw.setText(str(self.Node_S['Y']))
    except:
        self.pro_hw.setText('n/a')

    try:
        self.gi_kstime.setText(str(self.Node_S['KSTIME']))
    except:
        pass
    
    try:
        self.gi_lire.setCurrentIndex(int(self.Node_S['LIRE']))
    except:
        pass
    
    try:
        self.gi_qomax.setText(str(self.Node_S['QOMAX']))
    except:
        pass

    try:
        self.gi_qregel.setText(str(self.Node_S['QREGEL']))
    except:
        pass
    
    try:
        self.gi_novf.setText(str(self.Node_S['NOVF']))
    except:
        pass
    
    try:
        self.gi_iabovf.setText(str(self.Node_S['IABOVF']))
    except:
        pass

    try:
        self.gi_ovfan.setText(str(self.Node_S['OVFAN']))
    except:
        pass
    
    try:
        self.gi_ovfaus.setText(str(self.Node_S['OVFAUS']))
    except:
        pass

    try:
        self.gi_tovfan.setText(str(self.Node_S['TOVFAN']))
    except:
        pass
    try:
        self.gi_tovfaus.setText(str(self.Node_S['TOVFAUS']))
    except:
        pass