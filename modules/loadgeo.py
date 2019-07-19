# -*- coding: utf-8 -*-
"""
Created on Fri May 31 11:28:40 2019

@author: s.Shaji
"""

from PyQt5.QtWidgets import QTableWidgetItem

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