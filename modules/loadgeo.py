# -*- coding: utf-8 -*-
"""
Created on Fri May 31 11:28:40 2019

@author: s.Shaji
"""

from PyQt5.QtWidgets import QTableWidgetItem

# =============================================================================
# load geometrie info
# =============================================================================
def load_start(myapp):
    
    try:
        myapp.Node_S = myapp.df_s.loc[myapp.loc]
        try:
            myapp.gi_ityp.setCurrentIndex(myapp.qschnt.index(int(myapp.Node_S['ITYPE'])))
        except:
            myapp.qschnt.append(int(myapp.Node_S['ITYPE']))
            myapp.gi_ityp.addItem(str(int(myapp.Node_S['ITYPE'])))
            myapp.gi_ityp.setCurrentIndex(myapp.qschnt.index(int(myapp.Node_S['ITYPE'])))
    except:
        pass
    
    try:
        myapp.gi_id.setText(str(int(myapp.Node_S['ID'])))
    except:
         myapp.gi_id.setText('n/a')
         
    try:
        myapp.gi_width.setText(str(myapp.Node_S['WIDTH']))
    except:
        myapp.gi_width.setText('n/a')
    
    try:
        myapp.gi_heit.setText(str(myapp.Node_S['HEIT']))
    except:
        myapp.gi_heit.setText('n/a')
        
    try:
        myapp.gi_zo.setText(str(myapp.Node_S['ZO']))
    except:
        myapp.gi_zo.setText('n/a')
    
    try:
        myapp.gi_rni.setText(str(myapp.Node_S['RNI']))
    except:
        myapp.gi_rni.setText('n/a')
    
    try:
        myapp.gi_dzero.setText(str(myapp.Node_S['DZERO']))
    except:
         myapp.gi_dzero.setText('n/a')
    
    try:
        myapp.gi_qzero.setText(str(myapp.Node_S['QZERO']))
    except:
        myapp.gi_qzero.setText('n/a')
        
    try:
        myapp.gi_zs.setText(str(myapp.Node_S['ZS']))
    except:
        myapp.gi_zs.setText('n/a')
    
    try:
        myapp.gi_sf.setText(str(myapp.Node_S['SF']))
    except:
        myapp.gi_sf.setText('n/a')
        
    try:
        myapp.gi_xl.setText(str(myapp.Node_S['XL']))
    except:
        myapp.gi_xl.setText('n/a')
        
    try:
        myapp.gi_ztr.setText(str(myapp.Node_S['ZTR']))
    except:
         myapp.gi_ztr.setText('n/a')
    
    try:
        myapp.gi_ztl.setText(str(myapp.Node_S['ZTL']))
    except:
        myapp.gi_ztl.setText('n/a')
        
    try:
        myapp.wsp_ckm.setText(str(myapp.Node_S['CKS']))
    except:
        myapp.wsp_ckm.setText('n/a')
        
    try:
        myapp.wsp_hr0.setText(str(myapp.Node_S['HR0']))
    except:
        myapp.wsp_hr0.setText('n/a')
        
    for wsp_t in range(7):
        try:
            myapp.wsp_table.setItem(wsp_t,0,QTableWidgetItem(str(myapp.Node_S['HR'+str(wsp_t+1)])))
        except:
            myapp.wsp_table.setItem(wsp_t,0,QTableWidgetItem('n/a'))
        try:
            myapp.wsp_table.setItem(wsp_t,1,QTableWidgetItem(str(myapp.Node_S['RNV'+str(wsp_t+1)])))
        except:
            myapp.wsp_table.setItem(wsp_t,1,QTableWidgetItem('n/a'))
    
    try:
        myapp.pro_rw.setText(str(myapp.Node_S['X']))
    except:
        myapp.pro_rw.setText('n/a')
        
    try:
        myapp.pro_hw.setText(str(myapp.Node_S['Y']))
    except:
        myapp.pro_hw.setText('n/a')

    try:
        myapp.gi_kstime.setText(str(myapp.Node_S['KSTIME']))
    except:
        pass
    
    try:
        myapp.gi_lire.setCurrentIndex(int(myapp.Node_S['LIRE']))
    except:
        pass
    
    try:
        myapp.gi_qomax.setText(str(myapp.Node_S['QOMAX']))
    except:
        pass

    try:
        myapp.gi_qregel.setText(str(myapp.Node_S['QREGEL']))
    except:
        pass
    
    try:
        myapp.gi_novf.setText(str(myapp.Node_S['NOVF']))
    except:
        pass
    
    try:
        myapp.gi_iabovf.setText(str(myapp.Node_S['IABOVF']))
    except:
        pass

    try:
        myapp.gi_ovfan.setText(str(myapp.Node_S['OVFAN']))
    except:
        pass
    
    try:
        myapp.gi_ovfaus.setText(str(myapp.Node_S['OVFAUS']))
    except:
        pass

    try:
        myapp.gi_tovfan.setText(str(myapp.Node_S['TOVFAN']))
    except:
        pass
    try:
        myapp.gi_tovfaus.setText(str(myapp.Node_S['TOVFAUS']))
    except:
        pass