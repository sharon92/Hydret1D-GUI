# -*- coding: utf-8 -*-
"""
Created on Wed Jun 26 11:03:54 2019

@author: s.Shaji
"""

from modules.plotting import update_banks
from PyQt5.QtWidgets import QMessageBox

# =============================================================================
# Edit Mode
# =============================================================================
def knoten_label(self):
    if self.Edit:
        try:
            inp_knoten = int(self.knotenNr.currentText())
            ori_knoten = self.loc
            if inp_knoten != ori_knoten:
                if inp_knoten in self.df_copy.index:
                    overwrite = QMessageBox.question(self,'Editor',"Knoten Nummer existiert!\nÜberschreiben?",
                                                     QMessageBox.Yes | QMessageBox.No, QMessageBox.No)
                    if overwrite == QMessageBox.Yes:
                        self.df_copy.drop(labels = inp_knoten,inplace = True)
                        self.df_copy.rename({ori_knoten:inp_knoten},inplace = True)
                else:
                    self.df_copy.rename({ori_knoten:inp_knoten},inplace = True)
        except:
            pass
    
def edit_modus(self,i):
    if self.Edit:
        self.df_copy.at[self.loc,'Mode'] = i
        update_banks(self,0)

def edit_maxHeight(self,i):
    if self.Edit:
        if (i != -1) & (i < self.Node['Y'].min()):
            self.statusbar.showMessage('Max Height Error: Lower than min val '+str(self.Node['Y'].min()))
        else:
            self.df_copy.at[self.loc,'Max Height'] = i
            update_banks(self,0)
            self.statusbar.showMessage('Ready')

# =============================================================================
# update mode
# =============================================================================

'''Updating dataframe from Labels in Editing mode'''

def update_labels(app):
    if app.Edit:

    #def station_label(app):
        try:
            sn = float(app.edit_station.text())
            app.df_copy.at[app.loc,'Station'] = sn
            app.edit_station.clear()
        except: pass

    #def schnittName_label(app):
        try:
            pn = app.edit_pname.text()
            if pn.strip() != '':
                app.df_copy.at[app.loc,'PName'] = pn
            app.edit_pname.clear()
        except:  pass
        
    #def edit_ctab(app):
        ct = app.ctab_label.currentIndex()
        app.df_copy.at[app.loc,'CTAB'] = ct
    
         
    #def edit_maxHeight(app):
        try:  app.df_copy.at[app.loc,'Max Height'] = float(app.maxheight_label.text())
        except:  pass

    #def change_typ(app):
        t1 = app.gi_ityp.currentIndex()
        try:
            t2 = int(app.gi_typeedit.text())
        
            if t2 not in app.qschnt:
                app.df_start_copy.at[app.loc,'ITYPE'] = t2
        except:  app.df_start_copy.at[app.loc,'ITYPE'] = app.qschnt[t1]
        
    #def change_id(app):
        try: app.df_start_copy.at[app.loc,'ID'] = int(app.gi_id.text())
        except: pass

    #def change_width(app):
        try: app.df_start_copy.at[app.loc,'WIDTH'] =float(app.gi_width.text())
        except: pass

    #def change_heit(app):
        try: app.df_start_copy.at[app.loc,'HEIT'] = float(app.gi_heit.text())
        except: pass
            
    #def change_zo(app):
        try: app.df_start_copy.at[app.loc,'ZO'] = float(app.gi_zo.text())
        except: pass

    #def change_rni(app):
        try: app.df_start_copy.at[app.loc,'RNI'] = float(app.gi_rni.text())
        except: pass

    #def change_dzero(app):
        try: app.df_start_copy.at[app.loc,'DZERO'] = float(app.gi_dzero.text())
        except: pass

    #def change_qzero(app):
        try: app.df_start_copy.at[app.loc,'QZERO'] = float(app.gi_qzero.text())
        except: pass

    #def change_zs(app):
        try: app.df_start_copy.at[app.loc,'ZS'] = float(app.gi_zs.text())
        except: pass

    #def change_xl(app):
        try: app.df_start_copy.at[app.loc,'XL'] = float(app.gi_xl.text())
        except: pass

    #def change_ztr(app):
        try: app.df_start_copy.at[app.loc,'ZTR'] = float(app.gi_ztr.text())
        except: pass

    #def change_ztl(app):
        try: app.df_start_copy.at[app.loc,'ZTL'] = float(app.gi_ztl.text())
        except: pass

    #def change_ckm(app):
        try: app.df_start_copy.at[app.loc,'CKM'] = float(app.wsp_ckm.text())
        except: pass

    #def change_hr0(app):
        try: app.df_start_copy.at[app.loc,'HR0'] = float(app.wsp_hr0.text())
        except: pass

    #def change_rw(app):
        try: app.df_start_copy.at[app.loc,'X'] = float(app.pro_rw.text())
        except: pass

    #def change_hw(app):
        try:app.df_start_copy.at[app.loc,'Y'] = float(app.pro_hw.text())
        except:pass

    #def change_sf(app):
        try: app.df_start_copy.at[app.loc,'SF'] = float(app.gi_sf.text())
        except:pass
        
    #def change_wspm(app):
        try:
            for wi in range(7):
                app.df_start_copy.at[app.loc,'HR'+str(wi+1)] = float(app.wsp_table.item(0,wi))
                app.df_start_copy.at[app.loc,'RN'+str(wi+1)] = float(app.wsp_table.item(1,wi))
        except:pass
    return app

#for pro editor
def update_labels2(app):
    if app.Edit:

    #def station_label(app):
        try:
            sn = float(app.edit_station.text())
            app.df_copy.at[app.loc,'Station'] = sn
        except: pass

    #def schnittName_label(app):
        try:
            pn = app.edit_pname.text()
            if pn.strip() != '':
                app.df_copy.at[app.loc,'PName'] = pn
        except:  pass
        
    #def edit_ctab(app):
        ct = app.ctab_label.currentIndex()
        app.df_copy.at[app.loc,'CTAB'] = ct

    #def edit_maxHeight(app):
        try:  app.df_copy.at[app.loc,'Max Height'] = app.maxheight_label.value()
        except:  pass
    
    return app