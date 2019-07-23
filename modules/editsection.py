# -*- coding: utf-8 -*-
"""
Created on Wed Jun 26 11:03:54 2019

@author: s.Shaji
"""
import numpy as np
from modules.plotting import update_banks
from PyQt5.QtWidgets import QMessageBox,QApplication,QTableWidgetItem

# =============================================================================
# Edit Mode
# =============================================================================
def knoten_label(self):
    try:
        self.edit_knoten.blockSignals(True)
        inp_knoten = int(self.edit_knoten.text())
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
        self.edit_knoten.blockSignals(False)
    except:
        pass
    
def edit_modus(self,i):
    self.df_copy.at[self.loc,'Mode'] = i
    update_banks(self,0)

def edit_maxHeight(self):
    try:
        mh = True
        i = float(self.maxHeight_label.text())
    except:
        mh = False
        self.statusbar.showMessage('Max Height Error: Not a valid float..')
        
    if mh:
        if (i != -1) & (i < self.Node['Y'].min()):
            self.statusbar.showMessage('Max Height Error: Lower than min val '+str(self.Node['Y'].min()))
        else:
            self.df_copy.at[self.loc,'Max Height'] = i
            update_banks(self,0)
            self.statusbar.showMessage('Ready')
    
        
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
    clipboard_text =  QApplication.instance().clipboard().text()
    if self._rquer.isChecked():
        Node = self.Node.copy()
        iloc = self.iloc
    elif self._rrau.isChecked():
        Node = self.Node_R.copy()
        iloc = self.iloc_r
    elif self._rschalter.isChecked():
        Node = self.Node_S.copy()
        iloc = self.iloc_s
    if clipboard_text:
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
        try:
            x = [self.coords_table.item(i,0).text() for i in range(self.coords_table.rowCount()) if i!='']
            y = [self.coords_table.item(i,1).text() for i in range(self.coords_table.rowCount()) if i!='']
             
            if len(x) == len(y):
                self.changes +=1
                self.df_copy.iat[iloc,6]       = np.array(x, dtype = np.float)
                self.df_copy.iat[iloc,7]       = np.array(y, dtype = np.float)
                self.df_copy.iat[iloc,0] = len(x)
                self.idChange(self.iloc)
                self.changes +=1
                db_ = self.df_copy.copy()
                self.df_db.append(db_)
                self.undo.setEnabled(True)
        except:
            pass
         
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
        except:
            pass

    #def schnittName_label(app):
        try:
            pn = app.edit_pname.text()
            if pn.strip() != '':
                app.df_copy.at[app.loc,'PName'] = pn
            app.edit_pname.clear()
        except:
            pass
        
    #def edit_ctab(app):
        ct = app.ctab_label.currentIndex()
        app.df_copy.at[app.loc,'CTAB'] = ct
        

         
    #def edit_maxHeight(app):
        try:
            app.df_copy.at[app.loc,'Max Height'] = float(app.maxheight_label.text())
        except:
            pass

    #def change_typ(app):
        t1 = app.gi_ityp.currentIndex()
        try:
            t2 = int(app.gi_typeedit.text())
        
            if t2 not in app.qschnt:
                app.df_start_copy.at[app.loc,'ITYPE'] = t2
        except:
            app.df_start_copy.at[app.loc,'ITYPE'] = app.qschnt[t1]
        
    #def change_id(app):
        try:
            gid = int(app.gi_id.text())
            app.df_start_copy.at[app.loc,'ID'] = gid
        except:
            pass

    #def change_width(app):
        try:
            width = float(app.gi_width.text())
            app.df_start_copy.at[app.loc,'WIDTH'] = width
        except:
            pass

    #def change_heit(app):
        try:
            heit = float(app.gi_heit.text())
            app.df_start_copy.at[app.loc,'HEIT'] = heit
        except:
            pass
            
    #def change_zo(app):
        try:
            zo = float(app.gi_zo.text())
            app.df_start_copy.at[app.loc,'ZO'] = zo
        except:
            pass

    #def change_rni(app):
        try:
            rni = float(app.gi_rni.text())
            app.df_start_copy.at[app.loc,'RNI'] = rni
        except:
            pass

    #def change_dzero(app):
        try:
            dzero = float(app.gi_dzero.text())
            app.df_start_copy.at[app.loc,'DZERO'] = dzero
        except:
            pass

    #def change_qzero(app):
        try:
            qzero = float(app.gi_qzero.text())
            app.df_start_copy.at[app.loc,'QZERO'] = qzero
        except:
            pass

    #def change_zs(app):
        try:
            zs = float(app.gi_zs.text())
            app.df_start_copy.at[app.loc,'ZS'] = zs
        except:
            pass

    #def change_xl(app):
        try:
            xl = float(app.gi_xl.text())
            app.df_start_copy.at[app.loc,'XL'] = xl
        except:
            pass

    #def change_ztr(app):
        try:
            ztr = float(app.gi_ztr.text())
            app.df_start_copy.at[app.loc,'ZTR'] = ztr
        except:
            pass

    #def change_ztl(app):
        try:
            ztl = float(app.gi_ztl.text())
            app.df_start_copy.at[app.loc,'ZTL'] = ztl
        except:
            pass

    #def change_ckm(app):
        try:
            ckm = float(app.wsp_ckm.text())
            app.df_start_copy.at[app.loc,'CKM'] = ckm
        except:
            pass

    #def change_hr0(app):
        try:
            hr0 = float(app.wsp_hr0.text())
            app.df_start_copy.at[app.loc,'HR0'] = hr0
        except:
            pass

    #def change_rw(app):
        try:
            rw = float(app.pro_rw.text())
            app.df_start_copy.at[app.loc,'X'] = rw
        except:
            pass

    #def change_hw(app):
        try:
            hw = float(app.pro_hw.text())
            app.df_start_copy.at[app.loc,'Y'] = hw
        except:
            pass

    #def change_sf(app):
        try:
            sf = float(app.gi_sf.text())
            app.df_start_copy.at[app.loc,'SF'] = sf
        except:
            pass
        
    #def change_wspm(app):
        try:
            for wi in range(7):
                app.df_start_copy.at[app.loc,'HR'+str(wi+1)] = float(app.wsp_table.item(0,wi))
                app.df_start_copy.at[app.loc,'RN'+str(wi+1)] = float(app.wsp_table.item(1,wi))
        except:
            pass
    return app