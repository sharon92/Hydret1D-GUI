# -*- coding: utf-8 -*-
"""
Created on Wed Jun 26 11:03:54 2019

@author: s.Shaji
"""
import numpy as np
from modules.plotting import plot_bank
from PyQt5.QtWidgets import QMessageBox,QApplication,QTableWidgetItem

# =============================================================================
# Edit Mode
# =============================================================================
def knoten_label(myapp):
    try:
        myapp.edit_knoten.blockSignals(True)
        inp_knoten = int(myapp.edit_knoten.text())
        ori_knoten = myapp.loc
        if inp_knoten != ori_knoten:
            if inp_knoten in myapp.df_copy.index:
                overwrite = QMessageBox.question(myapp,'Editor',"Knoten Nummer existiert!\nÜberschreiben?",
                                                 QMessageBox.Yes | QMessageBox.No, QMessageBox.No)
                if overwrite == QMessageBox.Yes:
                    myapp.df_copy.drop(labels = inp_knoten,inplace = True)
                    myapp.df_copy.rename({ori_knoten:inp_knoten},inplace = True)
            else:
                myapp.df_copy.rename({ori_knoten:inp_knoten},inplace = True)
        myapp.edit_knoten.blockSignals(False)
    except:
        pass
    
def edit_modus(myapp,i):
    myapp.df_copy.at[myapp.loc,'Mode'] = i
    bankXY = myapp.cal_bank(myapp.Node)
    plot_bank(myapp,bankXY,myapp.plot_bottom,i,myapp.Node,myapp.graphicsView,ModeEdit=True)

def edit_maxHeight(myapp):
    try:
        mh = True
        i = float(myapp.maxHeight_label.text())
    except:
        mh = False
        myapp.statusbar.showMessage('Max Height Error: Not a valid float..')
        
    if mh:
        if (i != -1) & (i < myapp.Node['Y'].min()):
            myapp.statusbar.showMessage('Max Height Error: Lower than min val '+str(myapp.Node['Y'].min()))
        else:
            myapp.df_copy.at[myapp.loc,'Max Height'] = i
            bankXY = myapp.cal_bank(myapp.Node)
            plot_bank(myapp,bankXY,myapp.plot_bottom,myapp.Node['Mode'],myapp.Node,myapp.graphicsView,ModeEdit=True)
            myapp.statusbar.showMessage('Ready')

def delete_coords(myapp):
    myapp.changes +=1
    row_c = myapp.coords_table.selectedIndexes()
    row_c_idx = sorted(set([i.row() for i in row_c]))
    
    if myapp._rquer.isChecked():
        Node = myapp.Node.copy()
        iloc = myapp.iloc
    elif myapp._rrau.isChecked():
        Node = myapp.Node_R.copy()
        iloc = myapp.iloc_r
    elif myapp._rschalter.isChecked():
        Node = myapp.Node_S.copy()
        iloc = myapp.iloc_s
        
    delx = np.round(np.delete(Node['X'], row_c_idx),2)
    dely = np.round(np.delete(Node['Y'], row_c_idx),2)
   
    myapp.df_copy.iat[iloc,6]       = delx
    myapp.df_copy.iat[iloc,7]       = dely
    myapp.df_copy.iat[iloc,0]       = len(delx)
    db_ = myapp.df_copy.copy()
    myapp.df_db.append(db_)
    
    myapp.undo.setEnabled(True)
    myapp.idChange(i=abs(myapp.iloc))


def insert_coords(myapp):
    myapp.changes +=1
    npoints_2insert = myapp.npoints_insert.value()
    
    myapp.row_curr = myapp.coords_table.currentRow()
    if myapp.row_curr == len(myapp.Node['X'])-1:           
        i_x = float(myapp.coords_table.item(myapp.row_curr-1,0).text())
        c_x = float(myapp.coords_table.item(myapp.row_curr,0).text())

        i_y = float(myapp.coords_table.item(myapp.row_curr-1,1).text())
        c_y = float(myapp.coords_table.item(myapp.row_curr,1).text())
        
        n_x = np.linspace(c_x+(c_x-i_x),c_x+npoints_2insert*(c_x-i_x),npoints_2insert)
        n_y = np.linspace(c_y+(c_y-i_y),c_y+npoints_2insert*(c_y-i_y),npoints_2insert)
    else:
        c_x = float(myapp.coords_table.item(myapp.row_curr,0).text())
        i_x = float(myapp.coords_table.item(myapp.row_curr+1,0).text())
        
        c_y = float(myapp.coords_table.item(myapp.row_curr,1).text())
        i_y = float(myapp.coords_table.item(myapp.row_curr+1,1).text())

        n_x = np.linspace(c_x+((i_x-c_x)/npoints_2insert),i_x-((i_x-c_x)/npoints_2insert),npoints_2insert)
        n_y = np.linspace(c_y+((i_y-c_y)/npoints_2insert),i_y-((i_y-c_y)/npoints_2insert),npoints_2insert)    

    if myapp._rquer.isChecked():
        Node = myapp.Node.copy()
        iloc = myapp.iloc
    elif myapp._rrau.isChecked():
        Node = myapp.Node_R.copy()
        iloc = myapp.iloc_r
    elif myapp._rschalter.isChecked():
        Node = myapp.Node_S.copy()
        iloc = myapp.iloc_s
        
    ix  = np.round(np.insert(Node['X'],myapp.row_curr+1,n_x),2)
    iy  = np.round(np.insert(Node['Y'],myapp.row_curr+1,n_y),2)
    
    myapp.df_copy.iat[iloc,6]       = ix
    myapp.df_copy.iat[iloc,7]       = iy
    myapp.df_copy.iat[iloc,0] = len(ix)
    db_ = myapp.df_copy.copy()
    myapp.df_db.append(db_)
    
    myapp.undo.setEnabled(True)
    myapp.idChange(i=abs(myapp.iloc))
    
        
def _handlecopy(myapp):
    sidx = myapp.coords_table.selectedIndexes()
    text = ''
    rows = [i.row() for i in sidx]
    cols = [i.column() for i in sidx]
    
    if myapp._rquer.isChecked():
        Node = myapp.Node.copy()
    elif myapp._rrau.isChecked():
        Node = myapp.Node_R.copy()
    elif myapp._rschalter.isChecked():
        Node = myapp.Node_S.copy()
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

def _handlepaste(myapp):
    clipboard_text =  QApplication.instance().clipboard().text()
    if myapp._rquer.isChecked():
        Node = myapp.Node.copy()
        iloc = myapp.iloc
    elif myapp._rrau.isChecked():
        Node = myapp.Node_R.copy()
        iloc = myapp.iloc_r
    elif myapp._rschalter.isChecked():
        Node = myapp.Node_S.copy()
        iloc = myapp.iloc_s
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
        cid = myapp.coords_table.currentRow()
        pos = myapp.coords_table.currentColumn()
        if data_len > Node['Npoints'] - cid:
            myapp.coords_table.setRowCount(cid+data_len)
            for n,i in enumerate(range(cid,cid+data_len)):
                if (pos == 0) & (cols_ == 1):
                    myapp.coords_table.setItem(i,0,QTableWidgetItem(data[0][n]))
                elif (pos == 0) & (cols_ > 1):
                    myapp.coords_table.setItem(i,0,QTableWidgetItem(data[0][n]))
                    myapp.coords_table.setItem(i,1,QTableWidgetItem(data[1][n]))
                elif pos == 1:
                    myapp.coords_table.setItem(i,1,QTableWidgetItem(data[0][n]))
        try:
            x = [myapp.coords_table.item(i,0).text() for i in range(myapp.coords_table.rowCount()) if i!='']
            y = [myapp.coords_table.item(i,1).text() for i in range(myapp.coords_table.rowCount()) if i!='']
             
            if len(x) == len(y):
                myapp.changes +=1
                myapp.df_copy.iat[iloc,6]       = np.array(x, dtype = np.float)
                myapp.df_copy.iat[iloc,7]       = np.array(y, dtype = np.float)
                myapp.df_copy.iat[iloc,0] = len(x)
                myapp.idChange(myapp.iloc)
                myapp.changes +=1
                db_ = myapp.df_copy.copy()
                myapp.df_db.append(db_)
                myapp.undo.setEnabled(True)
        except:
            pass
         
# =============================================================================
# update mode
# =============================================================================

'''Updating dataframe from Labels in Editing mode'''

def update_labels(myapp):
    if myapp.Edit:

    #def station_label(self):
        try:
            sn = float(myapp.edit_station.text())
            myapp.df_copy.at[myapp.loc,'Station'] = sn
            myapp.edit_station.clear()
        except:
            pass

    #def schnittName_label(self):
        try:
            pn = myapp.edit_pname.text()
            if pn.strip() != '':
                myapp.df_copy.at[myapp.loc,'PName'] = pn
            myapp.edit_pname.clear()
        except:
            pass
        
    #def edit_ctab(self):
        ct = myapp.ctab_label.currentIndex()
        myapp.df_copy.at[myapp.loc,'CTAB'] = ct
        

         
    #def edit_maxHeight(self):
        try:
            myapp.df_copy.at[myapp.loc,'Max Height'] = float(myapp.maxheight_label.text())
        except:
            pass

    #def change_typ(self):
        t1 = myapp.gi_ityp.currentIndex()
        try:
            t2 = int(myapp.gi_typeedit.text())
        
            if t2 not in myapp.qschnt:
                myapp.df_start_copy.at[myapp.loc,'ITYPE'] = t2
        except:
            myapp.df_start_copy.at[myapp.loc,'ITYPE'] = myapp.qschnt[t1]
        
    #def change_id(self):
        try:
            gid = int(myapp.gi_id.text())
            myapp.df_start_copy.at[myapp.loc,'ID'] = gid
        except:
            pass

    #def change_width(self):
        try:
            width = float(myapp.gi_width.text())
            myapp.df_start_copy.at[myapp.loc,'WIDTH'] = width
        except:
            pass

    #def change_heit(self):
        try:
            heit = float(myapp.gi_heit.text())
            myapp.df_start_copy.at[myapp.loc,'HEIT'] = heit
        except:
            pass
            
    #def change_zo(self):
        try:
            zo = float(myapp.gi_zo.text())
            myapp.df_start_copy.at[myapp.loc,'ZO'] = zo
        except:
            pass

    #def change_rni(self):
        try:
            rni = float(myapp.gi_rni.text())
            myapp.df_start_copy.at[myapp.loc,'RNI'] = rni
        except:
            pass

    #def change_dzero(self):
        try:
            dzero = float(myapp.gi_dzero.text())
            myapp.df_start_copy.at[myapp.loc,'DZERO'] = dzero
        except:
            pass

    #def change_qzero(self):
        try:
            qzero = float(myapp.gi_qzero.text())
            myapp.df_start_copy.at[myapp.loc,'QZERO'] = qzero
        except:
            pass

    #def change_zs(self):
        try:
            zs = float(myapp.gi_zs.text())
            myapp.df_start_copy.at[myapp.loc,'ZS'] = zs
        except:
            pass

    #def change_xl(self):
        try:
            xl = float(myapp.gi_xl.text())
            myapp.df_start_copy.at[myapp.loc,'XL'] = xl
        except:
            pass

    #def change_ztr(self):
        try:
            ztr = float(myapp.gi_ztr.text())
            myapp.df_start_copy.at[myapp.loc,'ZTR'] = ztr
        except:
            pass

    #def change_ztl(self):
        try:
            ztl = float(myapp.gi_ztl.text())
            myapp.df_start_copy.at[myapp.loc,'ZTL'] = ztl
        except:
            pass

    #def change_ckm(self):
        try:
            ckm = float(myapp.wsp_ckm.text())
            myapp.df_start_copy.at[myapp.loc,'CKM'] = ckm
        except:
            pass

    #def change_hr0(self):
        try:
            hr0 = float(myapp.wsp_hr0.text())
            myapp.df_start_copy.at[myapp.loc,'HR0'] = hr0
        except:
            pass

    #def change_rw(self):
        try:
            rw = float(myapp.pro_rw.text())
            myapp.df_start_copy.at[myapp.loc,'X'] = rw
        except:
            pass

    #def change_hw(self):
        try:
            hw = float(myapp.pro_hw.text())
            myapp.df_start_copy.at[myapp.loc,'Y'] = hw
        except:
            pass

    #def change_sf(self):
        try:
            sf = float(myapp.gi_sf.text())
            myapp.df_start_copy.at[myapp.loc,'SF'] = sf
        except:
            pass
        
    #def change_wspm(self):
        try:
            for wi in range(7):
                myapp.df_start_copy.at[myapp.loc,'HR'+str(wi+1)] = float(myapp.wsp_table.item(0,wi))
                myapp.df_start_copy.at[myapp.loc,'RN'+str(wi+1)] = float(myapp.wsp_table.item(1,wi))
        except:
            pass
    return myapp