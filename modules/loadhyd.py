# -*- coding: utf-8 -*-
"""
Created on Fri May 31 11:28:40 2019

@author: s.Shaji
"""
import os
from PyQt5.QtWidgets import QTableWidgetItem

# =============================================================================
# load hyd here
# =============================================================================

class load_hyd:
    
    def __init__(self,myapp):
        
        self.gui = myapp
        
        '''RUN'''
        try:
            plan = self.gui.h1d.variante
            self.gui.p_plan.setText(str(plan))
        except:
            self.gui.p_plan.setText('HQXXX')
        try:
            com = self.gui.h1d.comments
            self.gui.p_comments.setText(str(com))
        except:
            pass
        
        try:
            cdr =  os.path.dirname(self.gui.h1d.hyd_p).replace('/','\\')
            self.gui.p_bak.setText(os.path.join(cdr,'BAK'))
        except:
            pass
        try:
            hyd_f = self.gui.h1d.hyd_f
            self.gui.p_hyd.setText(str(hyd_f))
        except:
            pass
    
        try:
            aus = self.gui.h1d.out_f
            self.gui.p_aus.setText(str(aus))
        except:
            pass
    
        try:
            pb = self.gui.h1d.plot_b
            if pb == 'J':
                self.gui.p_plot.setCurrentIndex(0)
            else:
                self.gui.p_plot.setCurrentIndex(1)
        except:
            pass
    
        try:
            pb = self.gui.h1d.format
            if pb == 'B':
                self.gui.p_format.setCurrentIndex(0)
            else:
                self.gui.p_format.setCurrentIndex(1)
        except:
            pass
        
        try:
            ach = self.gui.h1d.achse
            self.gui.p_achse.setText(str(ach))
        except:
            pass
        
        try:
            ovf = self.gui.h1d.ovfbil
            if ovf[0:4].upper() == 'KEIN':
                self.gui.p_ovfbil.setCurrentIndex(0)
            elif ovf.upper() == 'OVFBIL':
                self.gui.p_ovfbil.setCurrentIndex(1)
            elif ovf.upper() == 'OVFREG':
                self.gui.p_ovfbil.setCurrentIndex(2)
        except:
            pass
    
        try:
            zul = self.gui.h1d.dhzul
            self.gui.p_dhzul.setText(str(zul))
        except:
            pass
        
        try:
            relzul = self.gui.h1d.dhrelzul
            self.gui.p_dhrelzul.setText(str(relzul))
        except:
            pass
    
        try:
            vrelzul = self.gui.h1d.dvrelzul
            self.gui.p_dvrelzul.setText(str(vrelzul))
        except:
            pass
    
        try:
            kst = self.gui.h1d.kstmod
            if kst[0:4].upper() == 'KEIN':
                self.gui.p_kstmod.setCurrentIndex(0)
            else:
                self.gui.p_kstmod.setCurrentIndex(1)
        except:
            pass
        
        '''HYD'''
        try:
            st = self.gui.h1d.stime
            self.gui.p_stime.setText(str(st))
        except:
            pass
    
        try:
            toth = self.gui.h1d.toth
            self.gui.p_toth.setText(str(toth))
        except:
            pass
        try:
            dt = self.gui.h1d.dt
            self.gui.p_dt.setText(str(dt))
        except:
            pass
    
        try:
            lead = self.gui.h1d.lead
            self.gui.p_lead.setText(str(lead))
        except:
            pass
    
        try:
            tinc = self.gui.h1d.tinc
            self.gui.p_tinc.setText(str(tinc))
        except:
            pass
    
        try:
            self.gui.p_itun.setCurrentIndex(self.gui.h1d.itun)
        except:
            pass
        
        try:
            self.gui.p_ipro.setCurrentIndex(self.gui.h1d.ipro)
        except:
            pass
    
        try:
            self.gui.p_nl.setText(str(self.gui.h1d.nl))
        except:
            pass
        
        try:
            self.gui.p_rain.setText(str(self.gui.h1d.rain))
        except:
            pass
    
        try:
            self.gui.p_nret.setValue(self.gui.h1d.nret)
        except:
            pass
    
        try:
            self.gui.p_nstore.setValue(self.gui.h1d.nstore)
        except:
            pass
        
        try:
            self.gui.p_tol.setValue(self.gui.h1d.tol)
        except:
            pass
    
        try:
            self.gui.p_convec.setText(str(self.gui.h1d.convec))
        except:
            pass
        
        '''Zuflüsse'''
        try:
            self.gui.p_nqin.setValue(self.gui.h1d.nqin)
            self.gui.p_nupe.setRowCount(self.gui.h1d.nqin)
            for wi in range(self.gui.h1d.nqin):
                self.gui.p_nupe.setItem(wi,0,QTableWidgetItem(str(self.gui.h1d.nupe[wi])))
                self.gui.p_nupe.setItem(wi,1,QTableWidgetItem(str(self.gui.h1d.quinf_[wi])))
                self.gui.p_nupe.setItem(wi,2,QTableWidgetItem(str(self.gui.h1d.timmod[wi])))
                self.gui.p_nupe.setItem(wi,3,QTableWidgetItem(str(self.gui.h1d.faktor[wi])))
        except:
            pass
        
        #ganglinien
        try:
            self.gui.p_list_.setText(str(self.gui.h1d.list_))
        except:
            pass
        
        try:
            self.gui.p_dtwel.setText(str(self.gui.h1d.dtwel))
        except:
            pass
    
        try:
            self.gui.p_nwel.setValue(self.gui.h1d.nwel)
            self.gui.p_kwel_table.setRowCount(self.gui.h1d.nwel)
            for wi in range(self.gui.h1d.nwel):
                self.gui.p_kwel_table.setItem(wi,0,QTableWidgetItem(str(self.gui.h1d.kwel[wi])))
        except:
            pass
        
        '''Junctions'''
        try:
            self.gui.p_njunc.setValue(self.gui.h1d.njunc)
            self.gui.p_junnam_table.setRowCount(self.gui.h1d.njunc)
            self.gui.p_nxj_table.setRowCount(self.gui.h1d.njunc)
            self.gui.p_dxlgam_table.setRowCount(self.gui.h1d.njunc)
            for wi in range(self.gui.h1d.njunc):
                self.gui.p_junnam_table.setItem(wi,0,QTableWidgetItem(str(self.gui.h1d.junnam[wi])))
                for ni in range(7):
                    self.gui.p_nxj_table.setItem(wi,ni,QTableWidgetItem(str(self.gui.h1d.nxj[wi][ni])))
                self.gui.p_dxlgam_table.setItem(wi,0,QTableWidgetItem(str(self.gui.h1d.dxl[wi])))
                for pi in range(3):
                    self.gui.p_dxlgam_table.setItem(wi,pi+1,QTableWidgetItem(str(self.gui.h1d.gam[wi][pi])))
        except:
            pass
        
        '''untere Randbedingungen'''
        try:
             self.gui.p_idown.setCurrentIndex(self.gui.h1d.idown-1)
             if self.gui.h1d.idown == 3:
                 self.gui.g_wsg.setEnabled(True)
                 self.gui.g_abg.setEnabled(False)
                 self.gui.g_wehr.setEnabled(False)
                 self.gui.p_wsg_dat.setText(self.gui.h1d.rb[0:28])
                 if self.gui.h1d.rb[28:30] == 'IN':
                     self.gui.p_wsg_mod.setCurrentIndex(0)
                 elif self.gui.h1d.rb[28:30] == 'ST':
                     self.gui.p_wsg_mod.setCurrentIndex(1)
                 try:
                     self.gui.p_wsg_fak.setValue(float(self.gui.h1d.rb[30:]))
                 except:
                     pass
             elif self.gui.h1d.idown == 4:
                 self.gui.g_wsg.setEnabled(False)
                 self.gui.g_abg.setEnabled(True)
                 self.gui.g_wehr.setEnabled(False)
                 self.gui.p_abg_dat.setText(self.gui.rb[0:28])
                 if self.gui.rb[28:30] == 'IN':
                     self.gui.p_abg_mod.setCurrentIndex(0)
                 elif self.gui.rb[28:30] == 'ST':
                     self.gui.p_abg_mod.setCurrentIndex(1)
                 try:
                     self.gui.p_abg_fak.setValue(float(self.gui.rb[30:]))
                 except:
                     pass
             elif self.gui.h1d.idown == 1:
                 self.gui.g_wsg.setEnabled(False)
                 self.gui.g_abg.setEnabled(False)
                 self.gui.g_wehr.setEnabled(True)
                 self.gui.p_wehr_b.setValue(self.gui.h1d.weir_par[0])
                 self.gui.p_wehr_k.setValue(self.gui.h1d.weir_par[1])
                 self.gui.p_wehr_h.setValue(self.gui.h1d.weir_par[2])
                 try:
                     self.gui.p_wehr_qm.setValue(self.gui.h1d.weir_par[3])
                 except:
                     pass
    
        except:
            pass
        
        '''Seitliche Zuflussganglinien'''
        try:
            self.gui.p_latinf.setValue(self.gui.h1d.latinf)
            self.gui.p_lie_table.setRowCount(self.gui.h1d.latinf)
            for wi in range(self.gui.h1d.latinf):
                self.gui.p_lie_table.setItem(wi,0,QTableWidgetItem(str(self.gui.h1d.lie[wi])))
                self.gui.p_lie_table.setItem(wi,6,QTableWidgetItem(str(self.gui.h1d.latcom[wi])))
                try:
                    self.gui.p_lie_table.setItem(wi,1,QTableWidgetItem(str(self.gui.h1d.zdat[wi][0:28])))
                except:
                    pass
                try:
                    self.gui.p_lie_table.setItem(wi,2,QTableWidgetItem(str(self.gui.h1d.zdat[wi][28:30])))
                except:
                    pass
                try:
                    self.gui.p_lie_table.setItem(wi,3,QTableWidgetItem(str(self.gui.h1d.zdat[wi][30:40])))
                except:
                    pass
                try:
                    self.gui.p_lie_table.setItem(wi,4,QTableWidgetItem(str(self.gui.h1d.zdat[wi][40:50])))
                except:
                    pass
                try:
                    self.gui.p_lie_table.setItem(wi,5,QTableWidgetItem(str(self.gui.h1d.zdat[wi][50:60])))
                except:
                    pass
        except:
            pass
        
        '''Weirs and gates'''
        try:
            self.gui.p_nweirs.setValue(self.gui.h1d.nweirs)
            self.gui.p_weir_table.setRowCount(self.gui.h1d.nweirs)
            for wi in range(self.gui.h1d.nweirs):
                try:
                    for v in range(9):
                        self.gui.p_weir_table.setItem(wi,v,QTableWidgetItem(str(self.gui.h1d.weir_info[wi][0+v*10:10+v*10].strip())))
                    self.gui.p_weir_table.setItem(wi,9,QTableWidgetItem(str(self.gui.h1d.weir_info[wi][90:].strip())))
                except:
                    pass
        except:
            pass
        try:
            self.gui.p_ngates.setValue(self.gui.h1d.ngates)
            self.gui.p_gate_table.setRowCount(self.gui.h1d.ngates)
            for wi in range(self.gui.h1d.ngates):
                self.gui.p_gate_table.setItem(wi,0,QTableWidgetItem(str(self.gui.h1d.igate[wi])))
                self.gui.p_gate_table.setItem(wi,2,QTableWidgetItem(str(self.gui.h1d.iaga[wi])))
                for n,i in enumerate([1,3,4,5,6,7,8,9]):
                    self.gui.p_gate_table.setItem(wi,i,QTableWidgetItem(str(self.gui.h1d.gatdat[wi][n])))
        except:
            pass
        
        '''Geometry Data'''
        try:
            self.gui.p_prodat.setText(self.gui.h1d.prodat)
        except:
            pass
        
        try:
            self.gui.p_slo.setValue(self.gui.h1d.slo)
        except:
            pass
        
        try:
            self.gui.p_xsecmo.setCurrentIndex(['VK','RK','VF','RF'].index(self.gui.h1d.xsecmo))
        except:
            pass
        
        try:
            self.gui.p_startdat.setText(self.gui.h1d.startdat)
        except:
            pass

# =============================================================================
# append rows for value changes
# =============================================================================
def inflowNodes(myapp):
    myapp.p_nupe.setRowCount(myapp.p_nqin.value())
    try:
        for wi in range(myapp.h1d.nqin):
            myapp.p_nupe.setItem(wi,0,QTableWidgetItem(str(myapp.h1d.nupe[wi])))
            myapp.p_nupe.setItem(wi,1,QTableWidgetItem(str(myapp.h1d.quinf_[wi])))
            myapp.p_nupe.setItem(wi,2,QTableWidgetItem(str(myapp.h1d.timmod[wi])))
            myapp.p_nupe.setItem(wi,3,QTableWidgetItem(str(myapp.h1d.faktor[wi])))
    except:
        pass
    
def printCurves(myapp):
    myapp.p_kwel_table.setRowCount(myapp.p_nwel.value())
    try:
        for wi in range(myapp.h1d.nwel):
            myapp.p_kwel_table.setItem(wi,0,QTableWidgetItem(str(myapp.h1d.kwel[wi])))
    except:
        pass
    
def defWeirs(myapp):
    myapp.p_weir_table.setRowCount(myapp.p_nweirs.value())
    try:
        for wi in range(myapp.h1d.nweirs):
            try:
                for v in range(9):
                    myapp.p_weir_table.setItem(wi,v,QTableWidgetItem(str(myapp.h1d.weir_info[wi][0+v*10:10+v*10].strip())))
                myapp.p_weir_table.setItem(wi,9,QTableWidgetItem(str(myapp.h1d.weir_info[wi][90:].strip())))
            except:
                pass
    except:
        pass

def defGates(myapp):
    myapp.p_gate_table.setRowCount(myapp.p_ngates.value())
    try:
        for wi in range(myapp.h1d.ngates):
            myapp.p_gate_table.setItem(wi,0,QTableWidgetItem(str(myapp.h1d.igate[wi])))
            myapp.p_gate_table.setItem(wi,2,QTableWidgetItem(str(myapp.h1d.iaga[wi])))
            for n,i in enumerate([1,3,4,5,6,7,8,9]):
                myapp.p_gate_table.setItem(wi,i,QTableWidgetItem(str(myapp.h1d.gatdat[wi][n])))
    except:
        pass
        
def defJunctions(myapp):
    myapp.p_junnam_table.setRowCount(myapp.p_njunc.value())
    myapp.p_nxj_table.setRowCount(myapp.p_njunc.value())
    myapp.p_dxlgam_table.setRowCount(myapp.p_njunc.value())
    try:
        for wi in range(myapp.h1d.njunc):
            myapp.p_junnam_table.setItem(wi,0,QTableWidgetItem(str(myapp.h1d.junnam[wi])))
            for ni in range(7):
                myapp.p_nxj_table.setItem(wi,ni,QTableWidgetItem(str(myapp.h1d.nxj[wi][ni])))
            myapp.p_dxlgam_table.setItem(wi,0,QTableWidgetItem(str(myapp.h1d.dxl[wi])))
            for pi in range(3):
                myapp.p_dxlgam_table.setItem(wi,pi+1,QTableWidgetItem(str(myapp.h1d.gam[wi][pi])))
    except:
        pass
    
def lateralInflows(myapp):
    myapp.p_lie_table.setRowCount(myapp.p_latinf.value())
    try:
        for wi in range(myapp.h1d.latinf):
            myapp.p_lie_table.setItem(wi,0,QTableWidgetItem(str(myapp.h1d.lie[wi])))
            myapp.p_lie_table.setItem(wi,6,QTableWidgetItem(str(myapp.h1d.latcom[wi])))
            for ni in range(4):
                myapp.p_lie_table.setItem(wi,1,QTableWidgetItem(str(myapp.h1d.zdat[wi][0:28])))
                myapp.p_lie_table.setItem(wi,2,QTableWidgetItem(str(myapp.h1d.zdat[wi][28:30])))
                myapp.p_lie_table.setItem(wi,3,QTableWidgetItem(str(myapp.h1d.zdat[wi][30:40])))
                myapp.p_lie_table.setItem(wi,4,QTableWidgetItem(str(myapp.h1d.zdat[wi][40:50])))
                myapp.p_lie_table.setItem(wi,5,QTableWidgetItem(str(myapp.h1d.zdat[wi][50:60])))
    except:
        pass

def ovfmode(myapp,i):
    if myapp.Edit:
        if i == 0:
            myapp.ovf_modbox.setEnabled(False)
        elif (i ==1) or (i==2):
            myapp.ovf_modbox.setEnabled(True)

# =============================================================================
# Update Data from input
# =============================================================================
def updateHyd(myapp):
    '''RUN'''
    myapp.h1d.hyd_f    = myapp.p_hyd.text()
    myapp.h1d.out_f    = myapp.p_aus.text()
    myapp.h1d.plot_b   = myapp.p_plot.currentText()[0]
    myapp.h1d.format   = myapp.p_format.currentText()[0]
    myapp.h1d.achse    = myapp.p_achse.text()
    myapp.h1d.ovfbil   = myapp.p_ovfbil_2.currentText()
    myapp.h1d.dhzul    = float(myapp.p_dhzul.text())
    myapp.h1d.dhrelzul = float(myapp.p_dhrelzul.text())
    myapp.h1d.dvrelzul = float(myapp.p_dvrelzul.text())
    myapp.h1d.kstmod   = myapp.p_kstmod.currentText()
    
    '''HYD'''
    myapp.h1d.stime = float(myapp.p_stime.text())
    myapp.h1d.toth  = float(myapp.p_toth.text())
    myapp.h1d.dt    = float(myapp.p_dt.text())
    myapp.h1d.lead  = int(myapp.p_lead.text())
    myapp.h1d.tinc  = float(myapp.p_tinc.text())
    myapp.h1d.itun  = myapp.p_itun.currentIndex()
    myapp.h1d.ipro  = myapp.p_ipro.currentIndex()
    myapp.h1d.nl    = int(myapp.p_nl.text())
    myapp.h1d.rain  = myapp.p_rain.text()
    myapp.h1d.nret  = myapp.p_nret.value()
    myapp.h1d.nstore= myapp.p_nstore.value()
    myapp.h1d.tol   = myapp.p_tol.value()
    myapp.h1d.convec= float(myapp.p_convec.text())
    
    '''Zuflüsse'''
    myapp.h1d.nqin  = myapp.p_nqin.value()
    myapp.h1d.nupe    = [None]*myapp.h1d.nqin
    myapp.h1d.quinf_  = [None]*myapp.h1d.nqin
    myapp.h1d.timmod  = [None]*myapp.h1d.nqin
    myapp.h1d.faktor  = [None]*myapp.h1d.nqin
    
    for wi in range(myapp.h1d.nqin):
        myapp.h1d.nupe[wi]   = int(myapp.p_nupe.item(wi,0).text())
        myapp.h1d.quinf_[wi] = myapp.p_nupe.item(wi,1).text()
        myapp.h1d.timmod[wi] = myapp.p_nupe.item(wi,2).text()
        myapp.h1d.faktor[wi] = float(myapp.p_nupe.item(wi,3).text())

    
    '''ganglinien'''
    myapp.h1d.list_ = int(myapp.p_list_.text())
    myapp.h1d.dtwel = float(myapp.p_dtwel.text())
    myapp.h1d.nwel  = myapp.p_nwel.value()
    myapp.h1d.kwel  = [None]*myapp.h1d.nwel
    for wi in range(myapp.h1d.nwel):
        myapp.h1d.kwel[wi] = int(myapp.p_kwel_table.item(wi,0).text())

    '''Junctions'''
    myapp.h1d.njunc = myapp.p_njunc.value()
    myapp.h1d.junnam,myapp.h1d.nxj,myapp.h1d.dxl,myapp.h1d.gam = ([None]*myapp.h1d.njunc,
                                                                  [None]*myapp.h1d.njunc,
                                                                  [None]*myapp.h1d.njunc,
                                                                  [None]*myapp.h1d.njunc)
    
    for wi in range(myapp.h1d.njunc):
        myapp.h1d.junnam[wi] = myapp.p_junnam_table.item(wi,0).text()
        x = []
        for ni in range(7):
            x.append(int(myapp.p_nxj_table.item(wi,ni).text()))
        myapp.h1d.nxj[wi] = x
        myapp.h1d.dxl[wi] = float(myapp.p_dxlgam_table.item(wi,0).text())
        x = []
        for pi in range(3):
            x.append(float(myapp.p_dxlgam_table.item(wi,pi+1).text()))
        myapp.h1d.gam[wi] = x
        
    '''untere Randbedingungen'''
    myapp.h1d.idown = myapp.p_idown.currentIndex() + 1

    if myapp.h1d.idown == 3:
        myapp.h1d.rb = myapp.p_wsg_dat.text() + myapp.p_wsg_mod.currentText() + '%8.2f' %myapp.p_wsg_fak.value()

    elif myapp.h1d.idown == 4:
        myapp.h1d.rb = myapp.p_abg_dat.text() + myapp.p_abg_mod.currentText() + '%8.2f' %myapp.p_abg_fak.value()

    elif myapp.h1d.idown == 1:
        myapp.h1d.weir_par = [myapp.p_wehr_b.value(),myapp.p_wehr_k.value(),myapp.p_wehr_h.value(),myapp.p_wehr_qm.value()]

    
    '''Seitliche Zuflussganglinien'''
    myapp.h1d.latinf = myapp.p_latinf.value()
    myapp.h1d.lie    = [0]*myapp.h1d.latinf
    myapp.h1d.latcom = [0]*myapp.h1d.latinf
    myapp.h1d.zdat   = ['']*myapp.h1d.latinf
    for wi in range(myapp.h1d.latinf):
        myapp.h1d.lie[wi]    = int(myapp.p_lie_table.item(wi,0).text())
        myapp.h1d.latcom[wi] = int(myapp.p_lie_table.item(wi,6).text())
        myapp.h1d.zdat[wi]   = (myapp.p_lie_table.item(wi,1).text()+myapp.p_lie_table.item(wi,2).text()
        +myapp.p_lie_table.item(wi,3).text()+myapp.p_lie_table.item(wi,4).text()+myapp.p_lie_table.item(wi,5).text())
    
    '''Weirs and gates'''
    myapp.h1d.nweirs        = myapp.p_nweirs.value()
    myapp.h1d.weir_info     = ['']*myapp.h1d.nweirs
    for wi in range(myapp.h1d.nweirs):
        x = []
        for v in range(9):
            x.append(myapp.p_weir_table.item(wi,v).text())
        x.append(myapp.p_weir_table.item(wi,9).text())
        myapp.h1d.weir_info[wi] = x

    myapp.h1d.ngates = myapp.p_ngates.value()
    myapp.h1d.igate  = [0]*myapp.h1d.ngates
    myapp.h1d.iaga   = [0.]*myapp.h1d.ngates
    myapp.h1d.gatdat = ['']*myapp.h1d.ngates
    for wi in range(myapp.h1d.ngates):
        myapp.h1d.igate[wi] = int(myapp.p_gate_table.item(wi,0).text())
        myapp.h1d.iaga[wi]  = float(myapp.p_gate_table.item(wi,2).text())
        x = []
        for n,i in enumerate([1,3,4,5,6,7,8,9]):
            x.append(myapp.p_gate_table.item(wi,i).text())
        myapp.h1d.gatdat[wi] = x

    '''Geometry Data'''
    myapp.h1d.prodat   = myapp.p_prodat.text()
    myapp.h1d.xsecmo   = myapp.p_xsecmo.currentText()
    myapp.h1d.slo      = myapp.p_slo.value()
    myapp.h1d.startdat = myapp.p_startdat.text()

    return myapp.h1d