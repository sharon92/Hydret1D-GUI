# -*- coding: utf-8 -*-
"""
Created on Fri May 31 11:28:40 2019

@author: s.Shaji
"""

from PyQt5.QtWidgets import QTableWidgetItem

# =============================================================================
# load hyd here
# =============================================================================

def load_hyd(self):
    
    '''RUN'''
    try:
        plan = self.h1d.variante
        self.p_plan.setText(str(plan))
    except:
        self.p_plan.setText('HQXXX')
    try:
        com = self.h1d.comments
        self.p_comments.setText(str(com))
    except:
        pass
    
    try:
        hyd_f = self.h1d.hyd_f
        self.p_hyd.setText(str(hyd_f))
    except:
        pass

    try:
        aus = self.h1d.out_f
        self.p_aus.setText(str(aus))
    except:
        pass

    try:
        pb = self.h1d.plot_b
        if pb == 'J':
            self.p_plot.setCurrentIndex(0)
        else:
            self.p_plot.setCurrentIndex(1)
    except:
        pass

    try:
        pb = self.h1d.format
        if pb[0] == 'B':
            self.p_format.setCurrentIndex(0)
        elif pb[0] == 'T':
            self.p_format.setCurrentIndex(2)
        elif pb[0] == 'A':
            self.p_format.setCurrentIndex(1)
    except:
        pass
    
    try:
        ach = self.h1d.achse
        self.p_achse.setText(str(ach))
    except:
        pass
    
    try:
        ovf = self.h1d.ovfbil
        if ovf[0:4].upper() == 'KEIN':
            self.p_ovfbil.setCurrentIndex(0)
        elif ovf.upper() == 'OVFBIL':
            self.p_ovfbil.setCurrentIndex(1)
        elif ovf.upper() == 'OVFREG':
            self.p_ovfbil.setCurrentIndex(2)
    except:
        pass

    try:
        zul = self.h1d.dhzul
        self.p_dhzul.setText(str(zul))
    except:
        pass
    
    try:
        relzul = self.h1d.dhrelzul
        self.p_dhrelzul.setText(str(relzul))
    except:
        pass

    try:
        vrelzul = self.h1d.dvrelzul
        self.p_dvrelzul.setText(str(vrelzul))
    except:
        pass

    try:
        kst = self.h1d.kstmod
        if kst[0:4].upper() == 'KEIN':
            self.p_kstmod.setCurrentIndex(0)
        else:
            self.p_kstmod.setCurrentIndex(1)
    except:
        pass
    
    '''HYD'''
    try:
        st = self.h1d.stime
        self.p_stime.setText(str(st))
    except:
        pass

    try:
        toth = self.h1d.toth
        self.p_toth.setText(str(toth))
    except:
        pass
    try:
        dt = self.h1d.dt
        self.p_dt.setText(str(dt))
    except:
        pass

    try:
        lead = self.h1d.lead
        self.p_lead.setText(str(lead))
    except:
        pass

    try:
        tinc = self.h1d.tinc
        self.p_tinc.setText(str(tinc))
    except:
        pass

    try:
        self.p_itun.setCurrentIndex(self.h1d.itun)
    except:
        pass
    
    try:
        self.p_ipro.setCurrentIndex(self.h1d.ipro)
    except:
        pass

    try:
        self.p_nl.setText(str(self.h1d.nl))
    except:
        pass
    
    try:
        self.p_rain.setText(str(self.h1d.rain))
    except:
        pass

    try:
        self.p_nret.setValue(self.h1d.nret)
    except:
        pass

    try:
        self.p_nstore.setValue(self.h1d.nstore)
    except:
        pass
    
    try:
        self.p_tol.setValue(self.h1d.tol)
    except:
        pass

    try:
        self.p_convec.setText(str(self.h1d.convec))
    except:
        pass
    
    '''Zuflüsse'''
    try:
        self.p_nqin.setValue(self.h1d.nqin)
        self.p_nupe.setRowCount(self.h1d.nqin)
        for wi in range(self.h1d.nqin):
            self.p_nupe.setItem(wi,0,QTableWidgetItem(str(self.h1d.nupe[wi])))
            self.p_nupe.setItem(wi,1,QTableWidgetItem(str(self.h1d.quinf_[wi])))
            self.p_nupe.setItem(wi,2,QTableWidgetItem(str(self.h1d.timmod[wi])))
            self.p_nupe.setItem(wi,3,QTableWidgetItem(str(self.h1d.faktor[wi])))
    except:
        pass
    
    #ganglinien
    try:
        self.p_list_.setText(str(self.h1d.list_))
    except:
        pass
    
    try:
        self.p_dtwel.setText(str(self.h1d.dtwel))
    except:
        pass

    try:
        self.p_nwel.setValue(self.h1d.nwel)
        self.p_kwel_table.setRowCount(self.h1d.nwel)
        for wi in range(self.h1d.nwel):
            self.p_kwel_table.setItem(wi,0,QTableWidgetItem(str(self.h1d.kwel[wi])))
    except:
        pass
    
    '''Junctions'''
    try:
        self.p_njunc.setValue(self.h1d.njunc)
        self.p_junnam_table.setRowCount(self.h1d.njunc)
        self.p_nxj_table.setRowCount(self.h1d.njunc)
        self.p_dxlgam_table.setRowCount(self.h1d.njunc)
        for wi in range(self.h1d.njunc):
            self.p_junnam_table.setItem(wi,0,QTableWidgetItem(str(self.h1d.junnam[wi])))
            for ni in range(7):
                self.p_nxj_table.setItem(wi,ni,QTableWidgetItem(str(self.h1d.nxj[wi][ni])))
            self.p_dxlgam_table.setItem(wi,0,QTableWidgetItem(str(self.h1d.dxl[wi])))
            for pi in range(3):
                self.p_dxlgam_table.setItem(wi,pi+1,QTableWidgetItem(str(self.h1d.gam[wi][pi])))
    except:
        pass
    
    '''untere Randbedingungen'''
    try:
         self.p_idown.setCurrentIndex(self.h1d.idown-1)
         if self.h1d.idown == 3:
             self.g_wsg.setEnabled(True)
             self.g_abg.setEnabled(False)
             self.g_wehr.setEnabled(False)
             self.normalabfluss.setEnabled(False)
             self.p_wsg_dat.setText(self.h1d.rb[0:28])
             if self.h1d.rb[28:30] == 'IN':
                 self.p_wsg_mod.setCurrentIndex(0)
             elif self.h1d.rb[28:30] == 'ST':
                 self.p_wsg_mod.setCurrentIndex(1)
             try:
                 self.p_wsg_fak.setValue(float(self.h1d.rb[30:]))
             except:
                 pass
         elif self.h1d.idown == 4:
             self.g_wsg.setEnabled(False)
             self.g_abg.setEnabled(True)
             self.g_wehr.setEnabled(False)
             self.normalabfluss.setEnabled(False)
             self.p_abg_dat.setText(self.rb[0:28])
             if self.rb[28:30] == 'IN':
                 self.p_abg_mod.setCurrentIndex(0)
             elif self.rb[28:30] == 'ST':
                 self.p_abg_mod.setCurrentIndex(1)
             try:
                 self.p_abg_fak.setValue(float(self.rb[30:]))
             except:
                 pass
         elif self.h1d.idown == 1:
             self.g_wsg.setEnabled(False)
             self.g_abg.setEnabled(False)
             self.g_wehr.setEnabled(True)
             self.normalabfluss.setEnabled(False)
             self.p_wehr_b.setValue(self.h1d.weir_par[0])
             self.p_wehr_k.setValue(self.h1d.weir_par[1])
             self.p_wehr_h.setValue(self.h1d.weir_par[2])
             try:
                 self.p_wehr_qm.setValue(self.h1d.weir_par[3])
             except:
                 pass
         elif self.h1d.idown == 2:
              self.g_wsg.setEnabled(False)
              self.g_abg.setEnabled(False)
              self.g_wehr.setEnabled(False)
              self.normalabfluss.setEnabled(True)
              self.qhtcount.setValue(len(self.h1d.qh_tabelle))
              self.g_qhtab.setRowCount(len(self.h1d.qh_tabelle))
              for _r in range(len(self.h1d.qh_tabelle)):
                  self.g_qhtab.setItem(_r,0,QTableWidgetItem(str(self.h1d.qh_tabelle[_r][0])))
                  self.g_qhtab.setItem(_r,1,QTableWidgetItem(str(self.h1d.qh_tabelle[_r][1])))
    except:
        pass
    
    '''Seitliche Zuflussganglinien'''
    try:
        self.p_latinf.setValue(self.h1d.latinf)
        self.p_lie_table.setRowCount(self.h1d.latinf)
        for wi in range(self.h1d.latinf):
            self.p_lie_table.setItem(wi,0,QTableWidgetItem(str(self.h1d.lie[wi])))
            self.p_lie_table.setItem(wi,6,QTableWidgetItem(str(self.h1d.latcom[wi])))
            try:
                self.p_lie_table.setItem(wi,1,QTableWidgetItem(str(self.h1d.zdat[wi][0:28])))
            except:
                pass
            try:
                self.p_lie_table.setItem(wi,2,QTableWidgetItem(str(self.h1d.zdat[wi][28:30])))
            except:
                pass
            try:
                self.p_lie_table.setItem(wi,3,QTableWidgetItem(str(self.h1d.zdat[wi][30:40])))
            except:
                pass
            try:
                self.p_lie_table.setItem(wi,4,QTableWidgetItem(str(self.h1d.zdat[wi][40:50])))
            except:
                pass
            try:
                self.p_lie_table.setItem(wi,5,QTableWidgetItem(str(self.h1d.zdat[wi][50:60])))
            except:
                pass
    except:
        pass
    
    '''Weirs and gates'''
    try:
        self.p_nweirs.setValue(self.h1d.nweirs)
        self.p_weir_table.setRowCount(self.h1d.nweirs)
        for wi in range(self.h1d.nweirs):
            try:
                for v in range(9):
                    self.p_weir_table.setItem(wi,v,QTableWidgetItem(str(self.h1d.weir_info[wi][0+v*10:10+v*10].strip())))
                self.p_weir_table.setItem(wi,9,QTableWidgetItem(str(self.h1d.weir_info[wi][90:].strip())))
            except:
                pass
    except:
        pass
    try:
        self.p_ngates.setValue(self.h1d.ngates)
        self.p_gate_table.setRowCount(self.h1d.ngates)
        for wi in range(self.h1d.ngates):
            self.p_gate_table.setItem(wi,0,QTableWidgetItem(str(self.h1d.igate[wi])))
            self.p_gate_table.setItem(wi,2,QTableWidgetItem(str(self.h1d.iaga[wi])))
            for n,i in enumerate([1,3,4,5,6,7,8,9]):
                self.p_gate_table.setItem(wi,i,QTableWidgetItem(str(self.h1d.gatdat[wi][n])))
    except:
        pass
    
    '''Geometry Data'''
    try:
        self.p_prodat.setText(self.h1d.prodat)
    except:
        pass
    
    try:
        self.p_slo.setValue(self.h1d.slo)
    except:
        pass
    
    try:
        self.p_xsecmo.setCurrentIndex(['VK','RK','VF','RF'].index(self.h1d.xsecmo))
    except:
        pass
    
    try:
        self.p_startdat.setText(self.h1d.startdat)
    except:
        pass

# =============================================================================
# append rows for value changes
# =============================================================================

def inflowNodes(self):
    self.p_nupe.setRowCount(self.p_nqin.value())
    try:
        for wi in range(self.h1d.nqin):
            self.p_nupe.setItem(wi,0,QTableWidgetItem(str(self.h1d.nupe[wi])))
            self.p_nupe.setItem(wi,1,QTableWidgetItem(str(self.h1d.quinf_[wi])))
            self.p_nupe.setItem(wi,2,QTableWidgetItem(str(self.h1d.timmod[wi])))
            self.p_nupe.setItem(wi,3,QTableWidgetItem(str(self.h1d.faktor[wi])))
    except:
        pass
    
def printCurves(self):
    self.p_kwel_table.setRowCount(self.p_nwel.value())
    try:
        for wi in range(self.h1d.nwel):
            self.p_kwel_table.setItem(wi,0,QTableWidgetItem(str(self.h1d.kwel[wi])))
    except:
        pass

def idown_change(self):
    idx = self.p_idown.currentIndex()
    boo = [False,False,False,False]
    boo[idx] = True
    self.g_wehr.setEnabled(boo[0])
    self.normalabfluss.setEnabled(boo[1])
    self.g_wsg.setEnabled(boo[2])
    self.g_abg.setEnabled(boo[3])
         
def normq(self):
    self.normalabfluss.setRowCount(self.qhtcount.value())
    try:
        for wi in range(len(self.h1d.qh_tabelle)):
            self.normalabfluss.setItem(wi,0,QTableWidgetItem(str(self.h1d.qh_tabelle[wi][0])))
            self.normalabfluss.setItem(wi,1,QTableWidgetItem(str(self.h1d.qh_tabelle[wi][1])))
    except:
        pass
    
def defWeirs(self):
    self.p_weir_table.setRowCount(self.p_nweirs.value())
    try:
        for wi in range(self.h1d.nweirs):
            try:
                for v in range(9):
                    self.p_weir_table.setItem(wi,v,QTableWidgetItem(str(self.h1d.weir_info[wi][0+v*10:10+v*10].strip())))
                self.p_weir_table.setItem(wi,9,QTableWidgetItem(str(self.h1d.weir_info[wi][90:].strip())))
            except:
                pass
    except:
        pass

def defGates(self):
    self.p_gate_table.setRowCount(self.p_ngates.value())
    try:
        for wi in range(self.h1d.ngates):
            self.p_gate_table.setItem(wi,0,QTableWidgetItem(str(self.h1d.igate[wi])))
            self.p_gate_table.setItem(wi,2,QTableWidgetItem(str(self.h1d.iaga[wi])))
            for n,i in enumerate([1,3,4,5,6,7,8,9]):
                self.p_gate_table.setItem(wi,i,QTableWidgetItem(str(self.h1d.gatdat[wi][n])))
    except:
        pass
        
def defJunctions(self):
    self.p_junnam_table.setRowCount(self.p_njunc.value())
    self.p_nxj_table.setRowCount(self.p_njunc.value())
    self.p_dxlgam_table.setRowCount(self.p_njunc.value())
    try:
        for wi in range(self.h1d.njunc):
            self.p_junnam_table.setItem(wi,0,QTableWidgetItem(str(self.h1d.junnam[wi])))
            for ni in range(7):
                self.p_nxj_table.setItem(wi,ni,QTableWidgetItem(str(self.h1d.nxj[wi][ni])))
            self.p_dxlgam_table.setItem(wi,0,QTableWidgetItem(str(self.h1d.dxl[wi])))
            for pi in range(3):
                self.p_dxlgam_table.setItem(wi,pi+1,QTableWidgetItem(str(self.h1d.gam[wi][pi])))
    except:
        pass
    
def lateralInflows(self):
    self.p_lie_table.setRowCount(self.p_latinf.value())
    try:
        for wi in range(self.h1d.latinf):
            self.p_lie_table.setItem(wi,0,QTableWidgetItem(str(self.h1d.lie[wi])))
            self.p_lie_table.setItem(wi,6,QTableWidgetItem(str(self.h1d.latcom[wi])))
            for ni in range(4):
                self.p_lie_table.setItem(wi,1,QTableWidgetItem(str(self.h1d.zdat[wi][0:28])))
                self.p_lie_table.setItem(wi,2,QTableWidgetItem(str(self.h1d.zdat[wi][28:30])))
                self.p_lie_table.setItem(wi,3,QTableWidgetItem(str(self.h1d.zdat[wi][30:40])))
                self.p_lie_table.setItem(wi,4,QTableWidgetItem(str(self.h1d.zdat[wi][40:50])))
                self.p_lie_table.setItem(wi,5,QTableWidgetItem(str(self.h1d.zdat[wi][50:60])))
    except:
        pass

def ovfmode(self,i):
    if self.Edit:
        if i == 0:
            self.ovf_modbox.setEnabled(False)
        elif (i ==1) or (i==2):
            self.ovf_modbox.setEnabled(True)
			
# =============================================================================
# Update Data from input
# =============================================================================
def updateHyd(self):
    '''RUN'''
    self.h1d.hyd_f    = self.p_hyd.text()
    self.h1d.out_f    = self.p_aus.text()
    self.h1d.plot_b   = self.p_plot.currentText()[0]
    self.h1d.format   = self.p_format.currentText()
    self.h1d.achse    = self.p_achse.text()
    self.h1d.ovfbil   = self.p_ovfbil_2.currentText()
    self.h1d.dhzul    = float(self.p_dhzul.text())
    self.h1d.dhrelzul = float(self.p_dhrelzul.text())
    self.h1d.dvrelzul = float(self.p_dvrelzul.text())
    self.h1d.kstmod   = self.p_kstmod.currentText()
    
    '''HYD'''
    self.h1d.stime = float(self.p_stime.text())
    self.h1d.toth  = float(self.p_toth.text())
    self.h1d.dt    = float(self.p_dt.text())
    self.h1d.lead  = int(self.p_lead.text())
    self.h1d.tinc  = float(self.p_tinc.text())
    self.h1d.itun  = self.p_itun.currentIndex()
    self.h1d.ipro  = self.p_ipro.currentIndex()
    self.h1d.nl    = int(self.p_nl.text())
    self.h1d.rain  = self.p_rain.text()
    self.h1d.nret  = self.p_nret.value()
    self.h1d.nstore= self.p_nstore.value()
    self.h1d.tol   = self.p_tol.value()
    self.h1d.convec= float(self.p_convec.text())
    
    '''Zuflüsse'''
    self.h1d.nqin  = self.p_nqin.value()
    self.h1d.nupe    = [None]*self.h1d.nqin
    self.h1d.quinf_  = [None]*self.h1d.nqin
    self.h1d.timmod  = [None]*self.h1d.nqin
    self.h1d.faktor  = [None]*self.h1d.nqin
    
    for wi in range(self.h1d.nqin):
        self.h1d.nupe[wi]   = int(self.p_nupe.item(wi,0).text())
        self.h1d.quinf_[wi] = self.p_nupe.item(wi,1).text()
        self.h1d.timmod[wi] = self.p_nupe.item(wi,2).text()
        self.h1d.faktor[wi] = float(self.p_nupe.item(wi,3).text())

    
    '''ganglinien'''
    self.h1d.list_ = int(self.p_list_.text())
    self.h1d.dtwel = float(self.p_dtwel.text())
    self.h1d.nwel  = self.p_nwel.value()
    self.h1d.kwel  = [None]*self.h1d.nwel
    for wi in range(self.h1d.nwel):
        self.h1d.kwel[wi] = int(self.p_kwel_table.item(wi,0).text())

    '''Junctions'''
    self.h1d.njunc = self.p_njunc.value()
    self.h1d.junnam,self.h1d.nxj,self.h1d.dxl,self.h1d.gam = ([None]*self.h1d.njunc,
                                                                  [None]*self.h1d.njunc,
                                                                  [None]*self.h1d.njunc,
                                                                  [None]*self.h1d.njunc)
    
    for wi in range(self.h1d.njunc):
        self.h1d.junnam[wi] = self.p_junnam_table.item(wi,0).text()
        x = []
        for ni in range(7):
            x.append(int(self.p_nxj_table.item(wi,ni).text()))
        self.h1d.nxj[wi] = x
        self.h1d.dxl[wi] = float(self.p_dxlgam_table.item(wi,0).text())
        x = []
        for pi in range(3):
            x.append(float(self.p_dxlgam_table.item(wi,pi+1).text()))
        self.h1d.gam[wi] = x
        
    '''untere Randbedingungen'''
    self.h1d.idown = self.p_idown.currentIndex() + 1

    if self.h1d.idown == 3:
        self.h1d.rb = self.p_wsg_dat.text() + self.p_wsg_mod.currentText() + '%8.2f' %self.p_wsg_fak.value()

    elif self.h1d.idown == 4:
        self.h1d.rb = self.p_abg_dat.text() + self.p_abg_mod.currentText() + '%8.2f' %self.p_abg_fak.value()

    elif self.h1d.idown == 1:
        self.h1d.weir_par = [self.p_wehr_b.value(),self.p_wehr_k.value(),self.p_wehr_h.value(),self.p_wehr_qm.value()]
        
    elif self.h1d.idown == 2:
        self.h1d.qh_tabelle = []
        for _i in range(self.qhtcount.value()):
            self.h1d.qh_tabelle.append((float(self.g_qhtab.item(_i,0).text()),float(self.g_qhtab.item(_i,1).text())))

    '''Seitliche Zuflussganglinien'''
    self.h1d.latinf = self.p_latinf.value()
    self.h1d.lie    = [0]*self.h1d.latinf
    self.h1d.latcom = [0]*self.h1d.latinf
    self.h1d.zdat   = ['']*self.h1d.latinf
    for wi in range(self.h1d.latinf):
        self.h1d.lie[wi]    = int(self.p_lie_table.item(wi,0).text())
        self.h1d.latcom[wi] = int(self.p_lie_table.item(wi,6).text())
        self.h1d.zdat[wi]   = (self.p_lie_table.item(wi,1).text()+self.p_lie_table.item(wi,2).text()
        +self.p_lie_table.item(wi,3).text()+self.p_lie_table.item(wi,4).text()+self.p_lie_table.item(wi,5).text())
    
    '''Weirs and gates'''
    self.h1d.nweirs        = self.p_nweirs.value()
    self.h1d.weir_info     = ['']*self.h1d.nweirs
    for wi in range(self.h1d.nweirs):
        x = []
        for v in range(9):
            x.append(self.p_weir_table.item(wi,v).text())
        x.append(self.p_weir_table.item(wi,9).text())
        self.h1d.weir_info[wi] = x

    self.h1d.ngates = self.p_ngates.value()
    self.h1d.igate  = [0]*self.h1d.ngates
    self.h1d.iaga   = [0.]*self.h1d.ngates
    self.h1d.gatdat = ['']*self.h1d.ngates
    for wi in range(self.h1d.ngates):
        self.h1d.igate[wi] = int(self.p_gate_table.item(wi,0).text())
        self.h1d.iaga[wi]  = float(self.p_gate_table.item(wi,2).text())
        x = []
        for n,i in enumerate([1,3,4,5,6,7,8,9]):
            x.append(self.p_gate_table.item(wi,i).text())
        self.h1d.gatdat[wi] = x

    '''Geometry Data'''
    self.h1d.prodat   = self.p_prodat.text()
    self.h1d.xsecmo   = self.p_xsecmo.currentText()
    self.h1d.slo      = self.p_slo.value()
    self.h1d.startdat = self.p_startdat.text()