# -*- coding: utf-8 -*-

import os
import numpy as np
import pandas as pd
import json
import re
from PyQt5.QtWidgets import QTableWidgetItem

class HYDRET:
    
    def __init__(self,hydret_path = None):
        if not hydret_path == None:
            with open(hydret_path,'r') as run:
                rlines = run.readlines()
            
            #Pfad fuer die HYD Datei
            hdir   = os.path.dirname(hydret_path)
            self.hyd_f  = rlines[0].strip()
            self.hyd_p  = os.path.join(hdir,self.hyd_f)
            assert os.path.exists(self.hyd_p), '.HYD-Datei nicht gefunden!\nDie Datei musst in dem gleichen Ordner liegen!'
            
            #Ausgabe Pfad
            self.out_f = rlines[1].strip()
            self.out_p = os.path.join(hdir,self.out_f)
            
            self.plot_b = rlines[2][0].upper()
            self.format = rlines[3].upper()
            #Achse Shapefile
            self.achse = rlines[4].strip()
            
            #OVFBIL
            try:
                self.ovfbil = rlines[5].split()[0]
            except:
                self.ovfbil = 'keine'
            
            try:
                self.dhzul,self.dhrelzul,self.dvrelzul = list(map(float, rlines[6].split()[0:3]))
            except:
                pass
                #print('dhzul,dh/dvrelzul nicht lesbar!')
                
            try:
                self.kstmod = rlines[7].split()[0]
            except:
                self.kstmod = 'keine'
            
            self.readHYD(self.hyd_p)
    
    def readFOR(self,varlist,lines,ln):
        
        lncount = tcount = lcount = 0
        while tcount < len(varlist):
            if lcount < len(lines[ln].split()):
                try:
                    varlist[tcount] = float(lines[ln+lncount].split()[lcount])
                except ValueError:
                    print(lines[ln+lncount].split()[lcount])
                tcount +=1
                lcount +=1
            else:
                lncount +=1
                lcount   =0
        return ln+lncount+1,varlist

    def readHYD(self,hyd_p):
        with open(hyd_p,'r') as hd1:
            lines = hd1.readlines()
        
        cdr =  os.path.dirname(hyd_p)
        os.chdir(cdr)
        
        HydDict = {}
        #Project Description
        x = []
        for i in lines[0:4]:
            if i.strip() == '':
                x.append('-')
            else:
                x.append(i.strip())
        self.comments = '\n'.join(x) 
        HydDict['Title'] = self.comments
        
        #Variables
        varl1 = [0]*7
        ln,varl1 = self.readFOR(varl1,lines,4)
        self.stime,self.toth,self.dt,self.lead,self.tinc,self.itun,self.ipro = varl1
        self.itun,self.ipro = int(self.itun),int(self.ipro)
        HydDict['stime'] = self.stime
        HydDict['toth']  = self.toth
        HydDict['dt']    = self.dt
        HydDict['lead']  = self.lead
        HydDict['tinc']  = self.tinc
        HydDict['itun']  = self.itun
        HydDict['ipro']  = self.ipro
        
        varl2 = [0]*8
        ln,varl2 = self.readFOR(varl2,lines,ln)
        self.nl,self.njunc,self.nqin,self.latinf,self.nweirs,self.ngates,self.nret,self.nstore=list(map(int,varl2))
        HydDict['nl'] = self.nl
        
        varl3 = [0]*6
        ln,varl3 = self.readFOR(varl3,lines,ln)
        self.list_ ,self.nwel,self.dtwel,self.idown,self.tol,self.convec=varl3
        self.list_,self.nwel,self.idown = int(self.list_),int(self.nwel),int(self.idown)

        HydDict['list_'] = self.list_
        HydDict['idown']  = self.idown
        HydDict['tol']    = self.tol
        HydDict['convec']  = self.convec
        
        #Hydrographs
        self.kwel = [None]*self.nwel
        ln,self.kwel = self.readFOR(self.kwel,lines,ln)
        self.kwel = list(map(int,self.kwel))
        
        HydDict['Ganglinien'] = {'nwel': self.nwel,
                                 'dtwel': self.dtwel,
                                 'kwel': self.kwel}
        #kstime
        if self.kstmod == 'KSTIME':
            self.kstimedata = lines[ln].strip()
            ln =+1
        
        #Rainfall Data
        self.rain     = lines[ln].strip()
        ln +=1
        HydDict['rain']  = self.rain
        
        #inflow Nodes
        self.nupe     = list(map(int, lines[ln].split()))
        ln +=1
        
        HydDict['inflowNodes']  = {'nqin': self.nqin}
                                   
        self.quinf_,self.timmod,self.faktor = [],[],[]
        self.q_station,self.q_basis = [],[]
        for q in range(int(self.nqin)):
            try:
                self.quinf_.append(lines[ln+q][0:28].strip())
            except:
                self.quinf_.append('')
            try:
                self.timmod.append(lines[ln+q][28:30].strip())
            except:
                self.timmod.append('')
            try:
                self.faktor.append(float(lines[ln+q][30:40]))
            except:
                self.faktor.append('')
            try:
                self.q_station.append(float(lines[ln+q][40:50]))
            except:
                self.q_station.append('')
            try:
                self.q_basis.append(float(lines[ln+q][50:60]))
            except:
                self.q_basis.append('')
        
        for nqi in range(self.nqin):
            HydDict['inflowNodes'][nqi] = (self.nupe[nqi],self.quinf_[nqi],self.timmod[nqi],
                                           self.faktor[nqi],self.q_station[nqi],self.q_basis[nqi])
        #Junctions
        HydDict['Junctions']  = {'njunc': self.njunc}
        ln = ln+self.nqin
        if not self.njunc == 0:
            self.junnam,self.nxj,self.dxl,self.gam = [],[],[],[]
            for ji in range(self.njunc):
                self.junnam.append(lines[ln+ji*2].strip())
                self.nxj.append(list(map(int, lines[ln+ji*2+1].split()[0:7])))
                self.dxl.append(float(lines[ln+ji*2+1].split()[7]))
                self.gam.append(list(map(float, lines[ln+ji*2+1].split()[8:])))

        for nqi in range(self.njunc):
            HydDict['Junctions'][nqi] = (self.junnam[nqi],self.nxj[nqi],self.dxl[nqi],self.gam[nqi])

        #Downstream boundary conditions
        ln = ln+2*self.njunc
        if self.idown == 1:
            self.weir_par = list(map(float,lines[ln].split()))
            HydDict['Randbedingung'] = {'Weir':self.weir_par}
            ln +=1
        elif self.idown == 2:
            self.qh_tabelle = []
            _qhcount = int(lines[ln].split())
            if _qhcount>0:
                ln += 1
                while not len(self.qh_tabelle) == _qhcount:
                    _qh = list(map(float,lines[ln].split()))
                    [self.qh_tabelle.append(_qht) for _qht in list(zip(_qh[::2],_qh[1::2]))]
                    ln +=1
            HydDict['Randbedingung'] = {'Qh-Tabelle':self.qh_tabelle}
        elif (self.idown == 3) or (self.idown == 4):
            self.rb = lines[ln].strip()
            HydDict['Randbedingung'] = {'Datei':self.rb}
            ln += 1
        
        #Lateral Inflow
        lat = 2
        HydDict['latinf'] = {'latinf': self.latinf}
        
        if self.latinf > 0:
            self.naq = int(lines[ln].strip())
            self.gwpar = []
            HydDict['latinf']['naq']   = self.naq
            
            ln +=1
            if self.naq > 1000:
                #code for coupling groundwater data with GWE file format
                self.gwe = lines[ln].strip()
                HydDict['latinf']['gwe'] = self.gwe
                ln +=1
                for k in range(self.naq-1000):
                    ln = ln+k
                    self.gwpar.append(list(map(float,(lines[ln].split()))))
                ln +=1

            elif self.naq > 0:
                for k in range(self.naq):
                    ln = ln +k
                    self.gwpar.append(list(map(float,(lines[ln].split()))))
                ln += 1

            HydDict['latinf']['gwpar'] = self.gwpar
            
            self.lie,self.latcom,self.zdat,self.gangv= [],[],[],[]
            for li in range(self.latinf):
                self.lie.append(int(lines[ln+li*lat].split()[0]))
                try:
                    latcom = int(lines[ln+li*lat].split()[1])
                    self.latcom.append(latcom)
                except:
                    latcom = 0
                    self.latcom.append(0)
                if latcom == 0:
                    try:
                        self.zdat.append(lines[ln+li*lat+1][0:60].strip())
                    except:
                        self.zdat.append(lines[ln+li*lat+1].strip())
                    HydDict['latinf'][li] = (self.lie[li],self.latcom[li],self.zdat[li])
                elif latcom == 2:
                    self.gangv.append(lines[ln+li*lat+1].strip())
                    HydDict['latinf'][li] = (self.lie[li],self.latcom[li],self.gangv[li])
                elif latcom == 1:
                    lat = 1
                    HydDict['latinf'][li] = (self.lie[li],self.latcom[li])

        ln = ln+self.latinf*lat
        
        #weirs
        HydDict['weirs'] = {'nweirs': self.nweirs}
        if self.nweirs > 0:
            self.weir_info = []
            for wi in range(self.nweirs):
                self.weir_info.append(lines[ln+wi])
            for wi in range(self.nweirs):
                HydDict['weirs'][wi] = self.weir_info[wi]

        #Gates
        ln = ln+self.nweirs
        HydDict['gates'] = {'ngates': self.ngates}
        if self.ngates > 0:
            self.igate,self.iaga,self.gatdat = [],[],[]
            for ig in range(self.ngates):
                fl = lines[ln]
                if len(fl.split()) < 3:
                    self.iaga.append(float(fl.split()[-1]))
                    self.igate.append(int(fl.split()[0]))
                    if float(fl.split()[-1]) >= 0:
                        ln +=1
                        sl = lines[ln]
                        self.gatdat.append(['','','','','','','',sl.split()[0]])
                    else:
                        self.gatdat.append(['','','','','','','',''])
                else:
                    mod = fl[0]
                    self.igate.append(int(fl[5:10]))
                    self.iaga.append(float(fl[10:20]))
                    par = list(map(float,fl[20:].split()))
                    for x in range(6):
                        try:
                            par[x]
                        except:
                            par.append('')
                    if float(fl[10:20]) >= 0:
                        ln +=1
                        sl = lines[ln]
                        self.gatdat.append([mod,*par,sl.split()[0]])
                    else:
                        self.gatdat.append([mod,*par,''])
                ln +=1
                HydDict['gates'][ig] = (self.igate[ig],self.iaga[ig],self.gatdat[ig])
        
        #Read channel geometry
        self.slo     = float(lines[ln][:10])
        self.xsecmo  = lines[ln][18:20]
        self.prodat  = lines[ln][20:].strip()
        if '\\' in self.prodat:
            pbroken = self.prodat.split('\\')
            for cdir in pbroken[:-1]:
                os.chdir(cdir)
            pdat = pbroken[-1]
        else:
            pdat = self.prodat
        self.propath = os.path.join(os.getcwd(),pdat)
        assert os.path.exists(self.propath), 'PRO-Datei Pfad Pruefen!'
        os.chdir(cdr)
        
        HydDict['slo'] = self.slo
        HydDict['xsecmo'] = self.xsecmo
        HydDict['prodat'] = self.prodat
        HydDict['propath'] = self.propath
        
        self.readPRO(self.propath)
        ln +=1
        
        #block12
        if self.nl < 0:
            self.startdat = lines[ln].strip()
            HydDict['startdat'] = self.startdat
            if '\\' in self.startdat:
                sbroken = self.startdat.split('\\')
                for cdir in sbroken[:-1]:
                    os.chdir(cdir)
                sdat = sbroken[-1]
            else:
                sdat = self.startdat
            self.startpath = os.path.join(os.getcwd(),sdat)
            HydDict['startpath'] = self.startpath
            assert os.path.exists(self.startpath), ('Start-Datei Pfad existiert nicht!: '+self.startpath)
            os.chdir(cdr)
            ln +=1
#        else:
#            self.startpath = hyd_p
#            self.hyd_startdat = ln
#            ln = ln+self.nl+1
        self.readSTART(self.startpath)
        
        #Retentionsflaeche
        HydDict['Retentionsflaeche'] = {'nret':self.nret,
                                        'nstore':self.nstore}
        if self.nret > 0:
            self.retdat = []
            if self.ovfbil[:3] == 'OVF':
                for nsi in range(self.nstore):
                    self.retdat.append(lines[ln+nsi].split())
                    HydDict['Retentionsflaeche'][nsi] = self.retdat[nsi]
                ln = ln+nsi+1
            else:
                self.nverb = [None]*self.nstore
                ln,self.nverb = self.readFOR(self.nverb,lines,ln)
                self.nverb = list(map(int,self.nverb))
                self.nspe = [[None]*i for i in self.nverb]
                for nsi in range(int(self.nstore)):
                    ln,self.nspe[nsi] = self.readFOR(self.nspe[nsi],lines,ln)
                    self.nspe[nsi] = list(map(int,self.nspe[nsi]))
                    self.retdat.append(lines[ln].split())
                    HydDict['Retentionsflaeche'][nsi] = (self.nspe[nsi],self.retdat[nsi])
                    ln +=1
                    
        self.HydDict = HydDict
#        with open(hyd_p.upper().replace('.HYD','.json'),'w') as jsonf:
#            json.dump(self.HydDict,jsonf,indent=4)

    def readPRO(self,pro_path):
        with open(pro_path,'r') as prof:
            proflines = prof.readlines()
        
        idx = []
        for i,il in enumerate(proflines):
            if 'STATION' in il:
                idx.append(i)
        
        #Dataframe with the size equal to number of indexes with 'Station' found
        df_pro = pd.DataFrame(index=np.arange(0,len(idx)),
                                              columns = ('Node','Npoints','Mode','CTAB',
                                                         'Max Height','Station',
                                                         'PName','X','Y'))
        for n,i in enumerate(idx):        
            Header = proflines[i]    
            
            df_pro.iloc[n]['Node']         = int(  Header[0:5]  )
            df_pro.iloc[n]['Npoints']      = int(  Header[5:10] )
            try:
                df_pro.iloc[n]['CTAB']     = int(  Header[10:13]  )
            except:
                df_pro.iloc[n]['CTAB']     = 0
            if not Header[13:15] == '  ':
                df_pro.iloc[n]['Mode']     =       Header[13:15]
            else:
                try:
                    df_pro.iloc[n]['Mode'] = self.xsecmo
                except:
                    df_pro.iloc[n]['Mode'] = 'RF'
            try: 
                mht = float(Header[15:20])
            except:
                mht = -1
            df_pro.iloc[n]['Max Height']   = mht
            if not Header[29:40].strip() == '':
                df_pro.iloc[n]['Station']      =   Header[29:40].strip()
            else:
                df_pro.iloc[n]['Station']      =   '---'
            if not Header[41:-1].strip() == '':
                df_pro.iloc[n]['PName']        =       Header[41:-1].strip()
            else:
                df_pro.iloc[n]['PName']        =       '---'
        
            if i == idx[-1]: block = proflines[i+1:]
            else: block = proflines[i+1:idx[n+1]]
            
            xy = np.hstack([np.float_(re.split('\s+|,',b.strip())) for b in block])

            df_pro.iloc[n]['X']            = xy[::2]
            df_pro.iloc[n]['Y']            = xy[1::2]

        df_pro.set_index(keys = 'Node',drop=True, inplace= True)
        self.df_pro = df_pro
        return df_pro



    def readSTART(self,start_path):
        
        cols = ["IAB","ITYPE","WIDTH","HEIT","ZTR","ZTL","XL","ZO","RNI","DZERO","HZERO",
                "QZERO","ZS","CKS","HR0","HR1","RNV1","HR2","RNV2","HR3","RNV3","HR4",
                "RNV4","HR5","RNV5","HR6","RNV6","HR7","RNV7","SF","X","Y","ID","ZSHIFT"]
        
        typ =[int,int,float,float,float,float,float,float,float,float,float,float,float,float,
         float,float,float,float,float,float,float,float,float,float,float,float,float,float,
         float,float,float,float,int,float]
        
        form = ['{:4.0f}','{:3.0f}',(*['{:.2f}']*4),'{:.1f}','{:.2f}','{:.4f}',(*['{:.3f}']*3),
                 '{:.2f}',(*['{:.3f}']*2),(*['{:.3f}','{:.4f}']*7),'{:.8f}',(*['{:.2f}']*2),
                 '{:6.0f}','{:.2f}']

        #ovfbil
        if self.ovfbil == 'OVFBIL':
            [cols.insert(n,i)  for n,i in [(14,"LIRE"),(15,"NOVF"),(16,"IABOVF")]]
            [typ.insert(n,i)  for n,i in [(14,int),(15,int),(16,int)]]
            [form.insert(n,i) for n,i in [(14,'{:5.0f}'),(15,'{:5.0f}'),(16,'{:5.0f}')]]
            
        #ovfreq
        elif self.ovfbil == 'OVFREG':
            acol  = [(14,"LIRE"),(15,"NOVF"),(16,"IABOVF"),(17,"OVFAN"),(18,"OVFAUS"),
                     (19,"QOMAX"),(20,"QREGEL"),(21,"TOVFAN"),(22,"TOVFAUS")]
            atyp  = [(14,int),(15,int),(16,int),(17,float),(18,float),(19,float),
                     (20,float),(21,float),(22,float)]
            aform = [(14,'{:5.0f}'),(15,'{:5.0f}'),(16,'{:5.0f}'),(17,'{:.2f}'),(18,'{:.2f}'),(19,'{:.2f}'),
                     (20,'{:.2f}'),(21,'{:.2f}'),(22,'{:.2f}')]
            
            [cols.insert(n,i)  for n,i in acol]
            [typ.insert(n,i)  for n,i in atyp]
            [form.insert(n,i) for n,i in aform]
            

        #kstime
        if self.kstmod == 'KSTIME':
            cols.insert(9,"KRAUT")
            typ.insert(9,int)
            form.insert(9,'{:5.0f}')
        _dtype = dict(zip(cols,typ))
        _dform = dict(zip(cols,form))

        if not start_path.upper().endswith(".HYD"):
            self.df_start = pd.read_csv(start_path,sep=None,skiprows=1,engine='python',
                                        header=None,names=cols,dtype=_dtype)

        else:
            #read block
            self.df_start = pd.read_fwf(start_path,skiprows = self.hyd_start,engine='python',
                            header=None,sep=None,names=cols, dtype=_dtype)
        self._dtype = _dtype
        self._dform = _dform
        self.df_start.set_index('IAB',inplace=True)
        return self.df_start

def renumberHYD(h1d,d_idx,self):
    
    self.p_hyd.setText(self.h1d.hyd_f)
    self.p_aus.setText(self.h1d.out_f)
    self.p_prodat.setText(self.h1d.prodat)
    self.p_startdat.setText(self.h1d.startdat)
    
    #updatehyd
    if h1d.nwel > 0:
        h1d.kwel = [d_idx[i] for i in h1d.kwel]
        for wi in range(h1d.nwel):
            self.p_kwel_table.setItem(wi,0,QTableWidgetItem(str(h1d.kwel[wi])))

    if h1d.nqin > 0:
        h1d.nupe = [d_idx[i] for i in h1d.nupe]
        for wi in range(h1d.nqin):
            self.p_nupe.setItem(wi,0,QTableWidgetItem(str(h1d.nupe[wi])))
    
    if h1d.njunc > 0:
        h1d.nxj = [[d_idx[ii] for ii in i] for i in h1d.nxj]
        for wi in range(self.h1d.njunc):
            for ni in range(7):
                self.p_nxj_table.setItem(wi,ni,QTableWidgetItem(str(h1d.nxj[wi][ni])))

    if h1d.latinf > 0:
        h1d.lie = [d_idx[i] for i in h1d.lie]
        for wi in range(self.h1d.latinf):
            self.p_lie_table.setItem(wi,0,QTableWidgetItem(str(h1d.lie[wi])))
        
    if h1d.nweirs > 0:
        h1d.weir_info = ['%10.0f' %d_idx[int(i[:10])]+i[10:] for i in h1d.weir_info]
        for wi in range(self.h1d.nweirs):
            self.p_weir_table.setItem(wi,0,QTableWidgetItem(str(h1d.weir_info[wi].split()[0])))

    if h1d.ngates > 0:
        h1d.igate = [d_idx[i] for i in h1d.igate]
        for wi in range(self.h1d.ngates):
            self.p_gate_table.setItem(wi,0,QTableWidgetItem(str(h1d.igate[wi])))


def writePRO(ProName,df_pro):
    lines = []
    for i,k in enumerate(df_pro.index):
        
        df   = df_pro.loc[k]
        Node = '%5.0f' %k
        pkts = '%5.0f' %df['Npoints']
        if df['CTAB'] == 0: ctab = '   '
        else: ctab = ' 1 '
        mxht = '%5.1f' %df['Max Height']
        stn  = '%10.1f' %float(df['Station'])
        header = Node + pkts + ctab +df['Mode'] +mxht+ 'STATION :' + stn+' M '+'{:>30}'.format(df['PName'])+'\n'
        lines.append(header)
        coords = list(zip(df['X'],df['Y']))

        an = 0
        while an < len(coords):
            xy_str = ''
            cn = 0
            while cn < 4:
                try:
                    xy_str = xy_str +'%8.2f' %coords[an+cn][0]+'%8.2f' %coords[an+cn][1]+'  '
                except:
                    pass
                cn = cn+1
            xy_str = xy_str + '\n'
            an = an+4
            lines.append(xy_str)
    
    with open(ProName,'w') as out_f:
        for i in lines:
            out_f.write(i)

def writeStart(StartName,df_start,d):
    df = df_start.copy()
    df.reset_index(inplace=True)
    for key,item in d.items():
        df[key]    = df[key].apply(func = item.format, axis=1)
    df.to_csv(StartName,sep=',',index=False)
                
def writeHYD(hydName,h1d):
    
    lines = []
        
    #Description
    com = h1d.comments.split('\n\n')
    for c in com:
        lines.append(c+'\n')
   
    #Steuerdaten
    lines.append((' {:>10}'*7+'\n').format(h1d.stime,h1d.toth,h1d.dt,h1d.lead,
                                     h1d.tinc,h1d.itun,h1d.ipro))
    lines.append((' {:>10}'*8+'\n').format(h1d.nl,h1d.njunc,h1d.nqin,h1d.latinf,
                                     h1d.nweirs,h1d.ngates,h1d.nret,h1d.nstore))
    lines.append((' {:>10}'*6+'\n').format(h1d.list_,h1d.nwel,h1d.dtwel,h1d.idown,
                                     h1d.tol,h1d.convec))
    lines.append((' {:>6}'*h1d.nwel+'\n').format(*h1d.kwel))

#lines.append(('{:>10}'*7+''*20+
#              ('Simulation: {Startzeit, Gesamtzeit, Zeitschritt}, Gang-stützstellen,Gang-stützweite, Uhrzeit Schalter, Vorschunsteureung Schalter\n')).format(h1d.stime,h1d.toth,h1d.dt,h1d.lead,
#                                 h1d.tinc,h1d.itun,h1d.ipro))
#lines.append(('{:>10}'*8+''*10+'\n').format(h1d.nl,h1d.njunc,h1d.nqin,h1d.latinf,
#                                 h1d.nweirs,h1d.ngates,h1d.nret,h1d.nstore))
#lines.append(('{:>10}'*6+''*30+'\n').format(h1d.list_,h1d.nwel,h1d.dtwel,h1d.idown,
#                                 h1d.tol,h1d.convec))
#lines.append(('{:>6}'*h1d.nwel+'\n').format(*h1d.kwel))
    #Niederschlagsdaten
    lines.append(h1d.rain+'\n')
    
    #Zuflüsse von oberhalb
    lines.append(('{:10}'*h1d.nqin+'\n').format(*h1d.nupe))
    for i in range(h1d.nqin):
        lines.append('{:28}{:2}{:10}{:10}{:10}\n'.format(h1d.quinf_[i],h1d.timmod[i],h1d.faktor[i],
                                                       h1d.q_station[i],h1d.q_basis[i]))
    
    #Junctions
    for i in range(h1d.njunc):
        lines.append(h1d.junnam[i]+'\n')
        lines.append(('{:5}'*7+' '*5+'{:10.3f}'*4+'\n').format(*h1d.nxj[i],h1d.dxl[i],*h1d.gam[i]))
        
    #Boundary Conditions
    if h1d.idown == 1:
        lines.append(('{:10.4f}'*len(h1d.weir_par)+'\n').format(*h1d.weir_par))
        
    elif h1d.idown == 2:
        lines.append('{:5}\n'.format(len(h1d.qh_tabelle)))
        for _i in range(0,len(h1d.qh_tabelle),4):
            lines.append(('{:10.2f}'*4+'\n').format(*h1d.qh_tabelle[_i:_i+4]))
        
    elif (h1d.idown == 3) or (h1d.idown == 4):
        lines.append(h1d.rb+'\n')
        
    #GW-Parameter
    if h1d.latinf > 0:
        lines.append('{:4}\n'.format(h1d.naq))
        for i in range(h1d.naq):
            try:
                lines.append(('{:8.2f}'*2+'{:8.5f}\n').format(*h1d.gwpar))
            except:
                lines.append(('{:8.2f}'*2+'{:8.5f}\n').format(1,1,1))
    
        #Lateral Inflow
        for i in range(h1d.latinf):
            lines.append(('{:4}'*2+'\n').format(h1d.lie[i],h1d.latcom[i]))
            if h1d.latcom[i] == 0:
                lines.append(h1d.zdat[i]+'\n')
            elif h1d.latcom[i] == 2:
                lines.append(h1d.gangv[i]+'\n')
                
    #weirs
    if h1d.nweirs >0:
        for i in range(h1d.nweirs):
            lines.append(('{:>10}'*9+'{:10}\n').format(*h1d.weir_info[i]))
    
    #gates
    if h1d.ngates >0:
        for i in range(h1d.ngates):
            lines.append(('{:1}{:9}'+'{:>10}'*7+'\n').format(h1d.gatdat[i][0],h1d.igate[i],
                                                   h1d.iaga[i],*h1d.gatdat[i][1:-1]))
            if h1d.iaga[i] >=0:
                lines.append(h1d.gatdat[i][-1]+'\n')
    
    #pro dat
    lines.append('{:10}{:>10}{:10}{:30}\n'.format(h1d.slo,h1d.xsecmo,'',h1d.prodat))
    
    #startdat
    if h1d.nl <0:
        lines.append(h1d.startdat+'\n')
            
    with open(hydName,'w') as outf:
        for i in lines:
            outf.write(i)
            
def writeRUN(run,h1d):
    lines = []
    
    lines.append(h1d.hyd_f + '\n')
    lines.append(h1d.out_f + '\n')
    lines.append(h1d.plot_b+ '\n')
    lines.append(h1d.format+ '\n')
    lines.append(h1d.achse + '\n')
    if h1d.ovfbil == 'Aus':
        lines.append('kein              #OVFBIL\n')
    else:
         lines.append(h1d.ovfbil+' '*10+'#OVFBIL\n')
    if not h1d.dhzul == '':
        lines.append('{:0.3f}  {:0.3f}  {:0.3f}   DHZUL Mindestwert, DHRELZUL bezogen auf Wassertiefe,DVRELZUL\n'.format(h1d.dhzul,h1d.dhrelzul,h1d.dvrelzul))
        if h1d.kstmod == 'Aus':
            lines.append('Kein         Verkrautung')
        else:
            lines.append('KSTIME         Verkrautung')
    
    with open(run,'w') as out:
        for i in lines:
            out.write(i)
            
    mod = run[:-4]
    with open(run.replace('.run','.bat'),'w') as bat:
        bat.write('COPY '+mod+'.run HYDRET06.run\n'+
                  'hydret06\n'+
                  'COPY '+mod+'_Start.dat '+ mod+'_Start_bak.dat\n'+
                  'COPY Start.dat '+mod+'_Start.dat\n'+
                  'COPY WSP.dat '+mod+'_WSP.dat\n'+
                  'COPY WSP.PLT '+mod+'.dat\n'+
                  'COPY WSP.PLT '+mod+'.WSP\n'+
                  'COPY syst.plt '+mod+'.SYS\n'+
                  'COPY HCH.plt '+mod+'.HCH\n'+
                  'COPY QCH.plt '+mod+'.QCH\n'+
                  'DEL *.plt')
    