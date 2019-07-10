# -*- coding: utf-8 -*-

import os
import numpy as np
import pandas as pd
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
            self.format = rlines[3][0].upper()
            #Achse Shapefile
            self.achse = rlines[4].strip()
            
            #OVFBIL
            self.ovfbil = rlines[5].split()[0]
            
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
            
    def readHYD(self,hyd_p):
        with open(hyd_p,'r') as hd1:
            lines = hd1.readlines()
        
        cdr =  os.path.dirname(hyd_p)
        os.chdir(cdr)
        #block1
        x = []
        for i in lines[0:4]:
            if i.strip() == '':
                x.append('-')
            else:
                x.append(i.strip())
        self.comments = '\n'.join(x) 
       
        #block2
        self.stime    = float(lines[4].split()[0]) #in hours
        self.toth     = float(lines[4].split()[1]) #in hours
        self.dt       = float(lines[4].split()[2]) #in mins
        self.lead     = int(lines[4].split()[3])
        self.tinc     = float(lines[4].split()[4])
        self.itun     = int(lines[4].split()[5])
        self.ipro     = int(lines[4].split()[6])
        
        self.nl       = int(lines[5].split()[0])
        self.njunc    = int(lines[5].split()[1])
        self.nqin    = int(lines[5].split()[2])
        self.latinf   = int(lines[5].split()[3])
        self.nweirs   = int(lines[5].split()[4])
        self.ngates   = int(lines[5].split()[5])
        self.nret     = int(lines[5].split()[6])
        self.nstore   = int(lines[5].split()[7])
        
        self.list_    = int(lines[6].split()[0])
        self.nwel     = int(lines[6].split()[1])
        self.dtwel    = float(lines[6].split()[2]) #in mins
        self.idown    = int(lines[6].split()[3])
        self.tol      = float(lines[6].split()[4])
        self.convec   = float(lines[6].split()[5])
        
        self.kwel     = list(map(int, lines[7].split()))
        
        #block 3
        self.rain     = lines[8].strip()
        
        #block 4
        self.nupe     = list(map(int, lines[9].split()))
        
        self.quinf_,self.timmod,self.faktor = [],[],[]
        self.q_station,self.q_basis = [],[]
        for q in range(self.nqin):
            self.quinf_.append(lines[10+q][0:28].strip())
            self.timmod.append(lines[10+q][28:30].strip())
            self.faktor.append(float(lines[10+q][30:40]))
            try:
                self.q_station.append(float(lines[10+q][40:50]))
                self.q_basis.append(float(lines[10+q][50:60]))
            except:
                self.q_station.append('')
                self.q_basis.append('')
            
        #block 5
        b5 = 10+self.nqin
        if not self.njunc == 0:
            self.junnam,self.nxj,self.dxl,self.gam = [],[],[],[]
            for ji in range(self.njunc):
                self.junnam.append(lines[b5+ji*2].strip())
                self.nxj.append(list(map(int, lines[b5+ji*2+1].split()[0:7])))
                self.dxl.append(float(lines[b5+ji*2+1].split()[7]))
                self.gam.append(list(map(float, lines[b5+ji*2+1].split()[8:])))
                
        #block 6
        b6 = b5+2*self.njunc
        if self.idown == 1:
            self.weir_par = list(map(float,lines[b6].split()))
            b6 = b6+1
        if (self.idown == 3) or (self.idown == 4):
            self.rb = lines[b6].strip()
            b6 = b6+1
        
        #block7
        b7 = b6
        if not self.latinf == 0:
            self.naq = int(lines[b7].strip())
            if self.naq>0:
                self.gwpar = []
                for k in range(self.naq):
                    self.gwpar.append(list(map(float,(lines[b7+1+k].split()))))
            b7 = b7+self.naq+1
        
        #block 8
        b8 = b7
        if not self.latinf == 0:
            self.lie,self.latcom,self.zdat,self.gangv= [],[],[],[]
            for li in range(self.latinf):

                self.lie.append(int(lines[b8+li*2].split()[0]))
                try:
                    latcom = int(lines[b8+li*2].split()[1])
                    self.latcom.append(latcom)
                except:
                    latcom = 0
                    self.latcom.append(0)
                if latcom == 0:
                    try:
                        self.zdat.append(lines[b8+li*2+1][0:60].strip())
                    except:
                        self.zdat.append(lines[b8+li*2+1].strip())
                elif latcom == 2:
                    self.gangv.append(lines[b8+li*2+1].strip())
                    
        #block 9
        b9 = b8+self.latinf*2
        if not self.nweirs == 0:
            self.weir_info = []
            for wi in range(self.nweirs):
                self.weir_info.append(lines[b9+wi])

        #block 10
        b10 = b9+self.nweirs
        if not self.ngates == 0:
            self.igate,self.iaga,self.gatdat = [],[],[]
            for ig in range(self.ngates):
                fl = lines[b10+ig*2]
                sl = lines[b10+ig*2 +1]
                if len(fl.split()) < 3:
                    self.iaga.append(float(fl[-1]))
                    self.igate.append(int(fl[0]))
                    if float(fl[-1]) >= 0:
                        gi = 2
                        self.gatdat.append(['','','','','','','',sl.split()[0]])
                    else:
                        gi = 1
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
                        gi=2
                        self.gatdat.append([mod,*par,sl.split()[0]])
                    else:
                        gi = 1
                        self.gatdat.append([mod,*par,''])
            b10 = b10+self.ngates*gi
        
        #block 11
        b11         = b10
        self.slo         = float(lines[b11][:18])
        self.xsecmo      = lines[b11][18:20]
        self.prodat = lines[b11][20:].strip()
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
        self.readPRO(self.propath)
        
        #block12
        b12      = b11+1
        if self.nl < 0:
            self.startdat = lines[b12].strip()

            if '\\' in self.startdat:
                sbroken = self.startdat.split('\\')
                for cdir in sbroken[:-1]:
                    os.chdir(cdir)
                sdat = sbroken[-1]
            else:
                sdat = self.startdat
            self.startpath = os.path.join(os.getcwd(),sdat)
            assert os.path.exists(self.startpath), 'Start-Datei Pfad Pruefen!'
            os.chdir(cdr)
            self.readSTART(self.startpath)
            b12 = b12+1
        else:
            b12 = b12+self.nl+1
        #TODO:- Block 13, retentionsflaeche
    

            
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
            
            xy = np.hstack([np.float_(b.split()) for b in block])

            df_pro.iloc[n]['X']            = xy[::2]
            df_pro.iloc[n]['Y']            = xy[1::2]

        df_pro.set_index(keys = 'Node',drop=True, inplace= True)
        self.df_pro = df_pro
        return df_pro



    def readSTART(self,start_path):
        
        try:
            self.df_start = pd.read_csv(start_path,sep=',',index_col = 0,header=0)
            try:
                self.df_start = self.df_start.astype({'ID':int,'ITYPE':int})
            except:
                pass
            return self.df_start
        except:
            raise ValueError('Start Format Pruefen ob Header vorhanden ist')

def renumberHYD(h1d,d_idx,myapp):
    
    #updatehyd
    if h1d.nwel > 0:
        h1d.kwel = [d_idx[i] for i in h1d.kwel]
        for wi in range(h1d.nwel):
            myapp.p_kwel_table.setItem(wi,0,QTableWidgetItem(str(h1d.kwel[wi])))

    if h1d.nqin > 0:
        h1d.nupe = [d_idx[i] for i in h1d.nupe]
        for wi in range(h1d.nqin):
            myapp.p_nupe.setItem(wi,0,QTableWidgetItem(str(h1d.nupe[wi])))
    
    if h1d.njunc > 0:
        h1d.nxj = [[d_idx[ii] for ii in i] for i in h1d.nxj]
        for wi in range(myapp.h1d.njunc):
            for ni in range(7):
                myapp.p_nxj_table.setItem(wi,ni,QTableWidgetItem(str(h1d.nxj[wi][ni])))

    if h1d.latinf > 0:
        h1d.lie = [d_idx[i] for i in h1d.lie]
        for wi in range(myapp.h1d.latinf):
            myapp.p_lie_table.setItem(wi,0,QTableWidgetItem(str(h1d.lie[wi])))
        
    if h1d.nweirs > 0:
        h1d.weir_info = ['%10.0f' %d_idx[int(i[:10])]+i[10:] for i in h1d.weir_info]
        for wi in range(myapp.h1d.nweirs):
            myapp.p_weir_table.setItem(wi,0,QTableWidgetItem(str(h1d.weir_info[wi].split()[0])))

    if h1d.ngates > 0:
        h1d.igate = [d_idx[i] for i in h1d.igate]
        for wi in range(myapp.h1d.ngates):
            myapp.p_gate_table.setItem(wi,0,QTableWidgetItem(str(h1d.igate[wi])))

    return h1d
    
    
    
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

def writeStart(StartName,df_start):
    df_start.to_csv(StartName)
                
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
    