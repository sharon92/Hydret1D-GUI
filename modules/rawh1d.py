# -*- coding: utf-8 -*-

import os
import copy
#import logging
import numpy  as np
import pandas as pd
#import json
import re
import shapefile           as     shp
from   shapely.geometry    import LineString

class HYDRET:
    
    def __init__(self,hydret_path = None):
        if not hydret_path == None:
            with open(hydret_path,'r') as run:
                rlines = run.readlines()
            
            #Pfad fuer die HYD Datei
            hdir   = os.path.dirname(hydret_path)
            self.hyd_f  = rlines[0].strip()
            self.hyd_p  = os.path.abspath(os.path.join(hdir,self.hyd_f))
            assert os.path.exists(self.hyd_p), '.HYD-Datei nicht gefunden!\nDie Datei musst in dem gleichen Ordner liegen!'
            meldung = '.HYD-Datei nicht gefunden!\nDie Datei musst in dem gleichen Ordner liegen!\n'
            
            #Ausgabe Pfad
            self.out_f = rlines[1].strip()
            self.out_p = os.path.join(hdir,self.out_f)
            
            self.plot_b = rlines[2][0].upper()
            self.format = rlines[3].upper()
            
            #Achse Shapefile
            if os.path.isfile(rlines[4].strip()):
                self.achse = os.path.abspath(rlines[4].strip())
            else:
                self.achse = None
                meldung = meldung+'Gewässerachse ungültig oder nicht verfügbar\n'
#                self.statusbar.showMessage('Gewässerachse ungültig oder nicht verfügbar')
           
            #OVFBIL
            try: self.ovfbil = rlines[5].split()[0]
            except:self.ovfbil = 'keine'
            
            try:self.dhzul,self.dhrelzul,self.dvrelzul = list(map(float, rlines[6].split()[0:3]))
            except:pass
                
            try:self.kstmod = rlines[7].split()[0]
            except:self.kstmod = 'keine'
            
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
        
        with open(hyd_p,'r') as hd1: lines = hd1.readlines()
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
        self.nupe    = [0]*self.nqin 
        ln,self.nupe = self.readFOR(self.nupe,lines,ln)
        self.nupe    = list(map(int,self.nupe))
        HydDict['inflowNodes']  = {'nqin': self.nqin}
                                   
        self.quinf_,self.timmod,self.faktor = [],[],[]
        self.q_station,self.q_basis = [],[]
        
        for q in range(int(self.nqin)):
            try:self.quinf_.append(lines[ln+q][0:28].strip())
            except:self.quinf_.append('')
            
            try:self.timmod.append(lines[ln+q][28:30].strip())
            except:self.timmod.append('')
            
            try:self.faktor.append(float(lines[ln+q][30:40]))
            except:self.faktor.append(1)
            
            try:self.q_station.append(float(lines[ln+q][40:50]))
            except:self.q_station.append('')
            
            try:self.q_basis.append(float(lines[ln+q][50:60]))
            except:self.q_basis.append(0)
        
        for nqi in range(self.nqin):
            HydDict['inflowNodes'][nqi] = (self.nupe[nqi],self.quinf_[nqi],self.timmod[nqi],
                                           self.faktor[nqi],self.q_station[nqi],self.q_basis[nqi])
        #Junctions
        HydDict['Junctions']  = {'njunc': self.njunc}
        ln = ln+self.nqin
        self.junnam,self.nxj,self.dxl,self.gam = [],[],[],[]
        if not self.njunc == 0:
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
            _qhcount = int(lines[ln].split()[0])
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
        
        self.gwpar = []
        self.lie,self.latcom,self.zdat,self.gangv= [],[],[],[]
        if self.latinf > 0:
            self.naq = int(lines[ln].strip())
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
            for li in range(self.latinf):
                self.lie.append(int(lines[ln+li*lat].split()[0]))
                try:
                    latcom = int(lines[ln+li*lat].split()[1])
                    self.latcom.append(latcom)
                except:
                    latcom = 0
                    self.latcom.append(0)
                if latcom == 0:
                    try:self.zdat.append(lines[ln+li*lat+1][0:60].strip())
                    except:self.zdat.append(lines[ln+li*lat+1].strip())
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
        self.weir_info = []
        if self.nweirs > 0:
            for wi in range(self.nweirs): self.weir_info.append(lines[ln+wi])
            for wi in range(self.nweirs): HydDict['weirs'][wi] = self.weir_info[wi]

        #Gates
        ln = ln+self.nweirs
        HydDict['gates'] = {'ngates': self.ngates}
        self.igate,self.iaga,self.gatdat = [],[],[]
        if self.ngates > 0:
            for ig in range(self.ngates):
                fl = lines[ln]
                if len(fl) < 90: fl = fl + ' '*(90-len(fl))
                self.igate  += [int(fl[5:10])]
                self.iaga   += [float(fl[10:20])]
                gatdat       = [fl[0:4].strip(),fl[20:30].strip(),
                                fl[30:40].strip(),fl[40:50].strip(),
                                fl[50:60].strip(),fl[60:70].strip(),
                                fl[70:80].strip(),fl[80:85].strip(),
                                fl[85:90].strip()]
                ln +=1
                if not float(fl[10:20]) < 0:
                    if len(lines[ln].strip().split()) < 4:
                        self.gatdat += [[*gatdat,*lines[ln].strip().split(),*['']*(4-len(lines[ln].strip().split()))]]
                    else:
                        self.gatdat += [[*gatdat,*lines[ln].strip().split()]]
                    ln +=1
                else:
                    self.gatdat += [[*gatdat,*['']*4]]
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
            self.readSTART(self.startpath,skiprows=None)
        else:
            self.startdat = self.hyd_f
            self.startpath = hyd_p
            self.hyd_startdat = ln
            ln = ln+self.nl+1
            self.readSTART(self.startpath,skiprows=self.hyd_startdat)
        
        self.df_nodes = self.determineNodePlan()
        
        #Retentionsflaeche
        HydDict['Retentionsflaeche'] = {'nret':self.nret,
                                        'nstore':self.nstore}
        self.retdat,self.nverb,self.nspe =[], [],[]
        if self.nret > 0:
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
        with open(pro_path,'r') as prof: proflines = prof.readlines()
        
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
            df_pro.iloc[n]['Max Height']       = mht
            if not Header[29:40].strip() == '':
                df_pro.iloc[n]['Station']      =   Header[29:40]
            else:
                df_pro.iloc[n]['Station']      =   '---'
            if not Header[41:-1].strip() == '':
                df_pro.iloc[n]['PName']        =   Header[41:-1]
            else:
                df_pro.iloc[n]['PName']        =   '---'
        
            if i == idx[-1]: block = proflines[i+1:]
            else: block = proflines[i+1:idx[n+1]]
            
            if df_pro.iloc[n]['Npoints'] > 0:
                xy = np.hstack([np.float_(re.split('\s+|,',b.strip())) for b in block])
    
                df_pro.iloc[n]['X']            = xy[::2]
                df_pro.iloc[n]['Y']            = xy[1::2]

        df_pro.set_index(keys = 'Node',drop=True, inplace= True)
        self.df_pro = df_pro
        return df_pro



    def readSTART(self,start_path,skiprows=None):
        
        cols = ["IAB","ITYPE","WIDTH","HEIT","ZTR","ZTL","XL","ZO","RNI","DZERO","HZERO",
                "QZERO","ZS","CKS","HR0","HR1","RNV1","HR2","RNV2","HR3","RNV3","HR4",
                "RNV4","HR5","RNV5","HR6","RNV6","HR7","RNV7","SF","X","Y","ID","ZSHIFT"]

        typ =[int,int,float,float,float,float,float,float,float,float,float,float,float,float,
         float,float,float,float,float,float,float,float,float,float,float,float,float,float,
         float,float,float,float,int,float]
        
        form = ['{:4.0f}','{:3.0f}',(*['{:.2f}']*4),'{:.1f}','{:.2f}','{:.4f}',(*['{:.3f}']*3),
                 '{:.2f}',(*['{:.3f}']*2),(*['{:.3f}','{:.4f}']*7),'{:.8f}',(*['{:.2f}']*2),
                 '{:6.0f}','{:.2f}']

        c_bil = [(14,"LIRE"),(15,"NOVF"),(16,"IABOVF")]
        t_bil = [(14,int),(15,int),(16,int)]
        f_bil = [(14,'{:5.0f}'),(15,'{:5.0f}'),(16,'{:5.0f}')]

        acol  = [(14,"LIRE"),(15,"NOVF"),(16,"IABOVF"),(17,"OVFAN"),(18,"OVFAUS"),
                 (19,"QOMAX"),(20,"QREGEL"),(21,"TOVFAN"),(22,"TOVFAUS")]
        atyp  = [(14,int),(15,int),(16,int),(17,float),(18,float),(19,float),
                 (20,float),(21,float),(22,float)]
        aform = [(14,'{:5.0f}'),(15,'{:5.0f}'),(16,'{:5.0f}'),(17,'{:.2f}'),(18,'{:.2f}'),(19,'{:.2f}'),
                 (20,'{:.2f}'),(21,'{:.2f}'),(22,'{:.2f}')]
        
        mainc =copy.deepcopy(cols)
        maint =copy.deepcopy(typ)
        maind =copy.deepcopy(form)
        [mainc.insert(n,i)  for n,i in c_bil]
        [maint.insert(n,i)  for n,i in t_bil]
        [maind.insert(n,i) for n,i in f_bil]
        [mainc.insert(n,i)  for n,i in acol]
        [maint.insert(n,i)  for n,i in atyp]
        [maind.insert(n,i) for n,i in aform]
        mainc.insert(9,"KRAUT")
        maint.insert(9,int)
        maind.insert(9,'{:5.0f}')
        main_dtype = dict(zip(mainc,maint))
        main_dform = dict(zip(mainc,maind))
        _dtype,_dform = {},{}
        
        if all(hasattr(self,attr) for attr in ['ovfbil','kstmod']):
            #ovfbil
            if self.ovfbil == 'OVFBIL':
                [cols.insert(n,i)  for n,i in c_bil]
                [typ.insert(n,i)  for n,i in t_bil]
                [form.insert(n,i) for n,i in f_bil]

            #ovfreq
            elif self.ovfbil == 'OVFREG':
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
        
        else:
            with open(start_path,'r') as sf: header = sf.readline().strip().replace('"','')
            if ',' in header: cols = header.split(',')
            else: cols = header.split()
            for c in cols: 
                _dtype[c] = main_dtype[c]
                _dform[c] = main_dform[c]
                
        if not start_path.upper().endswith(".HYD"):
            self.df_start = pd.read_csv(start_path,sep=None,skiprows=1,engine='python',
                                        header=None,names=cols,dtype=_dtype)
    
        else:
            #read block
            try:
                self.df_start = pd.read_fwf(start_path,skiprows = skiprows,engine='python',
                            header=None,sep=None,names=cols, dtype=_dtype)
            except:
                try:
                    self.df_start = pd.read_fwf(start_path,skiprows = skiprows,engine='python',
                    header=None,sep=None,names=cols)
                except:
                    self.statusbar.showMessage('Ungültige Start-Datei! '+start_path)
        
        nXL  = self.df_start.XL.values 
        nID  = self.df_start.ID
        nX,nY = self.df_start.X.values, self.df_start.Y.values
        diff = nXL[1:] - nXL[:-1]
        ids  = [0,*(np.where(diff>0)[0]+1).tolist(),len(nXL)]
        if len(nID.unique()) <= len(ids)-1:
            id_array = nID.values
            for neid,eid in enumerate(ids[:-1]):
                start,end = eid,ids[neid+1]
                nidx      = self.df_start[start:end]
                if nidx.ID.unique()[0] <= 0:idx = -1*(neid+1)
                else: idx = nidx.ID.unique()[0]
                id_array[start:end]= np.full(len(nXL[start:end]),idx)
                self.df_start.ID = id_array
                
        self._dtype = _dtype
        self._dform = _dform
        self.df_start.set_index('IAB',inplace=True)
        self.naughty_list=[]
        if hasattr(self,'achse'):
            if self.achse is not None:
                rshp  = shp.Reader(self.achse)
                rshp.encodingErrors = 'ignore'
                s_gid,s_start = [],[]
                for n,i in enumerate(rshp.records()):
                    s_gid.append(i['GEW_ID'])
                    s_start.append(i['SSTART'])  #offset from starting point of the polyline
                
                for gid in nID.unique():
                    if not gid in s_gid: self.naughty_list.append(gid)
                    else:
                        idx      = s_gid.index(gid)
                        offset   = s_start[idx]
                        rec      = rshp.shapeRecords()[idx]
                        line     = LineString(rec.shape.points)
                        nidx     = np.where(nID == gid)
                        seg      = nXL[nidx]-offset
                        nX[nidx] = [line.interpolate(seg_i).x for seg_i in seg]
                        nY[nidx] = [line.interpolate(seg_i).y for seg_i in seg]
                        
                self.df_start.X,self.df_start.Y = nX, nY
        return self.df_start
    
    def determineNodePlan(self):
        df = self.df_start
        if not self.njunc > 0:
            d = {'IAB'   : [*df.index,],
                 'ID'    : [*df.ID,], 
                 'ITYPE' : [*df.ITYPE,],
                 'X'     : [*np.full(len(df),0)],
                 'Y'     : [*df.XL,]}
        else:
            channels = []
            self.channelIDs ={}
            for j in range(self.njunc):
                n_j = self.nxj[j]
                d_j = self.dxl[j]
                i_0 = df.loc[n_j[0]].ID
                i_2 = df.loc[n_j[2]].ID
                i_3 = df.loc[n_j[3]].ID
                if i_2 == i_3:
                    channels.append(i_2)
                    if not i_0 in self.channelIDs.keys(): self.channelIDs[i_0] = [(i_2,d_j)]
                    else: self.channelIDs[i_0].append((i_2,d_j))
                else:
                    channels.append(i_2)
                    channels.append(i_3)
                    if not i_0 in self.channelIDs.keys(): self.channelIDs[i_0] = [(i_2,d_j)]
                    else: self.channelIDs[i_0].append((i_2,d_j))
                    if not i_2 in self.channelIDs.keys(): self.channelIDs[i_2] = [(i_3,df[df.ID == i_2].XL.max())]
                    else: self.channelIDs[i_2].append((i_3,df[df.ID == i_2].XL.max()))
            ids = list(df.ID.unique())
            for i in channels: ids.remove(i)
            
            if not len(ids) == 1: return None
            
            self.mainChannelID = ids[0]
            md = df[df.ID == self.mainChannelID]
            d  = {'IAB'   : [*md.index,],
                  'ID'    : [*md.ID,], 
                  'ITYPE' : [*md.ITYPE,],
                  'X'     : [*np.full(len(md),0)],
                  'Y'     : [*md.XL,]}
            ID_defined = [self.mainChannelID]
            xval = {}
            yval = {}
            xval[self.mainChannelID] = 0
            for n,(ID,di) in enumerate(self.channelIDs[self.mainChannelID]):
                ID_defined.append(ID)
                c = df[df.ID == ID]
                xval[ID] = (n+1)*1000
                yval[ID] = di
                for iab in [*c.index.values,-c.index.values[-1]]: d['IAB'].append(iab) 
                for idd in [ID]*(len(c)+1)                      : d['ID'].append(idd)
                for ity in [*c.ITYPE.values,-4]                 : d['ITYPE'].append(ity)
                for x   in [*np.full(len(c),xval[ID]),xval[self.mainChannelID]]: d['X'].append(x)
                for y   in [*c.XL.values+di,di]                 : d['Y'].append(y) 
            del self.channelIDs[self.mainChannelID]
            
            while len(self.channelIDs.keys()) > 0:
                before = len(self.channelIDs.keys())
                for k,id_list in self.channelIDs.copy().items():
                    if k in ID_defined:
                        for n,(ID,di) in enumerate(id_list):
                            ID_defined.append(ID)
                            c = df[df.ID == ID]
                            xval[ID] = xval[k] + (n+1)*100
                            yval[ID] = yval[k] + di
                            for iab in [*c.index.values,-c.index.values[-1]]: d['IAB'].append(iab) 
                            for idd in [ID]*(len(c)+1)                      : d['ID'].append(idd)
                            for ity in [*c.ITYPE.values,-4]                 : d['ITYPE'].append(ity)
                            for x   in [*np.full(len(c),xval[ID]),xval[k]]  : d['X'].append(x)
                            for y   in [*c.XL.values+yval[ID],yval[ID]]     : d['Y'].append(y) 
                        del self.channelIDs[k]
                after = len(self.channelIDs.keys())
                if before == after: break
        ndf = pd.DataFrame(d)
        ndf.set_index('IAB',inplace=True)
        return ndf

def readHQ(self,file, form='B'):
    
    Data = {}
    
    #Unformatted Binär
    if form in ['B','b']:
        floats = np.fromfile(file,dtype=np.float32)
        ints   = np.fromfile(file,dtype=np.int32)
        
        nl = ints[1]
        
        if hasattr(self,'h1d'):
            if not nl == abs(self.h1d.nl)+1 :
                self.statusbar.showMessage('Anzahl der Knoten in Binär-Datei stimmt nicht mit Hyd-Datei!')
        
        knoten = ints[7:7+nl*2:2]
        idx1   = np.arange(7,7+nl*2,2)
        
        sts    = int((len(ints)-3)/(nl*2+5))
        if hasattr(self,'h1d'):
            if not sts == self.h1d.lead :
                self.statusbar.showMessage('Anzahl der Stützstellen in Binär-Datei stimmt nicht mit Hyd-Datei!')
            
        for n,k in enumerate(knoten):
            startpoint = idx1[n]
            idx = np.array([startpoint+(nl*2+5)*i for i in range(sts)])
            assert len(np.unique(ints[idx])) == 1, 'Daten Fehler! Knoten stimmt nicht!'
            Data[k] = floats[idx+1]
    
    #Formatted Transparent
    elif form in ['T','t']:
        floats = np.fromfile(file,dtype=np.float32)
        ints   = np.fromfile(file,dtype=np.int32)
        
        nl = ints[0]
        
        if hasattr(self,'h1d'):
            if not nl == abs(self.h1d.nl)+1 :
                self.statusbar.showMessage('Anzahl der Knoten in Binär-Datei stimmt nicht mit Hyd-Datei!')
        
        knoten = ints[2:2+nl*2:2]
        idx1   = np.arange(2,2+nl*2,2)
        
        sts    = int((len(ints)-2)/(nl*2+1))
        if hasattr(self,'h1d'):
            if not sts == self.h1d.lead :
                self.statusbar.showMessage('Anzahl der Stützstellen in Transparent-Datei stimmt nicht mit Hyd-Datei!')
            
        for n,k in enumerate(knoten):
            startpoint = idx1[n]
            idx = np.array([startpoint+(nl*2+1)*i for i in range(sts)])
            assert len(np.unique(ints[idx])) == 1, 'Daten Fehler! Knoten stimmt nicht!'
            Data[k] = floats[idx+1]
    
    #formatted Ascii
    elif form in ['A','a']:
        
        with open(file,'r') as f:
            lines = f.readlines()
            
        nl = int(lines[0])
        
        if hasattr(self,'h1d'):
            if not nl == abs(self.h1d.nl)+1 :
                self.statusbar.showMessage('Anzahl der Knoten in ASCII-Datei stimmt nicht mit Hyd-Datei!')
        
        knoten= np.hstack([np.int_(list(filter(None,re.split('\s+|³',l.strip())))[::2]) for l in lines if '³' in l])
        values = np.hstack([np.float_(list(filter(None,re.split('\s+|³',l.strip())))[1::2]) for l in lines if '³' in l])
        
        for k in np.unique(knoten):
            idx = np.where(knoten == k)
            vals = values[idx]
            
            Data[k] = vals

    else: raise KeyError('Form muss entweder Ascii,A,a,Binary,B,b, oder Transparent,T,t sein!')

    Data_df = pd.DataFrame(Data)
    
    return Data_df

def renumberHYD(h1d,d_idx):
    #updatehyd
    if h1d.nwel   > 0: h1d.kwel      = [d_idx[i] for i in h1d.kwel]
    if h1d.nqin   > 0: h1d.nupe      = [d_idx[i] for i in h1d.nupe]
    if h1d.njunc  > 0:
        nxj = []
        for i in h1d.nxj:
            for ii in i:
                sub = []
                if ii!=0: sub.append(d_idx[ii])
                else: sub.append(ii)
                nxj.append(sub)
        h1d.nxj = nxj
    if h1d.latinf > 0: h1d.lie       = [d_idx[i] for i in h1d.lie]       
    if h1d.nweirs > 0: h1d.weir_info = ['%10.0f' %d_idx[int(i[:10])]+i[10:] for i in h1d.weir_info]     
    if h1d.ngates > 0: h1d.igate     = [d_idx[i] for i in h1d.igate]
    return h1d

def dfpro_2_line(df):
    
    Node = '%5.0f' %df.name
    pkts = '%5.0f' %df['Npoints']
    ctab = '   ' if df['CTAB'] == 0 else ' 1 '
    mxht = '%5.1f' %df['Max Height']
    try:    stn  = '%10.1f' %df['Station']
    except: stn  = '{:>11}'.format(df['Station'])
    pnam = '   '+df['PName'] if df['PName'][0] != '' else df['PName']
    header = Node + pkts + ctab +df['Mode'] +mxht+ 'STATION :' + stn+' M'+pnam+'\n'

    coords = list(zip(df['X'],df['Y']))
    an = 0
    xy_str = ''
    while an < len(coords):
        cn = 0
        while cn < 4:
            try:
                xy_str = xy_str +'%8.2f' %coords[an+cn][0]+'%8.2f' %coords[an+cn][1]+'  '
            except: pass
            cn = cn+1
        xy_str = xy_str + '\n'
        an = an+4
    
    return header,xy_str
    
def writePRO(ProName,df_pro):
    lines = []
    for k in range(len(df_pro)):
        df   = df_pro.iloc[k]
        header,xy_str = dfpro_2_line(df)
        lines.append(header)
        lines.append(xy_str)

    with open(ProName,'w') as out_f:
        for i in lines: out_f.write(i)

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
            if h1d.gatdat[i][0] == 'N':
                lines.append(('{:1}'+' '*4+'{:5}'+'{:>10}'*7+'\n').format(h1d.gatdat[i][0],h1d.igate[i],
                                                       h1d.iaga[i],*h1d.gatdat[i][1:-4]))
            elif h1d.gatdat[i][0] in ['DE','DW']:
                lines.append((' '*2+'{:2}'+' '+'{:5}'+'{:>10}'*7+'\n').format(h1d.gatdat[i][0],h1d.igate[i],
                                                       h1d.iaga[i],*h1d.gatdat[i][1:-4]))
            else:
                lines.append((' '*5+'{:5}'+'{:>10}'*7+'\n').format(h1d.igate[i],
                                                       h1d.iaga[i],*h1d.gatdat[i][1:-4]))
            if h1d.iaga[i] >=0:
                lines.append('{:30}{:10.3f}{:10.3f}{:10.3f}\n'.format(h1d.gatdat[i][-4:]))
    
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
    