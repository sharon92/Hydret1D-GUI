# -*- coding: utf-8 -*-
"""
Created on Wed Jun 26 13:32:34 2019

@author: s.Shaji
"""

def riv_bed(Node):

    riv_bed_idx = Node['Y'].argmin()
    
    li,re = 0,-1
    
    while riv_bed_idx == 0:
        li +=1
        try:
            riv_bed_idx = Node['Y'][li:].argmin()
        except:
            riv_bed_idx = Node['Y'].argmin()
            break
    
    riv_bed_idx = li+riv_bed_idx

    while riv_bed_idx == len(Node['Y'])-1:
        re -=1
        try:
            riv_bed_idx = Node['Y'][:re].argmin()
        except:
            riv_bed_idx = Node['Y'].argmin()
            break

    try:
        riv_bed_y   = Node['Y'][riv_bed_idx]
        riv_bed_x   = Node['X'][riv_bed_idx]
    except:
        riv_bed_idx = Node['Y'].argmin()
        riv_bed_y   = Node['Y'][riv_bed_idx]
        riv_bed_x   = Node['X'][riv_bed_idx]
    
    return(riv_bed_y,riv_bed_idx,riv_bed_x)

def cal_bank(Node,Mode = None,MaxHt = None,return_idx = False):
    
    '''Modus: Uffer Ermittlung'''
    riv_bed_y,riv_bed_idx,riv_bed_x = riv_bed(Node)
    
    if Mode is None:
        Mode = Node.Mode
        MaxHt = Node['Max Height']

    if MaxHt == -1:

        if Mode in ['RF','RK','RS']:
            if Node['Y'][:riv_bed_idx].size ==0: 
                LOB_y = Node['Y'][riv_bed_idx]
                LOB_x = Node['X'][riv_bed_idx]
                midx1 = riv_bed_idx
            else:
                LOB_y = Node['Y'][:riv_bed_idx].max()
                midx1 = Node['Y'][:riv_bed_idx].argmax()
                LOB_x = Node['X'][midx1]
                
            if Node['Y'][riv_bed_idx:].size ==0: 
                ROB_y = Node['Y'][riv_bed_idx]
                ROB_x = Node['X'][riv_bed_idx]
                midx2 = riv_bed_idx
            else:
                ROB_y = Node['Y'][riv_bed_idx:].max()
                midx2 = riv_bed_idx+Node['Y'][riv_bed_idx:].argmax()
                ROB_x = Node['X'][midx2]
                
        elif Mode in ['VF','VK','H2','DF','OF','ZS']:
            midx1 = 0
            midx2 = -1
            LOB_y = Node['Y'][0]
            LOB_x = Node['X'][0]
            ROB_y = Node['Y'][-1]
            ROB_x = Node['X'][-1]

    else:
        for i,y in enumerate(reversed(Node['Y'][:riv_bed_idx])):
            midx1 = -1*i-1
            if y > MaxHt:
                break
        LOB_y = Node['Y'][:riv_bed_idx][midx1]
        LOB_x = Node['X'][:riv_bed_idx][midx1]
        for i,y in enumerate(Node['Y'][riv_bed_idx:]):
            midx2 = i
            if y > MaxHt:
                midx2 = i-1
                break
        ROB_y = Node['Y'][riv_bed_idx+midx2]
        ROB_x = Node['X'][riv_bed_idx+midx2]
    
    if return_idx:
        return LOB_x,LOB_y,ROB_x,ROB_y,midx1,midx2
    else:
        return LOB_x,LOB_y,ROB_x,ROB_y