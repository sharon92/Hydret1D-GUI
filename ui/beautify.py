# -*- coding: utf-8 -*-
'''import system modules'''
import sys
import os

'''import pyqt5 modules'''
from PyQt5.QtGui         import QIcon,QPixmap
from PyQt5.QtWidgets     import  QHeaderView


SCRIPT_DIR = os.path.dirname(os.path.realpath(sys.argv[0]))

def initiateBeautify(self):
    #icons
    icon = QIcon()
    icon.addPixmap(QPixmap(os.path.join(SCRIPT_DIR,"icons","hydret1D.ico")), QIcon.Normal,QIcon.Off)
    self.setWindowIcon(icon)
    icon1 = QIcon()
    icon1.addPixmap(QPixmap(os.path.join(SCRIPT_DIR,"icons","redo3.ico")), QIcon.Normal,QIcon.Off)
    self.redo.setIcon(icon1)
    icon2 = QIcon()
    icon2.addPixmap(QPixmap(os.path.join(SCRIPT_DIR,"icons","undo3.ico")), QIcon.Normal,QIcon.Off)
    self.undo.setIcon(icon2)
    icon3 = QIcon()
    icon3.addPixmap(QPixmap(os.path.join(SCRIPT_DIR,"icons","edit.ico")), QIcon.Normal,QIcon.Off)
    self.start_editing.setIcon(icon3)
    icon4 = QIcon()
    icon4.addPixmap(QPixmap(os.path.join(SCRIPT_DIR,"icons","editfinish.ico")), QIcon.Normal,QIcon.Off)
    self.stop_editing.setIcon(icon4)
    icon6 = QIcon()
    icon6.addPixmap(QPixmap(os.path.join(SCRIPT_DIR,"icons","new.ico")), QIcon.Normal,QIcon.Off)
    self.newp.setIcon(icon6)
    icon7 = QIcon()
    icon7.addPixmap(QPixmap(os.path.join(SCRIPT_DIR,"icons","open32.ico")), QIcon.Normal,QIcon.Off)
    self.openp.setIcon(icon7)
    icon8 = QIcon()
    icon8.addPixmap(QPixmap(os.path.join(SCRIPT_DIR,"icons","save.ico")), QIcon.Normal,QIcon.Off)
    self.savep.setIcon(icon8)
    icon9 = QIcon()
    icon9.addPixmap(QPixmap(os.path.join(SCRIPT_DIR,"icons","runmodel.ico")), QIcon.Normal,QIcon.Off)
    self.run.setIcon(icon9)
    icon10 = QIcon()
    icon10.addPixmap(QPixmap(os.path.join(SCRIPT_DIR,"icons","copy.ico")), QIcon.Normal,QIcon.Off)
    self.copy_clip.setIcon(icon10)
    icon11 = QIcon()
    icon11.addPixmap(QPixmap(os.path.join(SCRIPT_DIR,"icons","Paste.ico")), QIcon.Normal,QIcon.Off)
    self.paste_clip.setIcon(icon11)
    icon12 = QIcon()
    icon12.addPixmap(QPixmap(os.path.join(SCRIPT_DIR,"icons","delete.ico")), QIcon.Normal,QIcon.Off)
    self.delete_rows.setIcon(icon12)
    
    #set table sizes for nicer display
    self.coords_table.horizontalHeader().setSectionResizeMode(QHeaderView.Stretch)
    self.wsp_table.horizontalHeader().setSectionResizeMode(QHeaderView.Stretch)
    
    self.p_wspdat.horizontalHeader().setSectionResizeMode(0,QHeaderView.Stretch)
    for i in range(4):
        self.p_wspdat.horizontalHeader().setSectionResizeMode(i+1,QHeaderView.ResizeToContents)
    
    self.p_weir_table.horizontalHeader().setSectionResizeMode(9,QHeaderView.Stretch)
    for i in range(9):
        self.p_weir_table.horizontalHeader().setSectionResizeMode(i,QHeaderView.ResizeToContents)
        
    for i in range(9):
        self.p_gate_table.horizontalHeader().setSectionResizeMode(i,QHeaderView.ResizeToContents)
    self.p_gate_table.horizontalHeader().setSectionResizeMode(9,QHeaderView.Stretch)
    
    self.p_lie_table.horizontalHeader().setSectionResizeMode(1,QHeaderView.Stretch)
    for i in [0,2,3,4,5,6]:
        self.p_lie_table.horizontalHeader().setSectionResizeMode(i,QHeaderView.ResizeToContents)

    self.p_nxj_table.horizontalHeader().setSectionResizeMode(QHeaderView.Stretch)
    self.p_dxlgam_table.horizontalHeader().setSectionResizeMode(QHeaderView.Stretch)
    
    self.p_nupe.horizontalHeader().setSectionResizeMode(1,QHeaderView.Stretch)
    for i in [0,2,3]:
        self.p_nupe.horizontalHeader().setSectionResizeMode(i,QHeaderView.ResizeToContents)