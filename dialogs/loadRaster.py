# -*- coding: utf-8 -*-
'''import system modules'''

'''import pyqt5 modules'''
from PyQt5               import uic
from PyQt5.QtWidgets     import QDialog
import os
import sys
script_dir = os.getcwd()
SCRIPT_DIR = os.path.dirname(os.path.realpath(sys.argv[0]))
ui         = os.path.join(SCRIPT_DIR,'ui','rastercatalog.ui')

class openCatalog(QDialog):
    def __init__(self,myapp,fields, parent=None):
        super().__init__(parent)
        uic.loadUi(ui,self)
        self.main   = myapp
        self.image.addItems(fields)
        self.xmin.addItems(fields)
        self.ymin.addItems(fields)
        self.xmax.addItems(fields)
        self.ymax.addItems(fields)
        if 'IMAGE' in fields:
            self.image.setCurrentText('IMAGE')
        if 'XMIN' in fields:
            self.xmin.setCurrentText('XMIN')
        if 'YMIN' in fields:
            self.ymin.setCurrentText('YMIN')
        if 'XMAX' in fields:
            self.xmax.setCurrentText('XMAX')
        if 'YMAX' in fields:
            self.ymax.setCurrentText('YMAX')
            
def defineCatalog(self,fields):
    Popup = openCatalog(self,fields)
    if Popup.exec_():
#        self.sharon_filters = [Popup.filter.itemText(i) for i in range(Popup.filter.count())]
        return(Popup.image.currentText(),
               Popup.xmin.currentText(),
               Popup.ymin.currentText(),
               Popup.xmax.currentText(),
               Popup.ymax.currentText(),
               Popup.reduce.value(),
               Popup.filter.currentText(),
               Popup.abstand.value())
    else:
        return None