# -*- coding: utf-8 -*-
import pyqtgraph         as     pg
from functools           import partial
'''import pyqt5 modules'''
from PyQt5               import uic
from PyQt5.QtWidgets     import QDialog,QColorDialog
from PyQt5.QtGui         import QColor,QPen,QBrush
import os
import sys
script_dir = os.getcwd()
SCRIPT_DIR = os.path.dirname(os.path.realpath(sys.argv[0]))
ui         = os.path.join(SCRIPT_DIR,'ui','layerProp.ui')

class layerPropPopup(QDialog):
    def __init__(self,app,itemT,parent=None):
        super().__init__(parent)
        uic.loadUi(ui,self)
        self.apply.clicked.connect(self.accept)
        self.cancel.clicked.connect(self.reject)
        self.pitems = app.lw_dict[itemT]
        if type(self.pitems) == list:
            self.pitem = self.pitems[0]
        else:
            self.pitem = self.pitems
        buttons = [self.pcolor,self.scolor,self.sbrush,self.fbrush]
        [but.clicked.connect(partial(self.colorpicker,but)) for but in buttons]
        self.symbols = app.symbols
        self.pcolor.layer_color_sharon = None
        self.scolor.layer_color_sharon = None
        self.sbrush.layer_color_sharon = None
        self.fbrush.layer_color_sharon = None
        self.initiate()

    def initiate(self):
        
        pen         = self.pitem.opts['pen']
        symbol      = self.pitem.opts['symbol']
        symbolPen   = self.pitem.opts['symbolPen']
        symbolBrush = self.pitem.opts['symbolBrush']
        symbolSize  = self.pitem.opts['symbolSize']
        fillBrush   = self.pitem.opts['fillBrush']
        
        if type(pen) == QPen:
            self.pcolor.setStyleSheet("QPushButton {\n"
    "     background-color: rgba"+str(pen.color().getRgb())+"; border: 1px solid black;\n}")
            self.pstyle.setCurrentIndex(pen.style())
            self.pwidth.setValue(pen.width())
            self.pcolor.layer_color_sharon = pen.color()
        
        if symbol is not None:
            self.sstyle.setCurrentIndex(self.symbols.index(symbol))
            self.ssize.setValue(symbolSize)
        
        #symbolPen
        if type(symbolPen) == QPen:
            self.scolor.setStyleSheet("QPushButton {\n"
    "     background-color: rgba"+str(symbolPen.color().getRgb())+"; border: 1px solid black;\n}")
            self.scolor.layer_color_sharon = symbolPen.color()
    
        elif type(symbolPen) == QColor:
            self.scolor.setStyleSheet("QPushButton {\n"
    "     background-color: rgba"+str(symbolPen.getRgb())+"; border: 1px solid black;\n}")
            self.scolor.layer_color_sharon = symbolPen
        
        elif type(symbolPen) == tuple:
            self.scolor.setStyleSheet("QPushButton {\n"
    "     background-color: rgba"+str(symbolPen)+"; border: 1px solid black;\n}")
            self.scolor.layer_color_sharon = QColor(*symbolPen)

        #symbol brush
        if type(symbolBrush) == QBrush:
            self.sbrush.setStyleSheet("QPushButton {\n"
    "     background-color: rgba"+str(symbolBrush.color().getRgb())+"; border: 1px solid black;\n}")
            self.sbrush.layer_color_sharon = symbolBrush.color()
    
        elif type(symbolBrush) == QColor:
            self.sbrush.setStyleSheet("QPushButton {\n"
    "     background-color: rgba"+str(symbolBrush.getRgb())+"; border: 1px solid black;\n}")
            self.sbrush.layer_color_sharon = symbolBrush
        
        elif type(symbolBrush) == tuple:
            self.sbrush.setStyleSheet("QPushButton {\n"
    "     background-color: rgba"+str(symbolBrush)+"; border: 1px solid black;\n}")
            self.sbrush.layer_color_sharon = QColor(*symbolBrush)
        
        #fillBrush
        if fillBrush is not None:
            self.fill.setChecked(True)
        if type(fillBrush) == QBrush:
            self.fbrush.setStyleSheet("QPushButton {\n"
    "     background-color: rgba"+str(fillBrush.color().getRgb())+"; border: 1px solid black;\n}")
            self.fbrush.layer_color_sharon = fillBrush.color()
    
        elif type(fillBrush) == QColor:
            self.fbrush.setStyleSheet("QPushButton {\n"
    "     background-color: rgba"+str(fillBrush.getRgb())+"; border: 1px solid black;\n}")
            self.fbrush.layer_color_sharon = fillBrush
        
        elif type(fillBrush) == tuple:
            self.fbrush.setStyleSheet("QPushButton {\n"
    "     background-color: rgba"+str(fillBrush)+"; border: 1px solid black;\n}")
            self.fbrush.layer_color_sharon = QColor(*fillBrush)
    
    
    def colorpicker(self,button):
        color = QColorDialog.getColor(parent = self,title = 'Choose Color', options = QColorDialog.ShowAlphaChannel)
        if color.isValid():
            button.setStyleSheet("QPushButton {\n"
    "     background-color: rgba"+str(color.getRgb())+"; border: 1px solid black;\n"
    "}")
        button.layer_color_sharon = color

def changeProps(self,itemT):
    P = layerPropPopup(self,itemT)
    if P.exec_():
        if P.pstyle.currentIndex() == 0:
            pen = None
        else:
            pen = pg.mkPen(color =  P.pcolor.layer_color_sharon,
                           width = P.pwidth.value(),
                           style = P.pstyle.currentIndex())
        
        if P.sstyle.currentIndex() == 7:
            symbol = None
            symbolBrush = None
            symbolPen = None
            symbolSize = None
        else:
            symbol = self.symbols[P.sstyle.currentIndex()]
            symbolBrush = P.sbrush.layer_color_sharon
            symbolPen = P.scolor.layer_color_sharon
            symbolSize = P.ssize.value()
        
        if not P.fill.isChecked(): 
            fillLevel = False
            fillBrush = None
        else: 
            fillLevel = True
            fillBrush = P.fbrush.layer_color_sharon
        
        return(pen,symbol,symbolPen,symbolBrush,symbolSize,fillLevel,fillBrush)
    else:
        return(None)