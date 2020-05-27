# -*- coding: utf-8 -*-
from fbs_runtime.application_context.PyQt5 import ApplicationContext

'''import system modules'''
import sys
import time

#hidden import for pyinstaller
import pkg_resources.py2_warn
'''import pyqt5 modules'''
import pyqtgraph          as     pg

from PyQt5.QtWidgets      import QSplashScreen
from PyQt5.QtCore         import Qt
from PyQt5.QtGui          import QPixmap

'''import window classes'''
from windows.HydretWindow import MainW
from windows.ProfilWindow import ProfW

pg.setConfigOption('background', 'w')
pg.setConfigOption('foreground', 'k')

__version__ = '1.2.7'

if __name__ == '__main__':
    appctxt = ApplicationContext()
    
    icon  = 'icons/hydret1D.ico'
    if len(sys.argv)<2:
        myapp = MainW(__version__)   
    elif len(sys.argv)>1:
        if sys.argv[1].lower().endswith('.pro'): 
            myapp = ProfW(__version__,propath=sys.argv[1])
            icon  = 'icons/proEditor.png'
        elif sys.argv[1].lower().endswith('.run'):
            myapp = MainW(__version__)
        elif sys.argv[1].lower().endswith('.hgui'):
            myapp = MainW(__version__)
        
    splash_pix = QPixmap(icon)
    splash     = QSplashScreen(splash_pix,Qt.WindowStaysOnTopHint)
    splash.setWindowFlags(Qt.WindowStaysOnTopHint | Qt.FramelessWindowHint)
    splash.setEnabled(False)

    splash.show()
    splash.showMessage("", Qt.AlignBaseline| Qt.AlignBottom, Qt.white)

    time.sleep(0.7)

    myapp.show()
    splash.finish(myapp)
    
    if len(sys.argv)>1:
        if sys.argv[1].lower().endswith('.run'):
            try:
                myapp.OpenEnv(DOUBLECLICK_FILE=True)
            except Exception as err:
                myapp.statusbar.showMessage('Ungültige File!\nError: '+repr(err))
        elif sys.argv[1].lower().endswith('.hgui'):
            try:
                myapp.OpenEnv(DOUBLECLICK_FILE=True)
            except Exception as err:
                myapp.statusbar.showMessage('Ungültige File!\nError: '+repr(err))

    exit_code = appctxt.app.exec_()
    sys.exit(exit_code)