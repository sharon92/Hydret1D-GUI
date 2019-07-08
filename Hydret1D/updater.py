# -*- coding: utf-8 -*-
"""
Created on Mon Jul  8 08:40:42 2019

@author: s.Shaji
"""

from pyupdater.client import Client
from client_config import ClientConfig

APP_NAME = 'Hydret1D'
APP_VERSION = '1.0.0'

ASSET_NAME = 'Hydret1D'
ASSET_VERSION = '1.0.0'

def print_status_info(info):
    total = info.get(u'total')
    downloaded = info.get(u'downloaded')
    status = info.get(u'status')
    print(downloaded, total, status)
    
client = Client(ClientConfig())

client = Client(ClientConfig(), refresh=True,
                        progress_hooks=[print_status_info])

app_update = client.update_check(APP_NAME, APP_VERSION)

if app_update is not None:
    app_update.download(background=True)
    
    if app_update.is_downloaded():
        app_update.extract_restart()