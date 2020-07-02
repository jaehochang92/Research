'UTILITIES========================================================================================'
'Google API'
# !pip install --upgrade google-api-python-client google-auth-httplib2 google-auth-oauthlib
from googleapiclient.discovery import build
from google_auth_oauthlib.flow import InstalledAppFlow,Flow
from google.auth.transport.requests import Request

'Modeling utilities'
import gc, time, pickle
from datetime import datetime
from pprint import pprint
from tqdm import tqdm as tqdm_base
from collections import Counter

'Data handling'
import pandas as pd
# import datatable as dt
import numpy as np
import os, re, operator

'Text coloring'
'First, follow the instructions in:'
'https://ipywidgets.readthedocs.io/en/latest/user_install.html#installing-the-jupyterlab-extension'
from termcolor import colored

def mMscl(l):
    '''
    mMscl([1,3,3,9])
    >>> 
    '''
    if len(set(l)) > 1:
        l -= min(l)
        den = max(l) - min(l)
        return(l / den + 1)
    else:
        return(l)
    
def tqdm(*args, **kwargs):
    if hasattr(tqdm_base, '_instances'):
        for instance in list(tqdm_base._instances):
            tqdm_base._decr_instances(instance)
    return tqdm_base(*args, **kwargs)

def lngrd(l, m, M): # f(x)=a*exp(b*x)
    b = np.log(M/m)/(l-1)
    a = m/np.exp(b)
    return([a*np.exp(b*x) for x in range(1, l+1)])
    
def gcol() : 
    stop = False
    i = 0
    while stop == False :
        i = i+1
#         print('Collecting %d ...' % i)
        n = gc.collect()
#         print('Unreachable objects :', n)
#         print('Remaining Garbage :', gc.garbage)
        if n == 0 :
            stop = True

class my_list(list):
    """
    my_list([[1,3,2],['a',2,0,'b']]).sum()
    my_list(['a','b','c','d']).sum()
    """
    def __init__(self, l):
        self.l = l
    def sum(self):
        tmp = []
        for i in self.l:
            tmp += i
        return(tmp)
    def sumtkns(self):
        tmp = ''
        for i in self.l:
            tmp += i + ' '
        return(tmp)
            
class my_dict(dict):
    """
    d1 = {'a':[1,1],'b':[2,2]}
    d2 = {'a':[0,1,-1], 'b':[-1,0]}
    my_dict(d1).appnd(d2)
    d1
    >>> {'a': [1, 1, 0, 1, -1], 'b': [2, 2, -1, 0]}
    d3 = {'a':.1, 'b':0}
    my_dict(d1).appnd(d3)
    d1
    >>> {'a': [1, 1, 0, 1, -1, 0.1], 'b': [2, 2, -1, 0, 0]}
    """
    def __init__(self, d1):
        self.d1 = d1
        self.ks = d1.keys()
    def appnd(self, d2):
        for k in self.ks:
            if type(d2[k]) is not list:
                self.d1[k] += [d2[k]]
            else:
                self.d1[k] += d2[k]
                

# Google API

class GgleAPI:
    '''
    GSAPI = GgleAPI('credentials.json', 'sheets')
    GSAPI.Create_Service('v4', ['https://www.googleapis.com/auth/spreadsheets'])
    data_dict = {
        'sheet1':pd.DataFrame({'a':[1,2,3], 'b':[3,2,1]}),
        'sheet2':pd.DataFrame({'c':[1,2,3], 'd':[3,2,1]})
    }
    sheet_title, RANGE_NAME = 'TestSheet', 'A1:AA1000'
    GSAPI.MakeSheet(data_dict, sheet_title, RANGE_NAME)
    '''
    def __init__(self, credential_json, api_service_name):
        self.credential_json = credential_json
        self.api_service_name = api_service_name
        self.cred = None
    
    def Create_Service(self, api_version, *scopes):
        self.SCOPES = [scope for scope in scopes[0]]
#         if os.path.exists('token_write.pickle'):
#             with open('token_write.pickle', 'rb') as token:
#                 self.cred = pickle.load(token)
        if not self.cred or not self.cred.valid:
            if self.cred and self.cred.expired and self.cred.refresh_token:
                self.cred.refresh(Request())
            else:
                flow = InstalledAppFlow.from_client_secrets_file(self.credential_json, self.SCOPES)
                self.cred = flow.run_local_server()

#             with open('token_write.pickle', 'wb') as token:
#                 pickle.dump(self.cred, token)
        try:
            self.service = build(self.api_service_name, api_version, credentials=self.cred)
            print(self.api_service_name, 'service created successfully')
            #return service
        except Exception as e:
            print(e)
            #return None
            
    def MakeSheet(self, data_dict, sheet_title, RANGE_NAME):
        spreadsheet = {
            'properties': {
                'title': sheet_title
            },
            "sheets": [{'properties':{'title':s}} for s in data_dict.keys()]
        }
        
        self.SPREADSHEET_ID = self.service.spreadsheets().create(
            body=spreadsheet, fields='spreadsheetId'
        ).execute()['spreadsheetId']
        
        [self.service.spreadsheets().values().update(
            spreadsheetId=self.SPREADSHEET_ID,
            valueInputOption='RAW',
            range=s+'!'+RANGE_NAME,
            body=dict(
                majorDimension='ROWS',
                values=data_dict[s].T.reset_index().T.values.tolist())
        ).execute() for s in data_dict.keys()]
        print('Sheet "{}" successfully Created'.format(sheet_title))

# For R-Py
if __name__ == '__main__':
    import pandas as pd
    
    def read_pickle_file(file):
        pickle_data = pd.read_pickle(file)
        return pickle_data