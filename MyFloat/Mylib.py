"""Google API"""
from apiclient.discovery import build
from oauth2client.service_account import ServiceAccountCredentials
from google_auth_oauthlib.flow import InstalledAppFlow,Flow
from google.auth.transport.requests import Request

"""Utilities"""
from pprint import pprint
from tqdm import tqdm
import datetime, re

"""DM"""
import pandas as pd

"""Custom Classes and Methods"""

class GoogleAPI:
    def __init__(self):
        self.VIEW_ID = '206811706'
        self.cred = None
        
    def initialize_serivce(self, json, scope, service_name, version):
        if service_name == 'sheets':
            if not self.cred or not self.cred.valid:
                if self.cred and self.cred.expired and self.cred.refresh_token:
                    self.cred.refresh(Request())
                else:
                    flow = InstalledAppFlow.from_client_secrets_file(json, scope)
                    self.cred = flow.run_local_server()

            try:
                return build(service_name, version, credentials=self.cred)
                print(service_name, 'service created successfully')
            except Exception as e:
                print(e)
        else:
            return build(service_name, version,
                         credentials = ServiceAccountCredentials.from_json_keyfile_name(json, scope))
    
    
def MakeSheet(gsserv, data_dict, sheet_title, RANGE_NAME = 'A1:AA1000'):
    spreadsheet = {
        'properties': {
            'title': sheet_title
        },
        "sheets": [{'properties':{'title':s}} for s in data_dict.keys()]
    }

    SPREADSHEET_ID = gsserv.spreadsheets().create(
        body=spreadsheet, fields='spreadsheetId'
    ).execute()['spreadsheetId']

    [gsserv.spreadsheets().values().update(
        spreadsheetId=SPREADSHEET_ID,
        valueInputOption='RAW',
        range=s+'!'+RANGE_NAME,
        body=dict(
            majorDimension='ROWS',
            values=data_dict[s].T.reset_index().T.values.tolist())
    ).execute() for s in data_dict.keys()]
    print('Sheet "{}" successfully Created'.format(sheet_title))
    
    
def get_report(service, request_body):
    """Queries the Analytics Reporting API V4.
    Args:
        analytics: An authorized Analytics Reporting API V4 service object.
    Returns:
        The Analytics Reporting API V4 response.
    """
    return service.reports().batchGet(body = request_body).execute()


def form_response(response):
    #   Parses and forms the Analytics Reporting API V4 response.
    """
    Args:
        response: An Analytics Reporting API V4 response.
    """
    results = []
    for report in response.get('reports', []):
        columnHeader = report.get('columnHeader', {})
        dimensionHeaders = columnHeader.get('dimensions', [])
        metricHeaders = columnHeader.get('metricHeader', {}).get('metricHeaderEntries', [])

    for row in report.get('data', {}).get('rows', []):
        dimensions = row.get('dimensions', [])
        dateRangeValues = row.get('metrics', [])

        results.append({f'{header}': dimension 
                        for header, dimension in zip(dimensionHeaders, dimensions)})

        for i, values in enumerate(dateRangeValues):
            results[-1].update({f"{metricHeader.get('name')}": value
                                for metricHeader, value in zip(metricHeaders, values.get('values'))})
#             results[-1].update({'Date range': i})
    return(results)


def buildDF(DATES, DIMENSIONS, METRICS, COUNTRIES = ["Canada", 'United States']):
    """
    buildDF(DATES, DIMENSIONS, METRICS)
    """
    responses = []
    for date in tqdm(DATES, leave = False):
        rqst_body = {
            'reportRequests': [{
                'viewId': my_API.VIEW_ID, # Don't touch this
                'dateRanges': [{'startDate':date[0], 'endDate':date[1]}],

                'dimensions': [{'name': f'ga:{dim}'} for dim in DIMENSIONS],
                "dimensionFilterClauses": [{"operator": 'AND',
                                            "filters": [
                                                {"dimensionName": "ga:country",
                                                 "operator": "IN_LIST",
                                                 "expressions": COUNTRIES}
                                            ]}],

                'metrics': [{'expression': f'ga:{metrc}'} for metrc in METRICS]
#                 ,'metricFilterClauses': [{'filters': [
#                     {'metricName': 'ga:sessionDuration',
#                      'operator': 'GREATER_THAN',
#                      'comparisonValue': '0.0'}]}]
            }]
        }
        response = get_report(ar, rqst_body)
        responses.append(response)

    dict_list = []
    for response in tqdm(responses):
        dict_list.extend([fr for fr in form_response(response)])
    DF = pd.DataFrame(dict_list)
    DF.columns=pd.Index([re.sub('ga:', '', c) for c in DF.columns])
    return DF


print('==========================\n'
      'FloatAnalytics.py imported\n'
      '==========================')
# if __name__ == '__main__':
#   analytics = initialize_analyticsreporting()