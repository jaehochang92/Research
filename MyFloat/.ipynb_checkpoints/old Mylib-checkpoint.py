"""Google API"""
from apiclient.discovery import build
from oauth2client.service_account import ServiceAccountCredentials

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
        self.credentials = ServiceAccountCredentials.from_json_keyfile_name(
            './float-222704-70edd21a1124.json', # key file location
            ['https://www.googleapis.com/auth/analytics.readonly'] # scope
        )
        
    def initialize_serivce(self, service_name, version):
        """Initializes an Analytics Reporting API V4 service object.
        Returns:
            An authorized Analytics Reporting API V4 service object.
        """
        return build(service_name, version, credentials = self.credentials)
    
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
#             results.append('Date range: ' + str(i))
            results[-1].update({f"{metricHeader.get('name')}": value
                                for metricHeader, value in zip(metricHeaders, values.get('values'))})
    return(results)


print('==========================\n'
      'FloatAnalytics.py imported\n'
      '==========================')
# if __name__ == '__main__':
#   analytics = initialize_analyticsreporting()