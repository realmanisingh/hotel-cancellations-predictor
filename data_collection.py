#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Mar 29 21:17:32 2020

@author: mani
"""

from darksky.api import DarkSky
from darksky.types import languages, units, weather
from datetime import datetime as dt
import numpy as np


# key needed to access dark sky api
API_KEY = 'e7a30ac223ddd32b8afd34c868279d7f'

# dark sky api object
darksky = DarkSky(API_KEY)


# Function that returns csv containing the daily precipitation values
def get_data(year, latitude, longitude, start_month, end_month):
    # Storing the daily precipitaion values
    daily_precip = []
    # Accounting for leap years
    feb_days = 28
    if year % 4 == 0:
        feb_days = 29
        
    # Getting a subset of the months that we want
    wanted_keys = [i for i in range(start_month, end_month + 1)]
        
    months = {1: 31, 2: feb_days, 3: 31, 4: 30, 5: 31, 6: 30, 7: 31, 8: 31, 9: 30, 10: 31, 11: 30, 12: 31}
    
    # Creating a new dictionary with the subset
    months_subset = dict((k, months[k]) for k in wanted_keys if k in months)
    
    for i in months_subset:
        for j in range(1, months_subset[i] + 1):
            t = dt(year, i, j)
            forecast = darksky.get_time_machine_forecast(
                latitude, longitude,
                extend=False, # default `False`
                lang=languages.ENGLISH, # default `ENGLISH`
                values_units=units.AUTO, # default `auto`
                exclude=[weather.MINUTELY, weather.ALERTS], # default `[]`,
                timezone='UTC', # default None - will be set by DarkSky API automatically
                time=t
            )
            
        
            # Adding the precipitation to the list 
            daily_forecast = forecast.daily
            daily_data = daily_forecast.data
            print(daily_data)
            daily_precip.append(daily_data[0].precip_intensity_max)
            print(daily_precip)
        
    # Exporting results to a csv 
    daily_precip_np = np.array(daily_precip)
    file_name = str(start_month) + "-" + str(end_month) + "-" + str(year) + ".csv"
    np.savetxt(file_name, daily_precip_np, delimiter=",")
    
    return daily_precip


function_test = get_data(2015, 38.722252, -9.139337, 4, 12)
print(function_test)

test2 = get_data(2016, 38.722252, -9.139337, 4, 12)
test3 = get_data(2017, 38.722252, -9.139337, 4, 9)
test4 = get_data(2015, 38.638760, -9.037660, 1, 12)
test5 = get_data(2016, 38.638760, -9.037660, 1, 12)
test6 = get_data(2017, 38.638760, -9.037660, 1, 12)


