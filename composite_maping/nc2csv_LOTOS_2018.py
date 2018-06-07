#!/usr/bin/python3
# -*- coding: utf-8 -*-
"""
Created on Mon Apr 16 11:00:13 2018
@author: milic
SHIFTED... grid centre and not
"""
import datetime
import numpy as np
import pandas as pd
import itertools
from netCDF4 import Dataset, num2date

class Lotos2CSV(object):
    """
    lotos conversion to csv (for use with grafer and surfer)
    """
    def __init__(self):
        self.POLUTANTI = {'o3':'O3 [ug/m3]', #originalno u mol/mol --> 1ug/m3 O3 = 0.5 ppb O3
                          'no2':'NO2 [ug/m3]', #originalno u mol/mol --> 1ug/m3 NO2 = 0.523 ppb NO2
                          'no':'NO [ug/m3]', #originalno u mol/mol --> 1ug/m3 NO = 0.802 ppb NO
                          'nox':'NOx [ug/m3]', #originalno u mol/mol --> 1ug/m3 NOx = 0.523 ppb NOx
                          'tpm10':'PM10 [ug/m3]',#originalno u kg/m3
                          'tpm25':'PM2.5 [ug/m3]',#originalno u kg/m3
                          'co':'CO [mg/m3]', #originalno u mol/mol --> 1mg/m3 CO = 0.86 ppm CO
                          'so2':'SO2 [ug/m3]'} #originalno u mol/mol --> 1ug/m3 SO2 = 0.355 ppb SO2
        #CONVERSION FACTOR FOR UNITS --> translate from netcdf values to ug/m3
        self.CONVERSION_FACTORS = {'o3':2.0 * 1000000000,
                                   'no2':1.912 * 1000000000,
                                   'no':1.246 * 1000000000,
                                   'nox':1.912 * 1000000000,
                                   'tpm10':1000000000,
                                   'tpm25':1000000000,
                                   'co':1.162 * 1000000,
                                   'so2':2.82 * 1000000000}

    def process(self, ncFile, csvFajl):
        firstRun = True
        with Dataset(ncFile, mode='r') as ncf:
            #define and get full grid
            lons = ncf.variables['longitude'][:]
            lats = ncf.variables['latitude'][:]
            lonVectorized = np.repeat(lons, len(lats))
            latVectorized = np.tile(lats, len(lons))
            times = ncf.variables['time']
            for i in range(len(times)):
                t = num2date(times[i], units=times.units)
                print(t)
                tmp = {}
                for polutant in ['o3']:
                    polje = ncf.variables[polutant]
                    polje = polje[i,0,:,:]
                    val = polje.flatten(order='F')
                    val = val * self.CONVERSION_FACTORS[polutant] #convert units
                    tmp[self.POLUTANTI[polutant]] = val
                #assemble frame
                cols = ['godina', 'mjesec', 'dan', 'sat', 'lon', 'lat', self.POLUTANTI[polutant]]
                df = pd.DataFrame(data=tmp)
                df['godina'] = t.year
                df['mjesec'] = t.month
                df['dan'] = t.day
                df['sat'] = t.hour
                df['lon'] = lonVectorized
                df['lat'] = latVectorized
                df = df[cols]
                #write to file
                if firstRun == True:
                    df.to_csv(csvFajl, mode='a', index=False, sep=',', header=True)
                    firstRun = False
                else:
                    df.to_csv(csvFajl, mode='a', index=False, sep=',', header=False)

    def process_by_month(self, ncFile, csvFajl, mj, god):
        firstRun = True
        with Dataset(ncFile, mode='r') as ncf:
            #define and get full grid
            lons = ncf.variables['longitude'][:]
            lats = ncf.variables['latitude'][:]
            lonVectorized = np.repeat(lons, len(lats))
            latVectorized = np.tile(lats, len(lons))
            times = ncf.variables['time']
            for i in range(len(times)):
                t = num2date(times[i], units=times.units)
                if t.month != mj or t.year != god:
                    continue
                print(t)
                tmp = {}
                for polutant in ['o3']:
                    polje = ncf.variables[polutant]
                    polje = polje[i,0,:,:]
                    val = polje.flatten(order='F')
                    val = val * self.CONVERSION_FACTORS[polutant] #convert units
                    tmp[self.POLUTANTI[polutant]] = val
                #assemble frame
                cols = ['godina', 'mjesec', 'dan', 'sat', 'lon', 'lat', self.POLUTANTI[polutant]]
                df = pd.DataFrame(data=tmp)
                df['godina'] = t.year
                df['mjesec'] = t.month
                df['dan'] = t.day
                df['sat'] = t.hour
                df['lon'] = lonVectorized
                df['lat'] = latVectorized
                df = df[cols]
                df.sort_values(by=['godina','mjesec','dan','lon','lat'], inplace=True)
                #write to file
                if firstRun == True:
                    df.to_csv(csvFajl, mode='a', index=False, sep=',', header=True)
                    firstRun = False
                else:
                    df.to_csv(csvFajl, mode='a', index=False, sep=',', header=False)

    def process_daymax8h(self, ncFile, csvFajl):
        firstRun = True
        with Dataset(ncFile, mode='r') as ncf:
            lons = ncf.variables['longitude']
            lats = ncf.variables['latitude']
            times = ncf.variables['time']
            tindex = list(num2date(times[:], units=times.units)) #time index for everything
            prostorniIndeksi = list(itertools.product(range(len(lons)), range(len(lats))))
            for loni, lati in prostorniIndeksi:
                print('lon : ', loni, ' lat : ', lati)
                lon = lons[loni]
                lat = lats[lati]
                value = ncf.variables['o3'][:,0,lati,loni]
                #convert values
                value = value * self.CONVERSION_FACTORS['o3']
                #frejm & resample stuff
                df = pd.DataFrame(data={'day max 8h average O3 [ug/m3]':value}, index=tindex)
                df = df.rolling('8H').mean()
                df = df.resample('D', closed='left', label='left').max()
                df['lon'] = lon
                df['lat'] = lat
                df['godina'] = [i.year for i in df.index]
                df['mjesec'] = [i.month for i in df.index]
                df['dan'] = [i.day for i in df.index]
                cols = ['godina', 'mjesec', 'dan', 'lon', 'lat', 'day max 8h average O3 [ug/m3]']
                df = df[cols]
                if firstRun == True:
                    df.to_csv(csvFajl, mode='a', index=False, sep=',', header=True)
                    firstRun = False
                else:
                    df.to_csv(csvFajl, mode='a', index=False, sep=',', header=False)

    def process_year_stats(self, ncFile, csvFajl, god):
        firstRun = True
        with Dataset(ncFile, mode='r') as ncf:
            lons = ncf.variables['longitude']
            lats = ncf.variables['latitude']
            times = ncf.variables['time']
            tindex = list(num2date(times[:], units=times.units)) #time index for everything
            prostorniIndeksi = list(itertools.product(range(len(lons)), range(len(lats))))
            #locate start/end of year
            year_start = tindex.index(datetime.datetime(god, 1, 1))
            year_end = tindex.index(datetime.datetime(god, 12, 31, 23))
            ntindex = tindex[year_start:year_end+1] #TODO!
            for loni, lati in prostorniIndeksi:
                print('lon : ', loni, ' lat : ', lati)
                lon = lons[loni]
                lat = lats[lati]
                value = ncf.variables['o3'][year_start:year_end+1,0,lati,loni]
                #convert values
                value = value * self.CONVERSION_FACTORS['o3']
                #frejm & resample stuff
                tmp = pd.DataFrame(data={'O3':value}, index=ntindex)
                sredValue = tmp.resample('A').mean()
                maxValue = tmp.resample('A').max()
                #merge stuff
                out = pd.DataFrame(data={'mean O3 [ug/m3]':sredValue['O3'], 'max O3 [ug/m3]':maxValue['O3']}, index=sredValue.index)
                out['lon'] = lon
                out['lat'] = lat
                out['godina'] = [i.year for i in out.index]
                cols = ['godina', 'lon', 'lat', 'mean O3 [ug/m3]', 'max O3 [ug/m3]']
                out = out[cols]
                if firstRun == True:
                    out.to_csv(csvFajl, mode='a', index=False, sep=',', header=True)
                    firstRun = False
                else:
                    out.to_csv(csvFajl, mode='a', index=False, sep=',', header=False)



def sort_frejm(csvin, csvout):
    dfin = pd.read_csv(csvin)
    dfout = dfin.sort_values(by=['godina','mjesec','dan','lon','lat'])
    dfout.to_csv(csvout, index=False, sep=',')

def get_test_value(ncfile, tindex, lonindex, latindex):
    with Dataset(ncfile, mode='r') as ncf:
        ozon = ncf.variables['o3']
        lon = ncf.variables['longitude']
        lat = ncf.variables['latitude']
        times = ncf.variables['time']
        value = ozon[tindex,0,latindex,lonindex] * 2000000000
        t = num2date(times[tindex], units=times.units)
        return ",".join([str(t.year), str(t.month), str(t.day), str(t.hour), str(lon[lonindex]), str(lat[latindex]), str(value)])

if __name__ == '__main__':
    ncfajl = "/home/milic/Desktop/COMPOSITE_CONCENTRATIONS/LE_v2-1-001_conc-sfc-HR_2016.nc"
#    csvfajl = "/home/milic/Desktop/lotos_09052018/lotos_2016_o3_full.csv"
#    csv8Hunsorted = "/home/milic/Desktop/lotos_09052018/lotos_2016_o3_day8Hmax_unsorted.csv"
#    csv8Hsorted = "/home/milic/Desktop/lotos_09052018/lotos_2016_o3_day8Hmax_sorted.csv"
#    WORKER = Lotos2CSV()
#    WORKER.process_year_stats(ncfajl, "/home/milic/Desktop/lotos_ozon_godisnji_2016.csv", 2016)
#    for i in range(1, 13):
#        fname = "/home/milic/Desktop/lotos_ozon_2016/lotos_2016_o3_{0}mjesec.csv".format(str(i))
#        print('writing : ', fname)
#        WORKER.process_by_month(ncfajl, fname, i, 2016)
#    WORKER.process_daymax8h(ncfajl, csv8Hunsorted)
#    #sortnig frejm
#    sort_frejm(csv8Hunsorted, csv8Hsorted)
    #pass

