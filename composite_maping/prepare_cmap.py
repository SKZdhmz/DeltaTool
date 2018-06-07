#!/usr/bin/python3
# -*- coding: utf-8 -*-
"""
Created on Mon Apr 16 11:00:13 2018
@author: milic
SHIFTED... grid centre and not
"""
import os
import argparse
import numpy as np
import fiona
import datetime
import shapely.geometry
from netCDF4 import Dataset, num2date


class Mapper(object):
    def __init__(self, NC_FILE_INPUT, SHAPEFILE_INPUT):
        #POLUTANT DEFINITION
        self.POLUTANTI = {'o3':'O3', #originalno u mol/mol --> 1ug/m3 O3 = 0.5 ppb O3
                          'no2':'NO2', #originalno u mol/mol --> 1ug/m3 NO2 = 0.523 ppb NO2
                          'no':'NO', #originalno u mol/mol --> 1ug/m3 NO = 0.802 ppb NO
                          'nox':'NOx', #originalno u mol/mol --> 1ug/m3 NOx = 0.523 ppb NOx
                          'tpm10':'PM10',#originalno u kg/m3
                          'tpm25':'PM2.5',#originalno u kg/m3
                          'co':'CO', #originalno u mol/mol --> 1mg/m3 CO = 0.86 ppm CO
                          'so2':'SO2'} #originalno u mol/mol --> 1ug/m3 SO2 = 0.355 ppb SO2
        #CONVERSION FACTOR FOR UNITS --> translate from netcdf values to ug/m3
        self.CONVERSION_FACTORS = {'o3':2.0 * 1000000000,
                                   'no2':1.912 * 1000000000,
                                   'no':1.246 * 1000000000,
                                   'nox':1.912 * 1000000000,
                                   'tpm10':1000000000,
                                   'tpm25':1000000000,
                                   'co':1.162 * 1000000,
                                   'so2':2.82 * 1000000000}
        #GRID DEFINITION
        self.ncfilename = None #name of ncfile
        self.LONGITUDE_LL = None #TODO! u biti nije LL vec je center
        self.LONGITUDE_STEP = None
        self.LONGITUDE_COUNT = None
        self.LATITUDE_LL = None
        self.LATITUDE_STEP = None
        self.LATITUDE_COUNT = None
        #MASK DEFINITION
        self.mask = None
        #SHAPE OF TARGET AREA (epsg:4326 --> WGS84 projection)
        self.targetAreaShape = None
        #startup procedure for loading object
        self.init_grid_info(NC_FILE_INPUT)
        self.init_shapefile_geometry(SHAPEFILE_INPUT)
        self.init_mask()

    def init_grid_info(self, ncfilename):
        """
        Get and store basic ncfile information about grid
        """
        self.ncfilename = ncfilename
        with Dataset(ncfilename, mode='r') as ncf:
            #low left corner, stepsize and number of longitudes
            self.LONGITUDE_LL = ncf.variables['longitude'][0]
            self.LONGITUDE_STEP = ncf.variables['longitude'][1] - ncf.variables['longitude'][0] #0.125
            self.LONGITUDE_COUNT = ncf.dimensions['longitude'].size
            #low left corner, stepsize and number of latitudes
            self.LATITUDE_LL = ncf.variables['latitude'][0]
            self.LATITUDE_STEP = ncf.variables['latitude'][1] - ncf.variables['latitude'][0] #0.0625
            self.LATITUDE_COUNT = ncf.dimensions['latitude'].size

    def get_polygon(self, lonN, latN):
        """
        define corner points from grid definition and return shapely.polygon for
        gridpoint lonN, latN
        """
        lon1 = round((self.LONGITUDE_LL - (self.LONGITUDE_STEP/2.0) + (lonN * self.LONGITUDE_STEP)), 6)
        lon2 = round((self.LONGITUDE_LL - (self.LONGITUDE_STEP/2.0) + ((lonN + 1) * self.LONGITUDE_STEP)), 6)
        lat1 = round((self.LATITUDE_LL - (self.LATITUDE_STEP/2.0) + (latN * self.LATITUDE_STEP)), 6)
        lat2 = round((self.LATITUDE_LL - (self.LATITUDE_STEP/2.0) + ((latN + 1) * self.LATITUDE_STEP)), 6)
        #couter-clockwise
        poligon = [(lon1, lat1),
                   (lon2, lat1),
                   (lon2, lat2),
                   (lon1, lat2),
                   (lon1, lat1)]
        return shapely.geometry.asPolygon(poligon)

    def init_shapefile_geometry(self, shapefile):
        """
        shapely assumes all is in cartesian plane, lonlat stuff... if shapefile is in other projection
        one must reproject shape to EPSG:4326 --> WGS84
        """
        #define output
        self.targetAreaShape = shapely.geometry.Polygon()
        #read shapefile
        with fiona.open(shapefile, mode='r') as shapes:
            for shape in shapes:
                item = shapely.geometry.asShape(shape['geometry']) #grab polygon
                self.targetAreaShape = self.targetAreaShape.union(item) #union


    def init_mask(self):
        """creates mask based on shapefile..."""
        self.mask = np.zeros((self.LONGITUDE_COUNT, self.LATITUDE_COUNT))
        for i in range(self.mask.shape[0]):
            print('progress....')
            for j in range(self.mask.shape[1]):
                square = self.get_polygon(i,j)
                self.mask[i, j] = not bool(square.intersects(self.targetAreaShape))


    def get_field(self, polutant, function='mean', year=2016):
        """
        Get field value and apply specific function to it along time axis
        - mean, min, max, std, var, median, percentile95, percentile05
        """
        #scam resolution with getattr(np, funcname)(*args, **kwargs)
        with Dataset(self.ncfilename, mode='r') as ncf:
            polje = ncf.variables[polutant]
            #TODO! cut za godinu...
            ntajms = ncf.variables['time']
            ttimes = list(num2date(ntajms[:], units=ntajms.units))
            #locate year
            year_start = ttimes.index(datetime.datetime(year,1,1,0))
            year_end = ttimes.index(datetime.datetime(year,12,31,23))
            #time slice
            output = polje[year_start:year_end+1,0,:,:]
            if function == 'percentile95':
                output = np.percentile(output, 95, axis=0)
            elif function == 'percentile05':
                output = np.percentile(output, 5, axis=0)
            else:
                #get function from numpy namespace...
                f = getattr(np, function)
                output = f(output, axis=0)

        #need to redefine units...
        output = output * self.CONVERSION_FACTORS[polutant]
        #transpose - direct 2d field read is in [lat, lon] need to switch columns and rows to get [lon, lat]
        return np.transpose(output)

    def get_masked_field(self, polutant, function='mean', fill_value=-9999, year=2016):
        """
        Get field value and apply specific function to it along time axis
        - mean, min, max, std, var, median, percentile95, percentile05

        -return only non masked values
        """
        polje = self.get_field(polutant, function=function, year=year)
        maskiranoPolje = np.ma.array(polje, mask=self.mask, fill_value=fill_value)
        return np.ma.filled(maskiranoPolje)

    def assemble_output_file_name(self, outputFolder, polutant, function, opis):
        """output file assembly"""
        #dodaj _ ispred opisa ako nedostaje
        if len(opis) and (not opis.startswith('_')):
            opis = '_' + opis
        #templata za hrvatsku, LOTOS, lon lat mreza, WGS84
        template = "CMAP_LOTOS_HRV_{0}_EPSG4326_{1}{2}.asc".format(self.POLUTANTI[polutant], function, opis)
        return os.path.join(outputFolder, template)


    def write_asc(self, polutant, outputFolder, opis='', function='mean', fill_value=-9999, year=2016):
        """
        write out ESRI asc file for use in composite delta tool
        """
        #calculate field...
        polje = self.get_masked_field(polutant, function=function, fill_value=fill_value, year=year)

        #output file assembly
        outfile = self.assemble_output_file_name(outputFolder, polutant, function, opis)

        csize = " ".join([str(self.LONGITUDE_STEP), str(self.LATITUDE_STEP)])
        with open(outfile, mode='w') as f:
            f.write('NCOLS {0}\n'.format(str(self.LONGITUDE_COUNT)))
            f.write('NROWS {0}\n'.format(str(self.LATITUDE_COUNT)))
            f.write('XLLCENTER {0}\n'.format(str(self.LONGITUDE_LL)))
            f.write('YLLCENTER {0}\n'.format(str(self.LATITUDE_LL)))
            f.write('CELLSIZE {0}\n'.format(csize))
            f.write('NODATA_value {0}\n'.format(str(fill_value)))
            #writing top to bottom
            for row in reversed(range(self.LATITUDE_COUNT)):
                line = " ".join([str(i) for i in polje[:,row]])
                line = line + "\n" #add newline to end
                f.write(line)




if __name__ == '__main__':
    ############################################################################
    #when called from terminal...
#    outFolder = "/home/milic/Desktop/COPOSITE_CONCENTRATIONS/testrun"
#    croShape = "/home/milic/Desktop/COPOSITE_CONCENTRATIONS/HRV_adm_shp/HRV_adm0.shp"
#    lotosFile = "/home/milic/Desktop/COPOSITE_CONCENTRATIONS/cdo_ing/LE_conc-sfc-HR.nc"
#    mapper = Mapper(lotosFile, croShape)
#    mapper.write_asc('tpm10', outFolder, opis='_2016', function='mean', fill_value=-9999)
#    mapper.write_asc('tpm25', outFolder, opis='_2016', function='mean', fill_value=-9999)
#    mapper.write_asc('o3', outFolder, opis='_2016', function='mean', fill_value=-9999)
#    mapper.write_asc('no2', outFolder, opis='_2016', function='mean', fill_value=-9999)
    ############################################################################

    #Opis funkcije
    descriptionMsg = """
    Function creates ESRI asc format for use in composite mapping from LOTOS-EUROS output
    nc files.
    """
    #Završni komentar
    epilogMsg="""
    DMHZ, 2018.
    """
    #Stvaranje "argunent parser" objekta
    parser = argparse.ArgumentParser(description=descriptionMsg, epilog=epilogMsg)
    parser.add_argument("pollutant", choices=['tpm10', 'tpm25', 'o3', 'no2', 'no', 'co', 'so2'], help="Choice of pollutant")
    parser.add_argument("func", choices=['mean', 'min', 'max'], help="Numpy function to agregate time data")
    parser.add_argument("ncFile", help="Path of LOTOS-EUROS output nc file", type=str)
    parser.add_argument("shapefile", help="Path of country/area shapefile (WGS84, EPSG:4326).", type=str)
    parser.add_argument("year", help="year to use in calculation", type=int)
    parser.add_argument("outputFolder", help="Path of output folder.", type=str)
    parser.add_argument("-i", "--info", help="Info for output files.")

    #Dohvacanje rezultata
    args = parser.parse_args() #parse input arguments
    dictview = vars(args)

    #KLASA ZA RAČUNANJE
    mapper = Mapper(dictview['ncFile'], dictview['shapefile'])
    mapper.write_asc(
        dictview['pollutant'],
        dictview['outputFolder'],
        opis=dictview['info'],
        function=dictview['func'],
        year=dictview.get('year',2016),
        fill_value=-9999)
