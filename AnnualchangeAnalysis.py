#
# 12/12/2022
#
# code written by Alex Wallage for the use in their dissertation.
#
# Purpose: to compare each annual velocity mosaic from ITS_LIVE Dataset to a 
#longterm average from 1985-2018.
#
# The below video was useful in the writing of this code
# https://www.youtube.com/watch?v=E1H1oD8c-lo
#


# import modules for use in this code
import os
import glob
import processing

# set the working directory

dir = "D:\Backup\OneDrive - University of Exeter\year_4\Dissertation_data\ITS_LIVE_velocity"

print(os.path.exists(dir)) # check directory exists

# create list of all files to be used in the analysis, different years 
inputs = glob.glob(f'{dir}\**\*clip.tif')
print(inputs) # make sure these are the files needed



# loop for each year
for c1 in inputs:
    
    name = c1.split('\\')[6] # extract year of analysis from path name
    print(name)
    
    lyr1 =QgsRasterLayer(c1) # turn year GEOTIFF file into readable Raster layer for the calculator
    output = f'{dir}\\{name}\\{name}diffLongTermBaseline.tif' #  set output location 
    entries = [] #  set up list for raster filkes used in Analysis, calculator needs to reference them
    
    #Set up longterm baseline as Raster layer
    LongtermBaseline = f"{dir}\\LongTermAverageV1985to2018.tif"
    lyr2 = QgsRasterLayer(LongtermBaseline) # turn year GEOTIFF file into readable Raster layer for the calculator
    ras = QgsRasterCalculatorEntry() create Raster calculator reference layer
    ras.ref = 'ras@2' # name it
    ras.raster = lyr2 # set it to the Longterm baseline raster
    ras.bandNumber = 1 # select Band number to use
    entries.append(ras) # add to entries list to be referenced later in analysis
    
    # set up year as refernced raster layer
    ras = QgsRasterCalculatorEntry() # turn year GEOTIFF file into readable Raster layer for the calculator
    ras.ref = 'ras@1' # name it
    ras.raster = lyr1 # set it to the year raster
    ras.bandNumber = 1 # select band number to use
    entries.append(ras) # add to entries list to be referenced later in analysis
    
    # Run analysis : year - longterm Baseline 
    calc = QgsRasterCalculator('ras@1 - ras@2', output, 'Gtiff', \
    lyr1.extent(), lyr1.width(), lyr1.height(), entries)
    calc.processCalculation() # run above calculation
    



# end of script


