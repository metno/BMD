#!/bin/bash
# Script to calculate mean monthly frequency of heatwaves from the NASA Earth Exchange Global Daily Downscaled Projections (NEX-GDDP-CMIP6) data
# for Bangladesh (Bounding box)
#
# Link to the data
# https://ds.nccs.nasa.gov/thredds/catalog/AMES/NEX/GDDP-CMIP6/catalog.html
# Information on the data can be found at
# https://www.nccs.nasa.gov/services/data-collections/land-based-products/nex-gddp-cmip6 
# and (technical note)
# https://www.nccs.nasa.gov/sites/default/files/NEX-GDDP-CMIP6-Tech_Note.pdf 

# A subset covering the Bangladesh box can be found at
# https://thredds.met.no/thredds/catalog/metusers/andreasd/tilBMD/NEX-GDDP-CMIP6/CMIP6_Tmax_SSP585/catalog.html

### Cut out a box from the SSP5-8.5 NEX-GDDP-CMIP6 data, MPI-ESM1-2-HR model
for ((year=2015;year<=2100;year++))
do cdo -sellonlatbox,87.5,93,20,27 https://thredds.met.no/thredds/dodsC/metusers/andreasd/tilBMD/NEX-GDDP-CMIP6/CMIP6_Tmax_SSP585/Tmax_MPI-ESM1-2-HR_ssp585/tasmax_day_MPI-ESM1-2-HR_ssp585_r1i1p1f1_gn_${year}.nc BangladeshBox_tasmax_day_MPI-ESM1-2-HR_ssp585_r1i1p1f1_gn_${year}.nc
done

### Get the corresponding historical data (directly from NASA):
### This can be replaced with using your local files
for ((year=1981;year<=2014;year++))
#download the global data
do wget https://ds.nccs.nasa.gov/thredds/fileServer/AMES/NEX/GDDP-CMIP6/MPI-ESM1-2-HR/historical/r1i1p1f1/tasmax/tasmax_day_MPI-ESM1-2-HR_historical_r1i1p1f1_gn_${year}.nc
#cut out box
cdo -sellonlatbox,87.5,93,20,27 tasmax_day_MPI-ESM1-2-HR_historical_r1i1p1f1_gn_${year}.nc BangladeshBox_tasmax_day_MPI-ESM1-2-HR_historical_r1i1p1f1_gn_${year}.nc

#remove global data
rm tasmax_day_MPI-ESM1-2-HR_historical_r1i1p1f1_gn_${year}.nc
done

### Merge historical (1981-2014) and scenario (2015-2100) files
cdo mergetime BangladeshBox_tasmax_day_MPI-ESM1-2-HR_historical_r1i1p1f1_gn_????.nc BangladeshBox_tasmax_day_MPI-ESM1-2-HR_ssp585_r1i1p1f1_gn_????.nc BangladeshBox_tasmax_day_MPI-ESM1-2-HR_ssp585_r1i1p1f1_1981-2100.nc

## Calculate heatdays
cdo "expr,heatday=(tasmax-273.15)>=36;" BangladeshBox_tasmax_day_MPI-ESM1-2-HR_ssp585_r1i1p1f1_1981-2100.nc heatdays_BangladeshBox_MPI-ESM1-2-HR_ssp585_r1i1p1f1_1981-2100.nc

## Calculate monthly mean frequency of heatdays
cdo monmean heatdays_BangladeshBox_MPI-ESM1-2-HR_ssp585_r1i1p1f1_1981-2100.nc monmean_heatdays_BangladeshBox_MPI-ESM1-2-HR_ssp585_r1i1p1f1_1981-2100.nc

## Split into single months, addin names of the months to the filenames (%B)
cdo splitmon,%B monmean_heatdays_BangladeshBox_MPI-ESM1-2-HR_ssp585_r1i1p1f1_1981-2100.nc monmean_heatdays_BangladeshBox_MPI-ESM1-2-HR_ssp585_r1i1p1f1_1981-2100_


Calculate climatology 1991-2020 (current climate)
cdo -ymonmean -selyear,1991/2020 monmean_heatdays_BangladeshBox_MPI-ESM1-2-HR_ssp585_r1i1p1f1_1981-2100.nc ymonmean_heatdays_BangladeshBox_MPI-ESM1-2-HR_ssp585_r1i1p1f1_1991-2020.nc

Calculate climatology 2071-2100 (future climate)
cdo -ymonmean -selyear,2071/2100 monmean_heatdays_BangladeshBox_MPI-ESM1-2-HR_ssp585_r1i1p1f1_1981-2100.nc ymonmean_heatdays_BangladeshBox_MPI-ESM1-2-HR_ssp585_r1i1p1f1_2071-2100.nc
