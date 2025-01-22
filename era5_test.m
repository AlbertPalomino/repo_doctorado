cd /home/ddonoso/Desktop/repo_doctorado
addpath(genpath('/home/ddonoso/Desktop/datos_Albert/era5/'))

file = dir('/home/ddonoso/Downloads/era5_2013.nc');
ncinfo(file);
ncdisp(file.name);