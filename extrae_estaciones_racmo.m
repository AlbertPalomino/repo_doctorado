clear all; close all; clc
%%% ULTIMA ACTUALIZACION 3 febrero 2025

cd /media/ddonoso/KINGSTON/racmo11

addpath(genpath('/media/ddonoso/KINGSTON/racmo11/'))
addpath(genpath('/home/ddonoso/Desktop/datos_Albert/Toolbox_oce/'))
addpath(genpath('/home/ddonoso/Desktop/datos_Albert/Toolbox_oce/funciones_matlab/m_map1.4/m_map'))

folder = '/media/ddonoso/KINGSTON/racmo11/';

file_list = dir(fullfile(folder, '*.nc'));

ncfile = file_list(1).name;
ncdisp(ncfile) % read info in file
% missingValue= -32767;

% Subset lat and lon to extract fewer data
lat = ncread(ncfile, 'lat');
lon = ncread (ncfile, 'lon');


% bounding box
lat_min = -72; lat_max = -61;
lon_min = -70; lon_max = -55;

% Convert longitudes if needed (handle 0-360° notation)
if any(lon(:) > 180)
    lon(lon > 180) = lon(lon > 180) - 360;
end

% Create a logical mask for the polygon (select points inside bounds)
mask = (lat >= lat_min & lat <= lat_max) & (lon >= lon_min & lon <= lon_max);

% Find the indices of selected points
[row_idx, col_idx] = find(mask);

% Extract the corresponding lat/lon values
lat_subset = lat(mask);
lon_subset = lon(mask);


lat_idx = find(lat >= -71.5 & lat <= -62);
lon_idx = find(lon >= -70 & lon <= -58);

lat_subset = lat(lat_idx);
lon_subset = lon(lon_idx);

[x,y] = meshgrid(lon_subset,lat_subset); 
[x,y] = ndgrid(lon,lat); 


% TRANSFORM ROTATED POLAR COORDINATES
% Load NetCDF file
ncfile = 'your_file.nc';

% Read rotated coordinates
rlat = ncread(ncfile, 'rlat');  % Rotated latitude (2D or 1D)
rlon = ncread(ncfile, 'rlon');  % Rotated longitude (2D or 1D)

% Read rotated pole metadata (check your NetCDF file for correct variable names)
rotated_pole_lat = ncreadatt(ncfile, '/', 'rotated_pole_latitude'); % e.g., -39.25
rotated_pole_lon = ncreadatt(ncfile, '/', 'rotated_pole_longitude'); % e.g., 162

% Convert degrees to radians
rlat_rad = deg2rad(rlat);
rlon_rad = deg2rad(rlon);
theta_p = deg2rad(rotated_pole_lat);
lambda_0 = deg2rad(rotated_pole_lon);

% Perform coordinate transformation
sin_phi = sin(rlat_rad) .* cos(theta_p) + cos(rlat_rad) .* sin(theta_p) .* cos(rlon_rad);
phi = asin(sin_phi); % Standard latitude

lambda = lambda_0 + atan2( cos(rlat_rad) .* sin(rlon_rad), ...
                          cos(rlat_rad) .* cos(theta_p) .* cos(rlon_rad) - sin(rlat_rad) .* sin(theta_p));

% Convert back to degrees
lat = rad2deg(phi);
lon = rad2deg(lambda);

% Adjust longitudes to -180 to 180 range if necessary
lon(lon > 180) = lon(lon > 180) - 360;

% Now lat/lon are in the standard geographic coordinate system



csvfile = "/home/ddonoso/Desktop/repo_doctorado/station_locations.csv";
points = readtable(csvfile);

%variables = {'u10','v10','d2m','t2m','msl','tp','i10fg','sf'}; %variables in ERA5
variables = {'u10','v10','d2m','t2m','msl','i10fg'}; %variables in ERA5
variables =  {'tp','sf'};

time_series_data = cell(1, numel(variables));

% CREATE MASK WITH EXISTING GRID POINTS

nan_info = true(6574,6574); % 2D array with 0 or 1, representing NaN or non-NaNs in ncfile. MODIFICAR LAS DIMENSIONES DE LA GRILLA
var = ncread(ncfile,'hurs');
for i = 1:6574
    for j = 1:6574
        if any(isnan(var(j, i, :)))
            nan_info(j, i) = false;
        end
    end
end
clear var i j

imagesc(nan_info'); % plots cells with and without data in data.nc, transposing x and y axes

new_lon = x.* nan_info'; % applydata = ncread(ncfile, variables{i}); nan_info mask to x and y
new_lat = y.* nan_info';
figure,plot(rlon, rlat,'*')

% FIND NEAREST GRID POINT TO EACH STATION
%
stations = [];
for i = 1:size(points,1)
    
    target_lat = table2array(points(i,2));
    target_long = table2array(points(i,3));
    
    distances = sqrt((lat - target_lat).^2 + (lon - target_long).^2);
    
    J = find(distances > 84);
    distances(J) = NaN;
    
    [~, min_index] = min(distances(:));
    
    [idx,idy] = ind2sub(size(distances),min_index);
    
    racmo_lat = lat(idx,idy);
    racmo_lon = lon(idx,idy);
    
    stations = [stations; table(points.location(i), target_lat, target_long, racmo_lat, racmo_lon, idy,idx)];
    
end

folderPath = '/home/ddonoso/Desktop/datos_Albert/era5/downloads/';

% Save station and era5 coordinates as csv
    fn = sprintf ('%s/coords.csv', folderPath);
    writetable(stations, fn);

% EXTRACT TIME SERIES AT EACH GRID POINT

var_list1 = {}; % Cell array to store tables for each iteration
var_list2 = {};

for i = 1:size(stations, 1)
    
    %idx = find(lon == stations.racmo_lon(i));
    %idy = find(lat == stations.racmo_lat(i));    
    
    idx = stations.idx(i);
    idy = stations.idy(i);

    var_series = [];
    time_series = [];
        
    for j = 1:length(variables)
        
        cat_var_series = [];
        cat_time_series = [];
        
        for k = 1:length(file_list)
            
            ncfile = file_list(k).name;

            data_at_coord = squeeze(ncread(ncfile, 'hurs', [idx, idy, 1, 1], [1, 1, 1, Inf]));
            cat_var_series = [cat_var_series; data_at_coord]; % ; añade data_at_coord debajo de obs anteriores/ , las añade en nueva columna
            
            time = ncread(ncfile, 'time'); % 8760 x 1
            
            %seconds since 1970-01-01
            %n = datenum(1950,01,01,0,0,double(time));
            %datestr(n)
            ref_date = ;

% Convert to MATLAB datetime format
time_datetime = datetime(1950, 1, 1, 0, 0, 0) + days(time);

% Display first few dates
disp(time_datetime(1:10));
            
            cat_time_series = [cat_time_series; time]; %17520
            
            [i,j,k]
            
        end
        var_series = [var_series, cat_var_series];
        
    end
    var_series2 = [var_series, double(cat_time_series)];
    var_table = array2table(var_series2);
    
    %var_table.Properties.VariableNames = {'u10','v10','dew','temp','pres','gust','date'}; % Rename the columns
    var_table.Properties.VariableNames = {'prec','snowfall','date'}; % Rename the columns

    var_list2{i} = var_table;  % Store each table in a cell array
    
    %fn = sprintf ('%s/era5_%s.csv', folderPath, string(stations.Var1(i)));
    %writetable(var_series, fn);
            
end                      
                       
return

% COMBINE PAIRS OF TABLES (INSTANT AND ACCUM VARIABLES)

combined_var_series = {}; % Initialize a new cell array

for i = 1:min(length(var_list1), length(var_list2))
    
    var_list1{i}(:, strcmp(var_list1{i}.Properties.VariableNames, 'date')) = []; % Remove duplicate date column (from tables in var_list1)
    
    combined_table = [var_list1{i}, var_list2{i}]; % Combine element by element horizontally
    
    combined_var_series{i} = combined_table; % Store the combined table in the cell array
    
    fn = sprintf('%s/era5_%s.csv', folderPath, string(stations.Var1(i)));
    writetable(combined_var_series{i}, fn);
    
    i
end

% Plot map with stations and nearest ERA5 grid point
figure
m_proj('lambert','long',[-69 -53],'lat',[-69 -60],'rectbox','on');
m_grid('linestyle','none','ytick',[],'xtick',[]);
m_gshhs_f('patch',[.5 .5 .5],'edgecolor','none','facealpha',0.5);
hold on
% m_plot(lonBMin,latBMin,'k')
m_plot(table2array(stations(:,5)),table2array(stations(:,4)),'.k','markersize',15); hold on
m_plot(table2array(stations(:,3)),table2array(stations(:,2)),'.r','markersize',11)

plot(var_series(:, 3), var_series(:, 1), '-o');
plot(combined_var_series{1}{:, 9}, combined_var_series{1}{:, 7}, '-o');

 var_list{i}.var3
datetick('x', 'yyyy-mm-dd HH:MM', 'keepticks');
ylabel('u10 (m/s)');
title('Time Series Plot');