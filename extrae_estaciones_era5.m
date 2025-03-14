clear all; close all; clc
%%% ULTIMA ACTUALIZACION 2024-12-04 18:00:00

cd /media/ddonoso/KINGSTON/era5
%cd /home/ddonoso/Desktop/datos_Albert/era5

addpath(genpath('/media/ddonoso/KINGSTON/era5'))
addpath(genpath('/home/ddonoso/Desktop/datos_Albert/Toolbox_oce/'))
addpath(genpath('/home/ddonoso/Desktop/datos_Albert/Toolbox_oce/funciones_matlab/m_map1.4/m_map'))

folder = '/media/ddonoso/KINGSTON/era5/';

file_list = dir(fullfile(folder, '*instant*.nc'));
file_list = dir(fullfile(folder, '*accum*.nc'));
%file_list = dir(fullfile(folder, '*.nc'));

ncfile = file_list(1).name;
ncdisp(ncfile) % read info in file
% missingValue= -32767;

lat = ncread(ncfile, 'latitude');
lon = ncread (ncfile, 'longitude');
[x,y] = meshgrid(lon,lat); 

csvfile = "/home/ddonoso/Desktop/repo_doctorado/station_locations.csv";
points = readtable(csvfile);

%variables = {'u10','v10','d2m','t2m','msl','tp','i10fg','sf'}; %variables in ERA5
variables = {'u10','v10','d2m','t2m','msl','i10fg'}; %variables in ERA5
variables =  {'tp','sf'};

time_series_data = cell(1, numel(variables));

% CREATE MASK WITH EXISTING GRID POINTS

nan_info = true(61,43); % 2D array with 0 or 1, representing NaN or non-NaNs in ncfile. MODIFICAR LAS DIMENSIONES DE LA GRILLA
var = ncread(ncfile,'tp');
for i = 1:43
    for j = 1:61
        if any(isnan(var(j, i, :)))
            nan_info(j, i) = false;
        end
    end
end
clear var i j

imagesc(nan_info'); % plots cells with and without data in data.nc, transposing x and y axes

new_lon = x.* nan_info'; % applydata = ncread(ncfile, variables{i}); nan_info mask to x and y
new_lat = y.* nan_info';
figure,plot(new_lon, new_lat,'*')

% FIND NEAREST GRID POINT TO EACH STATION
%
stations = [];
for i = 1:size(points,1)
    
    target_lat = table2array(points(i,2));
    target_long = table2array(points(i,3));
    
    distances = sqrt((new_lat - target_lat).^2 + (new_lon - target_long).^2);
    
    J = find(distances > 84);
    distances(J) = NaN;
    
    [~, min_index] = min(distances(:));
    
    [idx,idy] = ind2sub(size(distances),min_index);
    
    era5_lat = y(idx,idy);
    era5_lon = x(idx,idy);
    
    stations = [stations; table(points.location(i),target_lat, target_long, era5_lat, era5_lon)];
    
end

folderPath = '/media/ddonoso/KINGSTON/era5/';

% Save station and era5 coordinates as csv
    fn = sprintf ('%s/coords.csv', folderPath);
    writetable(stations, fn);

% EXTRACT TIME SERIES AT EACH GRID POINT

%     valid_time
%            Size:       8760x1
%            Dimensions: valid_time
%            Datatype:   int64
%            Attributes:
%                        long_name     = 'time'
%                        standard_name = 'time'
%                        units         = 'seconds since 1970-01-01'
%                        calendar      = 'proleptic_gregorian'

var_list1 = {}; % Cell array to store tables for each iteration
var_list2 = {};

for i = 1:size(stations, 1)
    
    idx = find(lon == stations.era5_lon(i));
    idy = find(lat == stations.era5_lat(i));

    var_series = [];
    time_series = [];
        
    for j = 1:length(variables)
        
        cat_var_series = [];
        cat_time_series = [];
        
        for k = 1:length(file_list)
            
            ncfile = file_list(k).name;

            data_at_coord = squeeze(ncread(ncfile, variables{j}, [idx, idy, 1], [1 , 1, Inf]));
            cat_var_series = [cat_var_series; data_at_coord]; % ; añade data_at_coord debajo de obs anteriores/ , las añade en nueva columna
            
            time = ncread(ncfile, 'valid_time'); % 8760 x 1
            
            %seconds since 1970-01-01
            %n = datenum(1970,01,01,0,0,double(time));
            %datestr(n)
            
            cat_time_series = [cat_time_series; time]; %17520
            
            [i,j,k]
            
        end
        var_series = [var_series, cat_var_series];
        
    end
    var_series2 = [var_series, double(cat_time_series)];
    var_table = array2table(var_series2);
    
    var_table.Properties.VariableNames = {'u10','v10','dew','temp','pres','gust','date'}; % Rename columns
    %var_table.Properties.VariableNames = {'prec','snowfall','date'};

    var_list1{i} = var_table;  % Store each table in a cell array
    %var_list2{i} = var_table;
    
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
plot(combined_var_series{20}{:, 9}, combined_var_series{20}{:,8}, '-o');

 var_list{i}.var3
datetick('x', 'yyyy-mm-dd HH:MM', 'keepticks');
ylabel('u10 (m/s)');
title('Time Series Plot');