clear all; close all; clc
%%% ULTIMA ACTUALIZACION 2024-12-04 18:00:00

cd /home/ddonoso/Desktop/repo_doctorado
addpath(genpath('/home/ddonoso/Desktop/datos_Albert/era5/'))
addpath(genpath('/home/ddonoso/Desktop/datos_Albert/Toolbox_oce/'))
addpath(genpath('/home/ddonoso/Desktop/datos_Albert/Toolbox_oce/funciones_matlab/m_map1.4/m_map'))

folder = '/home/ddonoso/Desktop/datos_Albert/era5/';

file_list = dir(fullfile(folder, '*.nc'));
ncfile = file_list(1).name;
ncdisp(ncfile) % read info in file
% missingValue= -32767;

lat = ncread(ncfile, 'latitude');
lon = ncread (ncfile, 'longitude');
[x,y] = meshgrid(lon,lat); 

csvfile = "/home/ddonoso/Desktop/repo_doctorado/station_locations.csv";
points = readtable(csvfile);

variables = {'u10','v10','d2m','t2m','msl','tp','i10fg','sf'}; %editar según nombre de era5
time_series_data = cell(1, numel(variables));

% CREATE MASK WITH EXISTING GRID POINTS
%
nan_info = true(61,43); % 2D array with 0 or 1, representing NaN or non-NaNs in ncfile. MODIFICAR LAS DIMENSIONES DE LA GRILLA
u10 = ncread(ncfile,'u10');
for i = 1:43
    for j = 1:61
        if any(isnan(u10(j, i, :)))
            nan_info(j, i) = false;
        end
    end
end
clear u10  i j

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

% EXTRACT TIME SERIES AT EACH GRID POINT
folderPath = '/home/ddonoso/Desktop/datos_Albert/era5'

% Save station and era5 coordinates as csv
    fn = sprintf ('%s/coords.csv', folderPath);
    writetable(stations, fn);

for i = 1%:size(stations, 1)
    
    idx = find(lon == stations.era5_lon(i));
    idy = find(lat == stations.era5_lat(i));
    %i
    %kk = [];
    var_series = [];
    time_series = [];
        
    for j = 1:length(variables)
        %     valid_time
%            Size:       8760x1
%            Dimensions: valid_time
%            Datatype:   int64
%            Attributes:
%                        long_name     = 'time'
%                        standard_name = 'time'
%                        units         = 'seconds since 1970-01-01'
%                        calendar      = 'proleptic_gregorian'

        %var_series = [];
        %time_series = [];
        cat_var_series = [];
        cat_time_series = [];
        
        for k = 1:2%:length(file_list)
            
            data_at_coord = squeeze(ncread(ncfile, variables{j}, [idx, idy, 1], [1 , 1, Inf]));
            
            %var_series = [var_series; data_at_coord]; % ';' añade data_at_coord debajo de obs anteriores. ',' las añade en nueva columna
            cat_var_series = [cat_var_series; data_at_coord]; % ';' añade data_at_coord debajo de obs anteriores. ',' las añade en nueva columna
            
            time = ncread(ncfile, 'valid_time'); % 8760 x 1
            
            % seconds since 1970-01-01
            n = datenum(1970,01,01,0,0,double(time));
            datestr(n)
            
            %time_series = [time_series; time];
            cat_time_series = [cat_time_series; time]; %17520
            
            % 19, 11, 26
            [i,j,k]
            
        end
        %kk = [kk, var_series];
        var_series = [var_series, cat_var_series];
        
    end
    %kk = [kk, time_series];    
    %kk = array2table(kk);
    %location = table2array(stations(i,1));
    % time_series
    %fn = sprintf ('%s/datos_%s.csv', folderPath, string(stations.Var1(i)));
    %writetable(kk, fn);
    
    var_series = [var_series, cat_time_series];
    
    %size(var_series)
    
    %out_var_series = array2table(var_series);
    %fn = sprintf ('%s/datos_%s.csv', folderPath, string(stations.Var1(i)));
    %writetable(out_var_series,fn);
    
end

%     valid_time
%            Size:       8760x1
%            Dimensions: valid_time
%            Datatype:   int64
%            Attributes:
%                        long_name     = 'time'
%                        standard_name = 'time'
%                        units         = 'seconds since 1970-01-01'
%                        calendar      = 'proleptic_gregorian'
                       
                       
return

% Plot map with stations and nearest ERA5 grid point
figure
m_proj('lambert','long',[-69 -53],'lat',[-69 -60],'rectbox','on');
m_grid('linestyle','none','ytick',[],'xtick',[]);
m_gshhs_f('patch',[.5 .5 .5],'edgecolor','none','facealpha',0.5);
hold on
% m_plot(lonBMin,latBMin,'k')
m_plot(table2array(stations(:,5)),table2array(stations(:,4)),'.k','markersize',15); hold on
m_plot(table2array(stations(:,3)),table2array(stations(:,2)),'.r','markersize',11)


