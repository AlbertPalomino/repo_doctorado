clear all; close all; clc
%% ULTIMA ACTUALIZACION 2024-12-04 18:00:00

cd /home/ddonoso/Desktop/DoctoradoDefinitivo
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

csvfile = "/home/ddonoso/Desktop/DoctoradoDefinitivo/station_locations.csv";
points = readtable(csvfile);

variables = {'u10','v10','d2m','t2m','msl','tp','i10fg','sf'}; %editar según nombre de era5
time_series_data = cell(1, numel(variables));

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

% FOR WEATHER STATIONS INSTEAD OF COLONIES
%
colonies = [];
for i = 1:size(points,1)
    
    target_lat = table2array(points(i,2));
    target_long = table2array(points(i,3));
    
    distances = sqrt((new_lat - target_lat).^2 + (new_lon - target_long).^2);
    
    J = find(distances > 84);
    distances(J) = NaN;
    
    [~, min_index] = min(distances(:));
    
    [idx,idy] = ind2sub(size(distances),min_index);
    
    nearest_lat = y(idx,idy);
    nearest_lon = x(idx,idy);
    
    colonies = [colonies; table(points.location(i),target_lat, target_long, nearest_lat, nearest_lon)];
    i
end


folderPath = '/home/ddonoso/Desktop/datos_Albert/era5'

for i = 1:size(colonies, 1)
    
    idx = find(lon == colonies.nearest_lon(i));
    idy = find(lat == colonies.nearest_lat(i));
    i
    kk = [];
    for j = 1:length(variables)
        
        var_series = [];
        time_series = [];
        
        for k = 1:length(file_list)
            
            data_at_coord = squeeze(ncread(ncfile, variables{j}, [idx, idy, 1], [1 , 1, Inf]));
            
            var_series = [var_series; data_at_coord]; % ';' añade data_at_coord debajo de obs anteriores. ',' las añade en nueva columna
            
            time = ncread(ncfile, 'valid_time');
            time_series = [time_series; time];
            
        end
        kk = [kk, var_series];
    end
    kk = [kk, time_series];
    
    
    kk = array2table(kk);
    location = table2array(colonies(i,1));
    % time_series
    fn = sprintf ('%s/datos%s.csv', folderPath, string(colonies.Var1(i)));
    writetable(kk, fn);
end

% Plot map with colonies and nearest ERA5Land coords
figure
m_proj('lambert','long',[-69 -53],'lat',[-69 -60],'rectbox','on');
m_grid('linestyle','none','ytick',[],'xtick',[]);
m_gshhs_f('patch',[.5 .5 .5],'edgecolor','none','facealpha',0.5);
hold on
% m_plot(lonBMin,latBMin,'k')
m_plot(table2array(colonies(:,5)),table2array(colonies(:,4)),'.k','markersize',15); hold on
m_plot(table2array(colonies(:,3)),table2array(colonies(:,2)),'.r','markersize',11)


