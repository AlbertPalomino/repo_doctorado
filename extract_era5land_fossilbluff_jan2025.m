clear all; close all; clc

cd /media/ddonoso/KINGSTON/era5land

folder = '/media/ddonoso/KINGSTON/era5land';
%folder = '/home/ddonoso/Desktop/datos_Albert/era5land_fossilbluff';

file_list = dir(fullfile(folder, '*.nc'));

%addpath(genpath('/home/ddonoso/Desktop/datos_Albert/era5land_fossilbluff'))
addpath(genpath('/media/ddonoso/KINGSTON/era5land'))
addpath(genpath('/home/ddonoso/Desktop/datos_Albert/Toolbox_oce/'))
addpath(genpath('/home/ddonoso/Desktop/datos_Albert/Toolbox_oce/funciones_matlab/m_map1.4/m_map'))

ncfile = file_list(10).name;
ncdisp(ncfile) 

lat = ncread(ncfile, 'latitude');
lon = ncread (ncfile, 'longitude');
[x,y] = meshgrid(lon,lat); 

%variables = {'d2m','t2m','sf','u10','v10','sp','tp'};
variables = {'u10','v10','d2m','t2m','skt','snowc','sde','sf','sp','tp'};

time_series_data = cell(1, numel(variables));

nan_info = true(161, 91); % 2D array with 0 or 1, representing NaN or non-NaNs in ncfile.
var = ncread(ncfile,'u10');
for i = 1:91
    for j = 1:161
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

csvfile = "/home/ddonoso/Desktop/repo_doctorado/station_locations.csv";
points = readtable(csvfile);

stations = [];

for i = 1:size(points,1)
    
    target_lat = table2array(points(i,2));
    target_long = table2array(points(i,3));
    
    distances = sqrt((new_lat - target_lat).^2 + (new_lon - target_long).^2);
    
    J = find(distances > 84);
    distances(J) = NaN;
    
    [~, min_index] = min(distances(:));
    
    [idx,idy] = ind2sub(size(distances),min_index);
    
    era5land_lat = y(idx,idy);
    era5land_lon = x(idx,idy);
    
    stations = [stations; table(points.location(i),target_lat, target_long, era5land_lat, era5land_lon)];
    
end

% Save station and grid coordinates as csv
fn = sprintf ('%s/coords.csv', folder);
writetable(stations, fn);


% EXTRACT TIME SERIES AT FOSSIL BLUFF'S NEAREST GRID POINT

for i = 20%1:size(stations, 1)
    
    idx = find(lon == stations.era5land_lon(i));
    idy = find(lat == stations.era5land_lat(i));

    var_series = [];
    time_series = [];
        
    for j = 1:length(variables)
        
        cat_var_series = [];
        cat_time_series = [];
        
        for k = 1:length(file_list)
            
            ncfile = file_list(k).name;

            data_at_coord = squeeze(ncread(ncfile, variables{j}, [idx, idy, 1], [1 , 1, Inf]));
            cat_var_series = [cat_var_series; data_at_coord]; % ; añade data_at_coord debajo de obs anteriores/ , las añade en nueva columna
            
            %time = ncread(ncfile, 'valid_time');
            time = ncread(ncfile, 'time');
            
            cat_time_series = [cat_time_series; time];
            
            [i,j,k]
            
        end
        var_series = [var_series, cat_var_series];
        
    end
    var_series2 = [var_series, double(cat_time_series)];
    
    var_table = array2table(var_series2);
    
    % var_table.Properties.VariableNames = {'dew','temp','snowfall','u10','v10','pres','prec','date'}; % Rename columns
    var_table.Properties.VariableNames = {'u10','v10','dew','temp','skt','snowc','sde','snowfall','pres','prec','date'};
    
    %var_list2{i} = var_table;  % Store each table in a cell array
    
    fn = sprintf ('%s/era5land_%s.csv', folder, string(stations.Var1(i)));
    writetable(var_table, fn);
            
end                      


% Plot tests to check if data is correctly extracted

plot(var_series2(:, 11), var_series2(:, 4), '-o');

% Plot map with colonies and nearest ERA5Land coords
figure
m_proj('lambert','long',[-69 -53],'lat',[-69 -60],'rectbox','on');
m_grid('linestyle','none','ytick',[],'xtick',[]);
m_gshhs_f('patch',[.5 .5 .5],'edgecolor','none','facealpha',0.5);
hold on
% m_plot(lonBMin,latBMin,'k')
m_plot(table2array(colonies(:,4)),table2array(colonies(:,3)),'.k','markersize',15); hold on
m_plot(table2array(colonies(:,2)),table2array(colonies(:,1)),'.r','markersize',11)
