clear all; close all; clc
%%% ULTIMA ACTUALIZACION 4 febrero 2025

cd /media/ddonoso/KINGSTON/racmo5.5_2016

addpath(genpath('/media/ddonoso/KINGSTON/racmo5.5_2016/'))
addpath(genpath('/home/ddonoso/Desktop/datos_Albert/Toolbox_oce/'))
addpath(genpath('/home/ddonoso/Desktop/datos_Albert/Toolbox_oce/funciones_matlab/m_map1.4/m_map'))

folder = '/media/ddonoso/KINGSTON/racmo5.5_2016/';

file_list1 = dir(fullfile(folder, '*2016*.nc'));
file_list2 = dir(fullfile(folder, '*2021*.nc'));
file_lists = {file_list1, file_list2}; % Store both lists in a cell array

ncfile = file_lists{1}(1).name; 
ncdisp(ncfile)


% FIND NEAREST GRID POINT TO EACH STATION
% Same coordinates as for RACMO5.5

lat = ncread(ncfile, 'lat');
lon = ncread (ncfile, 'lon');

stations = [];

csvfile = "/home/ddonoso/Desktop/repo_doctorado/station_locations.csv";
points = readtable(csvfile);

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

% Save station and grid point coordinates as csv
% folder = '/media/ddonoso/KINGSTON/racmo11'; % Create new path if necessary

fn = sprintf ('%s/coords_racmo2016.csv', folder);
writetable(stations, fn);


% EXTRACT TIME SERIES AT EACH GRID POINT

var_list = {}; % Cell array to store tables for each iteration
var_list_struct = struct(); % Initialize a structure to store var_lists

for i = 1:2

current_list = file_lists{i} % Select a file list
    
   
    for j = 20%1:size(stations, 1)

    idx = stations.idx(j);
    idy = stations.idy(j);

    var_series = [];
              
        for k = 1:length(current_list)
            
            ncfile = current_list(k).name;
            info = ncinfo(ncfile);
            var_names = {info.Variables.Name}; % Extract variable names
            last_var = var_names{end}; % Get the last variable name

            variable_k = squeeze(ncread(ncfile, last_var, [idx, idy, 1, 1], [1, 1, 1, Inf]));            
                       
            var_series = [var_series, variable_k];

            [i,j,k]

        end
        
        time = ncread(ncfile, 'time');
        var_series2 = [var_series, double(time)];
        
        var_table = array2table(var_series2);
        
        var_table.Properties.VariableNames = {'vel','gust','pres','prec','surfpres','rh','snowfall','temp','u10','v10','date'}; % Rename the columns, pres refers to mean sea lvel pressure
        
        var_list{j} = var_table;  % Store each table in a cell array

    end
    
    var_list_struct.(['var_list' num2str(i)]) = var_list;
           
end                      
                       
return


% CONCATENATE PAIRS OF TABLES (2011 AND 2021 DATA)

var_list1 = var_list_struct.var_list1;
var_list2 = var_list_struct.var_list2;

concat_list = cell(size(var_list1));

for i = 20%1:length(var_list1)
    
    concat_table = [var_list1{i}; var_list2{i}]; 
    
    concat_list{i} = concat_table; % Store the concatenated table in the new cell array

    file_name = stations.Var1{i};
    
    file_name = [file_name '.csv']; % Append .csv
   
    writetable(concat_table, file_name);
    
    fprintf('Exported table %d to %s\n', i, file_name); % Display a message for confirmation
end



% Plot map with stations and nearest grid point

figure
m_proj('lambert','long',[-72 -53],'lat',[-73 -61],'rectbox','on');
m_grid('linestyle','none','ytick',[],'xtick',[]);
m_gshhs_f('patch',[.5 .5 .5],'edgecolor','none','facealpha',0.5);
hold on
% m_plot(lonBMin,latBMin,'k')
m_plot(table2array(stations(:,5)),table2array(stations(:,4)),'.r','markersize',15); 
hold on
m_plot(table2array(stations(:,3)),table2array(stations(:,2)),'.k','markersize',11)



% Plot tests - visualise time series
first_table = concat_list{20};
first_table.GregorianDate = datetime(1950, 1, 1, 0, 0, 0) + days(first_table{:, 11});

plot(first_table{:, 12}, first_table{:, 10}, '-o');

datetick('x', 'yyyy-mm-dd HH:MM', 'keepticks');
ylabel('u10 (m/s)');
title('Time Series Plot');
