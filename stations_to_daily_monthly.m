clear all; close all; clc

cd /media/ddonoso/KINGSTON/

addpath(genpath('/media/ddonoso/KINGSTON/station_series/'))
ruta = '/media/ddonoso/KINGSTON/station_series/';

data = dir(fullfile(ruta, 'validation*.csv'));

mkdir([ruta, 'monthly']); % Create folder for monthly data
mkdir([ruta, 'daily']);   % Create folder for daily data

for i =1:length(data)
    name = data(i).name;
    mat = readtable(fullfile(ruta, name)); % Ensure full path is used
    
    % Add empty u and v columns
    mat.u = NaN(height(mat), 1);
    mat.v = NaN(height(mat), 1);

    % 🔹 Define variables to sum and average
    vars_suma = {'prec', 'snowfall'};
    vars_promedio = setdiff(mat.Properties.VariableNames, [{'date'}, vars_suma]); % Exclude 'prec' and 'snowfall'

    mat.Day = day(mat.date);
    mat.Month = month(mat.date);
    mat.Year = year(mat.date);

    yy = unique(mat.Year);
    
    % Convert to double the columns that aren't already
    for j = 1:length(vars_promedio)
        var_name = vars_promedio{j};
        if iscell(mat.(var_name)) || ischar(mat.(var_name)) % If not double
            mat.(var_name) = str2double(mat.(var_name)); % Convert to double
        end
    end
    
    % Calculate u and v wind vectors
    mat.u = - mat.vel .* sind(mat.dir);
    mat.v = - mat.vel .* cosd(mat.dir);
 
    pos = find(mat.date>=datetime('2013-10-12 00:00:00','InputFormat','yyyy-MM-dd HH:mm:ss') & mat.date<datetime('2014-11-30 00:00:00','InputFormat','yyyy-MM-dd HH:mm:ss') ...
        |    mat.date>=datetime('2017-07-28 00:00:00','InputFormat','yyyy-MM-dd HH:mm:ss') & mat.date<datetime('2019-05-15 00:00:00','InputFormat','yyyy-MM-dd HH:mm:ss') ... % start at 2017-07-28, otherwise n days of raw data > n days every 3h
        |   mat.date>=datetime('2021-06-08 00:00:00','InputFormat','yyyy-MM-dd HH:mm:ss') & mat.date<datetime('2022-09-23 00:00:00','InputFormat','yyyy-MM-dd HH:mm:ss'));
    
    mat = mat(pos,:);
    
    % 🔹 Compute daily and monthly means AFTER filtering
    daily = [];
    monthly = [];

    for k = 1:length(yy)
        
        mat_year = mat(mat.Year == yy(k), :);

        % Compute daily means (only filtered variables)
        daily_mean_k = varfun(@(x) nanmean(x, 1), mat_year, 'GroupingVariables', {'Year', 'Month', 'Day'}, 'InputVariables', vars_promedio); % nanmean ignores NaNs when calculating means
       % daily_mean_k = varfun(@mean, mat_year, 'GroupingVariables', {'Year', 'Month', 'Day'}, 'InputVariables', vars_promedio);
        daily_mean_k(:, 4) = []; % Remove count column
        daily_mean_k.Properties.VariableNames = strrep(daily_mean_k.Properties.VariableNames, 'Fun_', '');

        % Compute monthly means (only filtered variables)
        monthly_mean_k = varfun(@(x) nanmean(x, 1), mat_year, 'GroupingVariables', {'Year', 'Month'}, 'InputVariables', vars_promedio);
        monthly_mean_k(:, 3) = []; % Remove count column
        monthly_mean_k.Properties.VariableNames = strrep(monthly_mean_k.Properties.VariableNames, 'Fun_', '');

        daily = [daily; daily_mean_k];
        monthly = [monthly; monthly_mean_k];
        
    end

    % 🔹 Merge summed and averaged values, not necessary anymore
    %final_daily = [daily]; % Combine daily means and daily sums
    %final_monthly = [monthly]; % Combine monthly means and monthly sums

    % Backtransform, to ensure u and v are correctly calculated
    daily.vel_uv = sqrt(daily.u.^2 + daily.v.^2); % Compute wind speed
    daily.dir_uv = atan2d(-daily.u, -daily.v);    % Compute wind direction
    daily.dir_uv(daily.dir_uv < 0) = daily.dir_uv(daily.dir_uv < 0) + 360;
    
    % Backtransform, to ensure u and v are correctly calculated
    monthly.vel_uv = sqrt(monthly.u.^2 + monthly.v.^2); % Compute wind speed
    monthly.dir_uv = atan2d(-monthly.u, -monthly.v);    % Compute wind direction
    monthly.dir_uv(monthly.dir_uv < 0) = monthly.dir_uv(monthly.dir_uv < 0) + 360;
    
    
    % 🔹 Save final daily & monthly matrices
    writetable(daily, fullfile(ruta, 'daily', name));
    writetable(monthly, fullfile(ruta, 'monthly', name));
    
end

restoredefaultpath; savepath;
