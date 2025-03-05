clear all; close all; clc

cd /media/ddonoso/KINGSTON/

addpath(genpath('/media/ddonoso/KINGSTON/era5/'))
ruta = '/media/ddonoso/KINGSTON/era5/';

addpath(genpath('/media/ddonoso/KINGSTON/era5land_fossilbluff/'))
ruta = '/media/ddonoso/KINGSTON/era5land_fossilbluff/';

addpath(genpath('/media/ddonoso/KINGSTON/era5land/'))
ruta = '/media/ddonoso/KINGSTON/era5land/';  % modificar ruta

data = dir(fullfile(ruta, '*.csv'));

mkdir([ruta, 'monthly']); % Create folder for monthly data
mkdir([ruta, 'daily']);   % Create folder for daily data

for i = 1:length(data)
    name = data(i).name;
    mat = readtable(fullfile(ruta, name)); % Ensure full path is used
    time = mat.date;
    
    % Convert time from Unix format, Adjust refernce year and seconds/hours
    Td = datetime(1970,1,1) + seconds(time); % ERA5 and ERA5-Land fossilbluff
    %Td = datetime(1900,1,1) + hours(time); % ERA5-Land

    Td.Format = 'yyyy-MM-dd HH:mm:ss';
    mat.date = Td;
    clear time Td
    
    pos = find(mat.date>=datetime('2013-10-12 00:00:00','InputFormat','yyyy-MM-dd HH:mm:ss') & mat.date<datetime('2014-11-30 00:00:00','InputFormat','yyyy-MM-dd HH:mm:ss') ...
        |    mat.date>=datetime('2017-07-28 00:00:00','InputFormat','yyyy-MM-dd HH:mm:ss') & mat.date<datetime('2019-05-15 00:00:00','InputFormat','yyyy-MM-dd HH:mm:ss') ... % start at 2017-07-28, otherwise n days of raw data > n days every 3h
        |   mat.date>=datetime('2021-06-08 00:00:00','InputFormat','yyyy-MM-dd HH:mm:ss') & mat.date<datetime('2022-09-23 00:00:00','InputFormat','yyyy-MM-dd HH:mm:ss'));
    
    mat = mat(pos,:);
    
    % 🔹 Define variables to sum and average
    vars_suma = {'prec', 'snowfall'};
    vars_promedio = setdiff(mat.Properties.VariableNames, [{'date'}, vars_suma]); % Exclude 'prec' and 'snowfall'

    mat.Day = day(mat.date);
    mat.Month = month(mat.date);
    mat.Year = year(mat.date);

    yy = unique(mat.Year);

    daily_sums_all = [];
    monthly_sums_all = [];

    for k = 1:length(yy)
        mat_year = mat(mat.Year == yy(k), :);

        % 🔹 Compute daily sums (before filtering)
        daily_sum_k = varfun(@sum, mat_year, 'GroupingVariables', {'Year', 'Month', 'Day'}, 'InputVariables', vars_suma);
        daily_sum_k(:, 1:4) = []; % Remove count column
        daily_sum_k.Properties.VariableNames = strrep(daily_sum_k.Properties.VariableNames, 'sum_', '');

        % 🔹 Compute monthly sums (before filtering)
        monthly_sum_k = varfun(@sum, mat_year, 'GroupingVariables', {'Year', 'Month'}, 'InputVariables', vars_suma);
        monthly_sum_k(:, 1:3) = []; % Remove count column
        monthly_sum_k.Properties.VariableNames = strrep(monthly_sum_k.Properties.VariableNames, 'sum_', '');

        daily_sums_all = [daily_sums_all; daily_sum_k];
        monthly_sums_all = [monthly_sums_all; monthly_sum_k];
    end

    % 🔹 Apply Hour Filter
    horas_deseadas = [0, 3, 6, 9, 12, 15, 18, 21];
    horas = hour(mat.date);
    filtro_horas = ismember(horas, horas_deseadas);
    mat = mat(filtro_horas, :);

    % 🔹 Compute daily and monthly means AFTER filtering
    daily_means_all = [];
    monthly_means_all = [];

    for k = 1:length(yy)
        mat_year = mat(mat.Year == yy(k), :);

        % Compute daily means (only filtered variables)
        daily_mean_k = varfun(@mean, mat_year, 'GroupingVariables', {'Year', 'Month', 'Day'}, 'InputVariables', vars_promedio);
        daily_mean_k(:, 4) = []; % Remove count column
        daily_mean_k.Properties.VariableNames = strrep(daily_mean_k.Properties.VariableNames, 'mean_', '');

        % Compute monthly means (only filtered variables)
        monthly_mean_k = varfun(@mean, mat_year, 'GroupingVariables', {'Year', 'Month'}, 'InputVariables', vars_promedio);
        monthly_mean_k(:, 3) = []; % Remove count column
        monthly_mean_k.Properties.VariableNames = strrep(monthly_mean_k.Properties.VariableNames, 'mean_', '');

        daily_means_all = [daily_means_all; daily_mean_k];
        monthly_means_all = [monthly_means_all; monthly_mean_k];
    end

    % 🔹 Merge summed and averaged values
    daily = [daily_means_all, daily_sums_all]; % Combine daily means and daily sums
    monthly = [monthly_means_all, monthly_sums_all]; % Combine monthly means and monthly sums

    % 🔹 Save final daily & monthly matrices
    writetable(daily, fullfile(ruta, 'daily', name));
    writetable(monthly, fullfile(ruta, 'monthly', name));
end

restoredefaultpath; savepath;
