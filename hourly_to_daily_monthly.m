clear all; close all; clc

cd /media/ddonoso/KINGSTON/

addpath(genpath('/media/ddonoso/KINGSTON/era5/'))
ruta = '/media/ddonoso/KINGSTON/era5/';

addpath(genpath('/media/ddonoso/KINGSTON/era5land_fossilbluff/'))
ruta = '/media/ddonoso/KINGSTON/era5land_fossilbluff/';

addpath(genpath('/media/ddonoso/KINGSTON/era5land/'))
ruta = '/media/ddonoso/KINGSTON/era5land/';  % modificar ruta

addpath(genpath('/media/ddonoso/KINGSTON/racmo5.5_2016/'))
ruta = '/media/ddonoso/KINGSTON/racmo5.5_2016/'


data = dir(fullfile(ruta, '*.csv'));

mkdir([ruta, 'monthly']); % Create folder for monthly data
mkdir([ruta, 'daily']);   % Create folder for daily data

for i = 1:length(data)
    name = data(i).name;
    mat = readtable(fullfile(ruta, name)); % Ensure full path is used
    time = mat.date;
    
    % Convert time from Unix format
    Td = datetime(1950,1,1) + hours(time); % Adjust refernce year and seconds/hours
    Td.Format = 'yyyy-MM-dd HH:mm:ss';
    mat.date = Td;
    clear time Td

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

        % 🔹 Compute monthly sums (before filtering)
        monthly_sum_k = varfun(@sum, mat_year, 'GroupingVariables', {'Year', 'Month'}, 'InputVariables', vars_suma);
        monthly_sum_k(:, 1:3) = []; % Remove count column

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

        % Compute monthly means (only filtered variables)
        monthly_mean_k = varfun(@mean, mat_year, 'GroupingVariables', {'Year', 'Month'}, 'InputVariables', vars_promedio);
        monthly_mean_k(:, 3) = []; % Remove count column

        daily_means_all = [daily_means_all; daily_mean_k];
        monthly_means_all = [monthly_means_all; monthly_mean_k];
    end

    % 🔹 Merge summed and averaged values
    final_daily = [daily_means_all, daily_sums_all]; % Combine daily means and daily sums
    final_monthly = [monthly_means_all, monthly_sums_all]; % Combine monthly means and monthly sums

    % 🔹 Save final daily & monthly matrices
    writetable(final_daily, fullfile(ruta, 'daily', name));
    writetable(final_monthly, fullfile(ruta, 'monthly', name));
end

restoredefaultpath; savepath;
