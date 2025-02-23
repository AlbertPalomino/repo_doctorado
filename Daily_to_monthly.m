clear all; close all; clc

cd /media/ddonoso/KINGSTON/

addpath(genpath('/media/ddonoso/KINGSTON/racmo5.5_2016/'))
ruta = '/media/ddonoso/KINGSTON/racmo5.5_2016/';

addpath(genpath('/media/ddonoso/KINGSTON/racmo5.5/'))
ruta = '/media/ddonoso/KINGSTON/racmo5.5/';

addpath(genpath('/media/ddonoso/KINGSTON/racmo11/'))
ruta = '/media/ddonoso/KINGSTON/racmo11/';

data = dir(fullfile(ruta,'*.csv'));

mkdir([ruta,'monthly']); % crea una carpeta para los datos mensuales

for i = 1:length(data)
    name = data(i).name;
    mat = readtable(data(i).name);
    time = mat.date;
    
    Td = datetime(1950,1,1) + days(time); Td.Format = 'yyyy-MM-dd'; % modificar
    mat.date = Td; clear time Td
    
  pos = find(mat.date>=datetime('2013-10-12','InputFormat','yyyy-MM-dd') & mat.date<datetime('2014-11-30','InputFormat','yyyy-MM-dd') ...
      |   mat.date>=datetime('2017-07-28','InputFormat','yyyy-MM-dd') & mat.date<datetime('2019-05-15','InputFormat','yyyy-MM-dd') ...
      |   mat.date>=datetime('2021-06-08','InputFormat','yyyy-MM-dd') & mat.date<datetime('2022-09-23','InputFormat','yyyy-MM-dd'));
    
   mat = mat(pos,:);

    vars_promedio = setdiff(mat.Properties.VariableNames, {'prec',  'snowfall','date'}); % elimina la columna de fecha ; % crea celdas para proimediar y sumar
    vars_suma = {'prec',  'snowfall'};
    
    mat.Month = month(mat.date);  % crea la columa mes y año
    mat.Year = year(mat.date);
    yy = unique(mat.Year);
    
    monthly_mat = [];
    
    for k = 1:length(yy)

        mat_year = mat(mat.Year ==yy(k),:);
        
        monthly_mean = varfun(@mean, mat_year, 'GroupingVariables', {'Year','Month'}, 'InputVariables', vars_promedio);
        monthly_mean(:,3) = [];  % elimina contador de datos promediados
        monthly_mean.Properties.VariableNames = strrep(monthly_mean.Properties.VariableNames, 'mean_', '');

        monthly_sum = varfun(@sum, mat_year, 'GroupingVariables', {'Year','Month'}, 'InputVariables', vars_suma);
        monthly_sum(:,1:3) = []; 
        monthly_sum.Properties.VariableNames = strrep(monthly_sum.Properties.VariableNames, 'sum_', '');

        monthly_mat = [monthly_mat; monthly_mean , monthly_sum];
    end
    
    output_dir =  [ruta,'monthly/'];
    outputFile = fullfile(output_dir,name);
    writetable(monthly_mat, outputFile);
end

restoredefaultpath; savepath;


