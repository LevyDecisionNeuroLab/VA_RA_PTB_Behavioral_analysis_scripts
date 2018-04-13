% exclude criteria

clearvars
close all

root='D:\Ruonan\Projects in the lab\VA_RA_PTB\Clinical and behavioral';
% filename = [root,'\Clinical_Behavioral_040518.txt'];
% % data=tdfread(filename);
% tb = readtable(filename);

load(fullfile(root,'all data.mat'));

% only males
tb = tb(tb.isMale == 1,:);

save 'all data_male.mat' 'tb'