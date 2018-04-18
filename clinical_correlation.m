% symptoms
clearvars
close all

root='D:\Ruonan\Projects in the lab\VA_RA_PTB\Clinical and behavioral';
% filename = [root,'\Clinical_Behavioral_040518.txt'];
% % data=tdfread(filename);
% tb = readtable(filename);

load(fullfile(root,'all data_male.mat'));
screensize = get( groot, 'Screensize' );


% which group to look at
include = strcmp(tb.group, 'P') & tb.isExcluded_behavior == 0;
% Include all subjects
include = (strcmp(tb.group, 'C') | strcmp(tb.group, 'P') | strcmp(tb.group, 'R')) & tb.isExcluded_behavior == 0;
% Include all subjects
include = (strcmp(tb.group, 'C') | strcmp(tb.group, 'P')) & tb.isExcluded_behavior == 0;

% number of controls
ncontrol = sum(tb.isGain == 1 & strcmp(tb.group, 'C') & tb.isExcluded_behavior == 0)
% number of PTSDs
nptsd = sum(tb.isGain == 1 & strcmp(tb.group, 'P') & tb.isExcluded_behavior == 0)
% number of remitted PTSDs
nrptsd = sum(tb.isGain == 1 & strcmp(tb.group, 'R') & tb.isExcluded_behavior == 0)

%% plot single correlation 

% find groups
include =find(tb.isExcluded_behavior == 0 & tb.isGain == 1);

include =find((strcmp(tb.group, 'C') | strcmp(tb.group, 'P')) &...
    tb.isExcluded_behavior == 0 & tb.isGain == 1);


xname = 'CAPS total';
x = tb.caps_total_pm(include);

xname = 'PCL5 total';
x = tb.pcl5_total(include);

yname = 'PCLM total';
y = tb.pclm_total(include);

yname = 'BDI total';
y = tb.bdiii_total(include);

xname = 'STAI 1';
x = tb.stai_x1_total(include);

yname = 'STAI 2';
y = tb.stai_x2_total(include);

yname = 'CES';
y = tb.ces_total(include);

plotcorr(x,y,xname,yname)

%% correlation matrix 
%% plot correlation matrix, PTSD
tb2plot = table(tb.caps_total_pm(include),tb.pcl5_total(include),...
    tb.pclm_total(include), 'VariableNames',{'CAPStotal', 'PCL5','PCLM'});

plotcorrmat1(tb2plot)

%% caps total and clusters
tb2plot = table(tb.caps_total_pm(include),tb.R_fi_pm(include),...
    tb.A_fi_pm(include), tb.N_fi_pm(include),...
    tb.DA_fi_pm(include), tb.AA_fi_pm(include),...
    'VariableNames',{'Total','Reex','Avoid','Numb','Dysph','Anx'});

plotcorrmat1(tb2plot)

%% plot correlation matrix, depression anxiety stress
tb2plot = table(tb.bdiii_total(include),tb.madrs_total(include),...
    tb.hama_total(include), tb.stai_x1_total(include), tb.stai_x2_total(include),...
    tb.bai_total(include), tb.rses_total(include),...
    'VariableNames',{'BDI', 'MADRS','HARS','STAI1','STAI2','BAI','RSES'});

plotcorrmat1(tb2plot)

%% plot correlation matrix, trauma, combat exposure
tb2plot = table(tb.ctq_total(include),tb.ces_total(include),...
    tb.drri2_combat_exposure_total(include),...
    'VariableNames',{'CTQ', 'CES','DRRI'});

plotcorrmat1(tb2plot)

%% plot correlation matrix, trauma, combat exposure

tb2plot = table(tb.caps_total_pm(include), tb.ces_total(include),...
    tb.bdiii_total(include), tb.hama_total(include),...
    tb.stai_x1_total(include), tb.stai_x2_total(include),...
    tb.bai_total(include), tb.rses_total(include),...
    'VariableNames',{'CAPStotal','CES','BDI', 'HARS','STAI1','STAI2','BAI','RSES'});

plotcorrmat1(tb2plot)





