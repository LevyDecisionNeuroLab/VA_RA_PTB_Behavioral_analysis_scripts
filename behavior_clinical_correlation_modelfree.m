clearvars
close all

root='D:\Ruonan\Projects in the lab\VA_RA_PTB\Clinical and behavioral';
% filename = [root,'\Clinical_Behavioral_040518.txt'];
% % data=tdfread(filename);
% tb = readtable(filename);

load(fullfile(root,'all data_male.mat'));


%% include subjects
cluster = {'Re-experiencing','Avoidance','Emotional Numbing','Dysphoric Arousal','Anxious Arousal','Total'};
param = {'risk','ambig'};
domain = {'gain','loss'};

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

%% Multilinear regression, model free
% risk gain
multiTb = tb(tb.isGain == 1 & include, :);
multiLm = fitlm(multiTb, 'r50~R_fi_pm+A_fi_pm+N_fi_pm+DA_fi_pm+AA_fi_pm')
multiLm = fitlm(multiTb, 'r~caps_total_pm')


% ambig gain
multiTb = tb(tb.isGain == 1 & include, :);
multiLm = fitlm(multiTb, 'a24_r50~R_fi_pm+A_fi_pm+N_fi_pm+DA_fi_pm+AA_fi_pm')
multiLm = fitlm(multiTb, 'a_r50~caps_total_pm')

% risk loss
multiTb = tb(tb.isGain == 0 & include, :);
multiLm = fitlm(multiTb, 'r25~R_fi_pm+A_fi_pm+N_fi_pm+DA_fi_pm+AA_fi_pm')
multiLm = fitlm(multiTb, 'r~caps_total_pm')

% ambig loss
multiTb = tb(tb.isGain == 0 & include, :);
multiLm = fitlm(multiTb, 'a24_r50~R_fi_pm+A_fi_pm+N_fi_pm+DA_fi_pm+AA_fi_pm')
multiLm = fitlm(multiTb, 'a_r50~caps_total_pm')

%% specify plot
cluster2plot = 'Total';
param2plot = 'risk'; % risk, ambig
domain2plot = 'gain'; % gain, loss

if strcmp(domain2plot,'gain')
    domainIdx = (tb.isGain == 1);
else
    domainIdx = (tb.isGain == 0);
end

if strcmp(cluster2plot,'Total')
    x = tb.caps_total_pm(tb.isExcluded_behavior == 0 & domainIdx);
elseif strcmp(cluster2plot,'Re-experiencing')
    x = tb.R_fi_pm(tb.isExcluded_behavior == 0 & domainIdx);
elseif strcmp(cluster2plot,'Avoidance')
    x = tb.A_fi_pm(tb.isExcluded_behavior == 0 & domainIdx);
elseif strcmp(cluster2plot,'Emotional Numbing')
    x = tb.N_fi_pm(tb.isExcluded_behavior == 0 & domainIdx);
elseif strcmp(cluster2plot,'Dysphoric Arousal')
    x = tb.DA_fi_pm(tb.isExcluded_behavior == 0 & domainIdx);
elseif strcmp(cluster2plot,'Anxious Arousal')
    x = tb.AA_fi_pm(tb.isExcluded_behavior == 0 & domainIdx);
end

if strcmp(param2plot, 'risk')
    y = tb.r(tb.isExcluded_behavior == 0 & domainIdx);
elseif strcmp(param2plot, 'ambig')
    y = tb.a_r50(tb.isExcluded_behavior == 0 & domainIdx);
end

plotcorr(x,y,cluster2plot,[param2plot '-' domain2plot]);

%% all plots
for i = 1:length(cluster)
    cluster2plot = cluster{i};
    for j = 1:length(param)
        param2plot = param{j};
        for k = 1:length(domain)
            domain2plot = domain(k);
            
            if strcmp(domain2plot,'gain')
                domainIdx = (tb.isGain == 1);
            else
                domainIdx = (tb.isGain == 0);
            end
            
            if strcmp(cluster2plot,'Total')
                x = tb.caps_total_pm(tb.isExcluded_behavior == 0 & domainIdx & include);
            elseif strcmp(cluster2plot,'Re-experiencing')
                x = tb.R_fi_pm(tb.isExcluded_behavior == 0 & domainIdx & include);
            elseif strcmp(cluster2plot,'Avoidance')
                x = tb.A_fi_pm(tb.isExcluded_behavior == 0 & domainIdx & include);
            elseif strcmp(cluster2plot,'Emotional Numbing')
                x = tb.N_fi_pm(tb.isExcluded_behavior == 0 & domainIdx & include);
            elseif strcmp(cluster2plot,'Dysphoric Arousal')
                x = tb.DA_fi_pm(tb.isExcluded_behavior == 0 & domainIdx & include);
            elseif strcmp(cluster2plot,'Anxious Arousal')
                x = tb.AA_fi_pm(tb.isExcluded_behavior == 0 & domainIdx & include);
            end

            if strcmp(param2plot, 'risk')
                y = tb.r(tb.isExcluded_behavior == 0 & domainIdx & include);
            elseif strcmp(param2plot, 'ambig')
                y = tb.a_r50(tb.isExcluded_behavior == 0 & domainIdx & include);
            end

            plotcorr(x,y,cluster2plot,[param2plot '-' domain2plot])
            
        end
    end
end