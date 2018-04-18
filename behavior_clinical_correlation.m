clearvars
close all

root='D:\Ruonan\Projects in the lab\VA_RA_PTB\Clinical and behavioral';
% filename = [root,'\Clinical_Behavioral_040518.txt'];
% % data=tdfread(filename);
% tb = readtable(filename);

load(fullfile(root,'all data_male.mat'));

%% including
cluster = {'Re-experiencing','Avoidance','Emotional Numbing','Dysphoric Arousal','Anxious Arousal','Total'};
param = {'alpha','beta'};
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


% caculate model free scores
% tb.r = nanmean([tb.r25, tb.r50, tb.r75],2);
% tb.a_r50 = nanmean([tb.a24_r50, tb.a50_r50, tb.a74_r50],2);
% save 'all data.mat' 'tb'
%% Multilinear regression, model based
% risk gain
multiTb = tb(tb.isGain == 1 & include, :);
multiLm = fitlm(multiTb, 'alpha_t~R_fi_pm+A_fi_pm+N_fi_pm+DA_fi_pm+AA_fi_pm')
multiLm = fitlm(multiTb, 'alpha_t~caps_total_lt')


% ambig gain
multiTb = tb(tb.isGain == 1 & include, :);
multiLm = fitlm(multiTb, 'beta_t~R_fi_pm+A_fi_pm+N_fi_pm+DA_fi_pm+AA_fi_pm')
multiLm = fitlm(multiTb, 'beta_t~caps_total_pm')

% risk loss
multiTb = tb(tb.isGain == 0 & include, :);
multiLm = fitlm(multiTb, 'alpha_t~R_fi_pm+A_fi_pm+N_fi_pm+DA_fi_pm+AA_fi_pm')
multiLm = fitlm(multiTb, 'alpha_t~caps_total_pm')

% ambig loss
multiTb = tb(tb.isGain == 0 & include, :);
multiLm = fitlm(multiTb, 'beta_t~R_fi_pm+A_fi_pm+N_fi_pm+DA_fi_pm+AA_fi_pm')
multiLm = fitlm(multiTb, 'beta_t~caps_total_lt')


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

            if strcmp(param2plot, 'alpha')
                y = tb.alpha_t(tb.isExcluded_behavior == 0 & domainIdx & include);
            elseif strcmp(param2plot, 'beta')
                y = tb.beta_t(tb.isExcluded_behavior == 0 & domainIdx & include);
            end
            
            plotcorr(x,y,cluster2plot,[param2plot '-' domain2plot]);


            %     x = tb.N_fi_pm(tb.isExcluded_behavior == 0 & tb.isGain == 0);
            %     x = tb.caps_total_pm(tb.isExcluded_behavior == 0 & tb.isGain == 0);
            %     y = tb.beta_t(tb.isExcluded_behavior == 0 & tb.isGain == 0);

                % save the plot
        end
    end
end

%% specify plot
cluster2plot = 'Total';
param2plot = 'beta';
domain2plot = 'loss';

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

if strcmp(param2plot, 'alpha')
    y = tb.alpha_t(tb.isExcluded_behavior == 0 & domainIdx);
elseif strcmp(param2plot, 'beta')
    y = tb.beta_t(tb.isExcluded_behavior == 0 & domainIdx);
end


plotcorr(x,y,cluster2plot,[param2plot '-' domain2plot]);

%% correlation matrix

include_gain = (strcmp(tb.group, 'C') | strcmp(tb.group, 'P')) &...
    tb.isExcluded_behavior == 0 & tb.isGain == 1;

include_loss = (strcmp(tb.group, 'C') | strcmp(tb.group, 'P')) &...
    tb.isExcluded_behavior == 0 & tb.isGain == 0;


tb2plot_clinical = table(tb.caps_total_pm(include_gain),tb.pcl5_total(include_gain),...
    tb.pclm_total(include_gain), 'VariableNames',{'CAPStotal', 'PCL5','PCLM'});

tb2plot_clinical = table(tb.caps_total_pm(include_gain),tb.R_fi_pm(include_gain),...
    tb.A_fi_pm(include_gain), tb.N_fi_pm(include_gain),...
    tb.DA_fi_pm(include_gain), tb.AA_fi_pm(include_gain),...
    'VariableNames',{'Total','Reex','Avoid','Numb','Dysph','Anx'});

tb2plot_clinical = table(tb.bdiii_total(include_gain),tb.madrs_total(include_gain),...
    tb.hama_total(include_gain), tb.stai_x1_total(include_gain), tb.stai_x2_total(include_gain),...
    tb.bai_total(include_gain), tb.rses_total(include_gain),...
    'VariableNames',{'BDI', 'MADRS','HARS','STAI1','STAI2','BAI','RSES'});

tb2plot_clinical = table(tb.ctq_total(include_gain),tb.ces_total(include_gain),...
    tb.drri2_combat_exposure_total(include_gain),...
    'VariableNames',{'CTQ', 'CES','DRRI'});

tb2plot_behav = table(tb.alpha_t(include_gain), tb.beta_t(include_gain),...
    tb.alpha_t(include_loss), tb.beta_t(include_loss),...
    'VariableNames',{'alphaGain', 'betaGain','alphaLoss','betaLoss'});

tb2plot_behav = table(tb.r(include_gain), tb.a_r50(include_gain),...
    tb.r(include_loss), tb.a_r50(include_loss),...
    'VariableNames',{'riskGain', 'ambigGain','riskLoss','ambigLoss'});

plotcorrmat2(tb2plot_clinical, tb2plot_behav)

