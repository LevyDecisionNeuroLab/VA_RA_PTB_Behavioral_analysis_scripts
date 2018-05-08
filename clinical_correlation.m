% symptoms
clearvars
close all

root='D:\Ruonan\Projects in the lab\VA_RA_PTB\Clinical and behavioral';
% filename = [root,'\Clinical_Behavioral_040518.txt'];
% % data=tdfread(filename);
% tb = readtable(filename);

load(fullfile(root,'all data_male.mat'));

% which group to look at
include = strcmp(tb.group, 'P') & tb.isExcluded_behavior == 0;
% Include all subjects
include = (strcmp(tb.group, 'C') | strcmp(tb.group, 'P') | strcmp(tb.group, 'R')) & tb.isExcluded_behavior == 0;
% Include all subjects
include = (strcmp(tb.group, 'C') | strcmp(tb.group, 'P')) & tb.isExcluded_behavior == 0;

include = tb.isExcluded_behavior == 0;

% number of controls
ncontrol = sum(tb.isGain == 1 & strcmp(tb.group, 'C') & tb.isExcluded_behavior == 0)
% number of PTSDs
nptsd = sum(tb.isGain == 1 & strcmp(tb.group, 'P') & tb.isExcluded_behavior == 0)
% number of remitted PTSDs
nrptsd = sum(tb.isGain == 1 & strcmp(tb.group, 'R') & tb.isExcluded_behavior == 0)

%% plot single correlation 

% find groups
include =tb.isExcluded_behavior == 0 & tb.isGain == 1;

include =(strcmp(tb.group, 'C') | strcmp(tb.group, 'P')) &...
    tb.isExcluded_behavior == 0 & tb.isGain == 1;


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
include =tb.isExcluded_behavior == 0 & tb.isGain == 1;

%% plot correlation matrix, PTSD
tb2plot = table(tb.caps_totalscorem(include),tb.pcl5_total(include),...
    tb.pclm_total(include), 'VariableNames',{'CAPStotal', 'PCL5','PCLM'});

plotcorrmat1(tb2plot)

%% plot correlation matrix, addiction severity index
tb2plot = table(tb.asi_alcohol(include),tb.asi_drug(include),...
    tb.caps_totalscorem(include),...
    'VariableNames',{'ASIalcohol','ASIdrug','CAPStotal'});

plotcorrmat1(tb2plot)
%% caps total and clusters
tb2plot = table(tb.caps_totalscorem(include),tb.R_F_I_PastMonth_(include),...
    tb.A_F_I_PastMonth_(include), tb.N_F_I_PastMonth_(include),...
    tb.DA_F_I_PastMonth_(include), tb.AA_F_I_PastMonth_(include),...
    'VariableNames',{'Total','Reex','Avoid','Numb','Dysph','Anx'});

plotcorrmat1(tb2plot)

%% caps, clusters and ASI
tb2plot = table(tb.caps_totalscorem(include),tb.R_F_I_PastMonth_(include),...
    tb.A_F_I_PastMonth_(include), tb.N_F_I_PastMonth_(include),...
    tb.DA_F_I_PastMonth_(include), tb.AA_F_I_PastMonth_(include),...
    tb.asi_alcohol(include), tb.asi_drug(include),...
    'VariableNames',{'Total','Reex','Avoid','Numb','Dysph','Anx','ASIalcohol','ASIdrug'});

plotcorrmat1(tb2plot)

%% CAPS and ASI, excluding one ASI alcohol score > 1
tb2plot = table(tb.caps_totalscorem(include & tb.asi_alcohol < 1),...
    tb.asi_alcohol(include & tb.asi_alcohol < 1), tb.asi_drug(include & tb.asi_alcohol < 1),...
    'VariableNames',{'Total','ASIalcohol','ASIdrug'});

plotcorrmat1(tb2plot)

%% plot correlation matrix, PTSD and other clinical measurement
tb2plot = table(tb.caps_totalscorem(include), tb.bdiii_total(include),tb.madrs_total(include),...
    tb.hama_total(include), tb.stai_x1_total(include), tb.stai_x2_total(include),...
    tb.bai_total(include), tb.rses_total(include),...
    tb.ftnd_total(include),tb.ders_total(include),...
    tb.des_total_mean(include),tb.fssii_total(include),tb.rlocscale_total(include),...
    tb.bis11_total(include),...
    'VariableNames',{'CAPStotal', 'BDI', 'MADRS','HARS','STAI1','STAI2',...
    'BAI','RSES','FTND','DERS','DES','FSS','RLOCS','BIS11'});

plotcorrmat1(tb2plot)

%% plot correlation matrix, PTSD and depression anxiety, stress,adiction, emotion regulation, dissociative, fear, impulsivity
tb2plot = table(tb.caps_totalscorem(include), tb.bdiii_total(include),...
    tb.stai_x1_total(include), tb.stai_x2_total(include),...
    tb.bai_total(include), tb.rses_total(include),...
    tb.ders_total(include),...
    tb.des_total_mean(include),tb.fssii_total(include),...
    tb.bis11_total(include),...
    'VariableNames',{'CAPStotal', 'BDI','STAI1','STAI2',...
    'BAI','RSES','DERS','DES','FSS','BIS11'});

plotcorrmat1(tb2plot)

%% plot correlation matrix, trauma, combat exposure
tb2plot = table(tb.ctq_total(include),tb.ces_total(include),...
    tb.drri2_combat_exposure_total(include),...
    'VariableNames',{'CTQ', 'CES','DRRI'});

plotcorrmat1(tb2plot)

%% plot correlation matrix, PTSD, combat, depresstion, anxiety
tb2plot = table(tb.caps_totalscorem(include), tb.ces_total(include),...
    tb.bdiii_total(include), tb.hama_total(include),...
    tb.stai_x1_total(include), tb.stai_x2_total(include),...
    tb.bai_total(include), tb.rses_total(include),...
    'VariableNames',{'CAPStotal','CES','BDI', 'HARS','STAI1','STAI2','BAI','RSES'});

plotcorrmat1(tb2plot)

%% PCA all clinical 
include =tb.isExcluded_behavior == 0 & tb.isGain == 1;

tb2pca = table(tb.caps_totalscorem(include),tb.caps_totalscorel(include),...
    tb.pcl5_total(include),tb.pclm_total(include),...
    tb.ctq_total(include),tb.ces_total(include),tb.drri2_combat_exposure_total(include),...
    tb.bdiii_total(include),tb.stai_x1_total(include), tb.stai_x2_total(include),...
    tb.bai_total(include),...
    tb.asi_alcohol(include),tb.asi_drug(include),...
    tb.ders_total(include),tb.des_total_mean(include),tb.fssii_total(include),...
    tb.rses_total(include),tb.rlocscale_total(include),...
    tb.BASDrive(include),tb.BASFunSeeking(include),tb.BASRewardResponsiveness(include),...
    tb.BIS(include),...
    tb.bis11_total(include),...
    'VariableNames',...
    {'CAPSpm','CAPSlt', 'PCL5','PCLM',...       % PTSD
    'CTQ','CES','DRRI',...                      % Trauma 
    'BDI','STAI1','STAI2','BAI',...             % Axiety and depression
    'ASIalcohol','ASIdrug',...             
    'DERS','DES','FSS','RSES','RLOCS',...     
    'BASd','BASfs','BASrr','BIS','BIS11'});    


tb2pca = table(tb.caps_totalscorem(include),tb.caps_totalscorel(include),...
    tb.pcl5_total(include),tb.pclm_total(include),...
    tb.ctq_total(include),tb.ces_total(include),tb.drri2_combat_exposure_total(include),...
    tb.bdiii_total(include),tb.stai_x1_total(include), tb.stai_x2_total(include),...
    tb.bai_total(include),...
    tb.asi_alcohol(include),tb.asi_drug(include),...
    tb.ders_total(include),tb.des_total_mean(include),tb.fssii_total(include),...
    tb.rses_total(include),tb.rlocscale_total(include),...
    'VariableNames',...
    {'CAPSpm','CAPSlt', 'PCL5','PCLM',...       % PTSD
    'CTQ','CES','DRRI',...                      % Trauma 
    'BDI','STAI1','STAI2','BAI',...             % Axiety and depression
    'ASIalcohol','ASIdrug',...             
    'DERS','DES','FSS','RSES','RLOCS'});   

% only use PCLM
tb2pca = table(tb.caps_totalscorem(include),tb.caps_totalscorel(include),...
    tb.pclm_total(include),...
    tb.ctq_total(include),tb.ces_total(include),tb.drri2_combat_exposure_total(include),...
    tb.bdiii_total(include),tb.stai_x1_total(include), tb.stai_x2_total(include),...
    tb.bai_total(include),...
    tb.asi_alcohol(include),tb.asi_drug(include),...
    tb.ders_total(include),tb.des_total_mean(include),tb.fssii_total(include),...
    tb.rses_total(include),tb.rlocscale_total(include),...
    'VariableNames',...
    {'CAPSpm','CAPSlt', 'PCLM',...       % PTSD
    'CTQ','CES','DRRI',...                      % Trauma 
    'BDI','STAI1','STAI2','BAI',...             % Axiety and depression
    'ASIalcohol','ASIdrug',...             
    'DERS','DES','FSS','RSES','RLOCS'});   

% exclude one ASI_alcohol score over 1
tb2pca = table(tb.caps_totalscorem(include),tb.caps_totalscorel(include),...
    tb.pclm_total(include),...
    tb.ctq_total(include),tb.ces_total(include),tb.drri2_combat_exposure_total(include),...
    tb.bdiii_total(include),tb.stai_x1_total(include), tb.stai_x2_total(include),...
    tb.bai_total(include),...
    tb.asi_alcohol(include),tb.asi_drug(include),...
    tb.ders_total(include),tb.des_total_mean(include),tb.fssii_total(include),...
    tb.rses_total(include),tb.rlocscale_total(include),...
    'VariableNames',...
    {'CAPSpm','CAPSlt', 'PCLM',...       % PTSD
    'CTQ','CES','DRRI',...                      % Trauma 
    'BDI','STAI1','STAI2','BAI',...             % Axiety and depression
    'ASIalcohol','ASIdrug',...             
    'DERS','DES','FSS','RSES','RLOCS'}); 

    tb2pca.ASIalcohol(tb2pca.ASIalcohol > 1) = NaN;
    figure
    hist(tb2pca.ASIalcohol,20)
    title('ASI alohol')

    figure
    hist(tb2pca.ASIdrug,20)
    title('ASI drug')

% use days in pm as alcohol and drug ASI score, do not include FSS, RSES, RLOCS
tb2pca = table(tb.caps_totalscorem(include),tb.caps_totalscorel(include),...
    tb.pclm_total(include),...
    tb.ctq_total(include),tb.ces_total(include),tb.drri2_combat_exposure_total(include),...
    tb.bdiii_total(include),tb.stai_x1_total(include), tb.stai_x2_total(include),...
    tb.bai_total(include),...
    tb.asi_alcohol_dayspm(include),tb.asi_drug_dayspm(include),...
    tb.ders_total(include),tb.des_total_mean(include),...
    'VariableNames',...
    {'CAPSpm','CAPSlt', 'PCLM',...       % PTSD
    'CTQ','CES','DRRI',...                      % Trauma 
    'BDI','STAI1','STAI2','BAI',...             % Axiety and depression
    'ASIalcoholPM','ASIdrugPM',...             
    'DERS','DES'}); 



% exclude rows with NaNs
% tb2pca = tb2pca(~any(ismissing(tb2pca),2),:);

% standardize each predictor
array2pca = nanzscore(table2array(tb2pca));

% PCA
[coeff1,score1,latent1,tsquared1,explained1,mu1] = pca(array2pca);
coeff = coeff1;
score = score1;
latent = latent1;
tsquared = tsquared1;
explained = explained1;
mu = mu1;

% PCA with algorithms dealing with missing value
[coeff2,score2,latent2,tsquared2,explained2,mu2] = pca(array2pca,...
    'algorithm','als'); % using alternatint least squares algorithm when there are missing values in the data
coeff = coeff2;
score = score2;
latent = latent2;
tsquared = tsquared2;
explained = explained2;
mu = mu2;

% probablistic PCA dealing with missing values
[coeff3,score3,pcvar3,mu3,v3,S3] = ppca(array2pca,size(array2pca,2));
coeff = coeff3;
score = score3;

% how many missigng data excluded
sum(isnan(score(:,1)))

% predictor names
names = tb2pca.Properties.VariableNames';

% plot variance explained
figure
plot(1:size(array2pca,2),cumsum(explained),'-bo');
xlabel('Number of PCA components');
ylabel('Percent Variance Explained');
ax = gca;
ax.XLim = [0,size(array2pca,2)];
ax.YLim = [0,105];

% plot loadings for components
ncomp = 5;
screensize = get( groot, 'Screensize' );
figure('Position', [0.1*screensize(3) 0.07*screensize(4) 0.8*screensize(3) 0.7*screensize(4)])
bar(1:size(coeff,1),coeff(:,1:ncomp))
ax = gca;
ax.XTick = [1:1:size(coeff,1)];
xlabel('Predictors');
ylabel('Loadings');
ax.XTickLabels = tb2pca.Properties.VariableNames;
legendtxt = {};
for i=1:ncomp    
    legendtxt = [legendtxt, ['Component ' num2str(i)]];
end
leg = legend(legendtxt);
leg.FontSize = 10;

% visualize components
figure
biplot(coeff(1:4,1:2),'scores',score(1:4,1:2),'varlabels',{'v_1','v_2','v_3','v_4'})





