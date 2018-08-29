clearvars

root_image= 'D:\Ruonan\Projects in the lab\VA_RA_PTB\Imaging analysis\Imaging_analysis_041218';
root_behav = 'D:\Ruonan\Projects in the lab\VA_RA_PTB\Clinical and behavioral';

file = 'BetaExtracts_noneGLM_ROInoneGLM_CovariateTotal-All4Conds_0.001Alphasim8mm-vmPFC92.xlsx';
file = 'BetaExtracts_noneGLM_ROInoneGLM_CovariateTotal-All4Conds_0.001Alphasim8mm-lMFG120.xlsx';
file = 'BetaExtracts_noneGLM_ROIBartra13_SV_ROI_peakcoord_5mm_fromZhihao_vmPFC.xlsx';
file = 'BetaExtracts_noneGLM_ROIBartra13_SV_ROI_peakcoord_5mm_fromZhihao_vStr.xlsx';
file = 'BetaExtracts_noneGLM_ROIGilaie-DotanEtAl_2014_rPPC_7mmSphere.xlsx';

filename = fullfile(root_image,file);
tbbeta = readtable(filename);
tbbeta.Properties.VariableNames{1} = 'id';


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% normalize four conditions betas within each subject
betas = table2array(tbbeta(:,2:5));
betas_z = nanzscore(betas');
betas = betas_z';
tbbeta(:,2:5) = array2table(betas);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% calculate betas for combined consitions
tbbeta.beta_all = (tbbeta.Amb_gains_Display + tbbeta.Risk_gains_Display + tbbeta.Amb_loss_Display + tbbeta.Risk_loss_Display) ./ 4;
tbbeta.beta_gain = (tbbeta.Amb_gains_Display + tbbeta.Risk_gains_Display) ./ 2;
tbbeta.beta_loss = (tbbeta.Amb_loss_Display + tbbeta.Risk_loss_Display) ./ 2;


%exclude subject 1300
% tb = tbold(tbold.Subject ~= 1300,:);

% behavioral and clinical data
tbfile =fullfile(root_behav, 'all data_male.mat');
load(tbfile);

tb = tb(tb.isExcluded_imaging == 0,:);

% combine 
tball = join(tb, tbbeta, 'Keys', {'id'});

%% correlation
% include_gain = tball.isExcluded_imaging == 0 & tball.isGain == 1;
% include_loss = tball.isExcluded_imaging == 0 & tball.isGain == 0;
% 
% include = include_gain;
% 
% x = tball.caps_totalscorem(include & ~isnan(tball.caps_totalscorem));
% y = tball.beta_all(include & ~isnan(tball.caps_totalscorem)); 
% 
% plotcorr(x,y,'CAPS total', 'beta all conds')
% 
% % imainga nd behaivor
% include = include_loss;
% 
% x = tball.beta_t(include);
% y = tball.beta_all(include); 
% 
% plotcorr(x,y,'model based ambig loss attitude', 'beta all conds')

%% Calculate components PCA 17 measures
include_gain = tball.isExcluded_imaging == 0 & tball.isGain == 1;
include_loss = tball.isExcluded_imaging == 0 & tball.isGain == 0;

include = include_gain;

% load PCA loadings
loadingsfilename = fullfile(root_behav,'pca loadings 17.xlsx');
loadings = readtable(loadingsfilename);


% exclude the one with an extreme ASI alcohol score
tball.asi_alcohol(tball.asi_alcohol>1) = NaN;

% calculate PCA component
tball.cp1 = nanzscore([tball.caps_totalscorem,tball.caps_totalscorel,...
    tball.pclm_total,...
    tball.ctq_total,tball.ces_total,tball.drri2_combat_exposure_total,...
    tball.bdiii_total,tball.stai_x1_total, tball.stai_x2_total,...
    tball.bai_total,...
    tball.asi_alcohol,tball.asi_drug,...
    tball.ders_total,tball.des_total_mean,tball.fssii_total,...
    tball.rses_total,tball.rlocscale_total])...
    * table2array(loadings(:,2));

tball.cp2 = nanzscore([tball.caps_totalscorem,tball.caps_totalscorel,...
    tball.pclm_total,...
    tball.ctq_total,tball.ces_total,tball.drri2_combat_exposure_total,...
    tball.bdiii_total,tball.stai_x1_total, tball.stai_x2_total,...
    tball.bai_total,...
    tball.asi_alcohol,tball.asi_drug,...
    tball.ders_total,tball.des_total_mean,tball.fssii_total,...
    tball.rses_total,tball.rlocscale_total])...
    * table2array(loadings(:,3));

tball.cp3 = nanzscore([tball.caps_totalscorem,tball.caps_totalscorel,...
    tball.pclm_total,...
    tball.ctq_total,tball.ces_total,tball.drri2_combat_exposure_total,...
    tball.bdiii_total,tball.stai_x1_total, tball.stai_x2_total,...
    tball.bai_total,...
    tball.asi_alcohol,tball.asi_drug,...
    tball.ders_total,tball.des_total_mean,tball.fssii_total,...
    tball.rses_total,tball.rlocscale_total])...
    * table2array(loadings(:,4));

%% Calculate components PCA 13 measures
include_gain = tball.isExcluded_imaging == 0 & tball.isGain == 1;
include_loss = tball.isExcluded_imaging == 0 & tball.isGain == 0;

include = include_gain;

% load PCA loadings
loadingsfilename = fullfile(root_behav,'pca loadings 13.xlsx');
loadings = readtable(loadingsfilename);


% exclude the one with an extreme ASI alcohol score
tball.asi_alcohol(tball.asi_alcohol>1) = NaN;

% calculate PCA component
tball.cp1 = nanzscore([tball.caps_totalscorem,...
    tball.pclm_total,...
    tball.ctq_total,tball.ces_total,tball.drri2_combat_exposure_total,...
    tball.bdiii_total,tball.stai_x1_total, tball.stai_x2_total,...
    tball.bai_total,...
    tball.asi_alcohol_dayspm,tball.asi_drug_dayspm,...
    tball.ders_total,tball.des_total_mean])...
    * table2array(loadings(:,2));

tball.cp2 = nanzscore([tball.caps_totalscorem,...
    tball.pclm_total,...
    tball.ctq_total,tball.ces_total,tball.drri2_combat_exposure_total,...
    tball.bdiii_total,tball.stai_x1_total, tball.stai_x2_total,...
    tball.bai_total,...
    tball.asi_alcohol_dayspm,tball.asi_drug_dayspm,...
    tball.ders_total,tball.des_total_mean])...
    * table2array(loadings(:,3));

tball.cp3 = nanzscore([tball.caps_totalscorem,...
    tball.pclm_total,...
    tball.ctq_total,tball.ces_total,tball.drri2_combat_exposure_total,...
    tball.bdiii_total,tball.stai_x1_total, tball.stai_x2_total,...
    tball.bai_total,...
    tball.asi_alcohol_dayspm,tball.asi_drug_dayspm,...
    tball.ders_total,tball.des_total_mean])...
    * table2array(loadings(:,4));

tball.cp4 = nanzscore([tball.caps_totalscorem,...
    tball.pclm_total,...
    tball.ctq_total,tball.ces_total,tball.drri2_combat_exposure_total,...
    tball.bdiii_total,tball.stai_x1_total, tball.stai_x2_total,...
    tball.bai_total,...
    tball.asi_alcohol_dayspm,tball.asi_drug_dayspm,...
    tball.ders_total,tball.des_total_mean])...
    * table2array(loadings(:,5));

tball.cp5 = nanzscore([tball.caps_totalscorem,...
    tball.pclm_total,...
    tball.ctq_total,tball.ces_total,tball.drri2_combat_exposure_total,...
    tball.bdiii_total,tball.stai_x1_total, tball.stai_x2_total,...
    tball.bai_total,...
    tball.asi_alcohol_dayspm,tball.asi_drug_dayspm,...
    tball.ders_total,tball.des_total_mean])...
    * table2array(loadings(:,6));

%% Calculate components PCA 11 measures
include_gain = tball.isExcluded_imaging == 0 & tball.isGain == 1;
include_loss = tball.isExcluded_imaging == 0 & tball.isGain == 0;

include = include_gain;

% load PCA loadings
loadingsfilename = fullfile(root_behav,'pca loadings 11.xlsx');
loadings = readtable(loadingsfilename);


% calculate PCA component
tball.cp1 = nanzscore([tball.caps_totalscorem,...
    tball.pclm_total,...
    tball.ctq_total,tball.ces_total,tball.drri2_combat_exposure_total,...
    tball.bdiii_total,tball.stai_x1_total, tball.stai_x2_total,...
    tball.bai_total,...
    tball.ders_total,tball.des_total_mean])...
    * table2array(loadings(:,2));

tball.cp2 = nanzscore([tball.caps_totalscorem,...
    tball.pclm_total,...
    tball.ctq_total,tball.ces_total,tball.drri2_combat_exposure_total,...
    tball.bdiii_total,tball.stai_x1_total, tball.stai_x2_total,...
    tball.bai_total,...
    tball.ders_total,tball.des_total_mean])...
    * table2array(loadings(:,3));

tball.cp3 = nanzscore([tball.caps_totalscorem,...
    tball.pclm_total,...
    tball.ctq_total,tball.ces_total,tball.drri2_combat_exposure_total,...
    tball.bdiii_total,tball.stai_x1_total, tball.stai_x2_total,...
    tball.bai_total,...
    tball.ders_total,tball.des_total_mean])...
    * table2array(loadings(:,4));

tball.cp4 = nanzscore([tball.caps_totalscorem,...
    tball.pclm_total,...
    tball.ctq_total,tball.ces_total,tball.drri2_combat_exposure_total,...
    tball.bdiii_total,tball.stai_x1_total, tball.stai_x2_total,...
    tball.bai_total,...
    tball.ders_total,tball.des_total_mean])...
    * table2array(loadings(:,5));

tball.cp5 = nanzscore([tball.caps_totalscorem,...
    tball.pclm_total,...
    tball.ctq_total,tball.ces_total,tball.drri2_combat_exposure_total,...
    tball.bdiii_total,tball.stai_x1_total, tball.stai_x2_total,...
    tball.bai_total,...
    tball.ders_total,tball.des_total_mean])...
    * table2array(loadings(:,6));

%% create table for multilinear model PCA 17
tb2ml = table(tball.id(include), tball.age(include),...
    tball.caps_totalscorem(include),tball.caps_totalscorel(include),...
    tball.pclm_total(include),...
    tball.ctq_total(include),tball.ces_total(include),tball.drri2_combat_exposure_total(include),...
    tball.bdiii_total(include),tball.stai_x1_total(include), tball.stai_x2_total(include),...
    tball.bai_total(include),...
    tball.asi_alcohol(include),tball.asi_drug(include),...
    tball.ders_total(include),tball.des_total_mean(include),tball.fssii_total(include),...
    tball.rses_total(include),tball.rlocscale_total(include),...
    ...
    tb.R_F_I_PastMonth_(include),...
    tb.A_F_I_PastMonth_(include), tb.N_F_I_PastMonth_(include),...
    tb.DA_F_I_PastMonth_(include), tb.AA_F_I_PastMonth_(include),...
    ...
    tball.cp1(include),tball.cp2(include),tball.cp3(include),...
    ...
    tball.alpha_t(include_gain), tball.beta_t(include_gain),...
    tball.alpha_t(include_loss), tball.beta_t(include_loss),...
    tball.r(include_gain), tball.a_r50(include_gain),...
    tball.r(include_loss), tball.a_r50(include_loss),...
    ...
    tball.Risk_gains_Display(include), tball.Amb_gains_Display(include),...
    tball.Risk_loss_Display(include), tball.Amb_loss_Display(include),...
    tball.beta_gain(include), tball.beta_loss(include),...
    tball.beta_all(include),...
    ...
    'VariableNames',...
    {'id', 'age',...
    'CAPSpm','CAPSlt', 'PCLM',...       % PTSD
    'CTQ','CES','DRRI',...                      % Trauma 
    'BDI','STAI1','STAI2','BAI',...             % Axiety and depression
    'ASIalcohol','ASIdrug',...             
    'DERS','DES','FSS','RSES','RLOCS',...
    ...
    'Reex','Avoid','Numb','Dysph','Anx',...
    ...
    'CP1', 'CP2', 'CP3',...
    ...
    'RGmb','AGmb',...
    'RLmb', 'ALmb',...
    'RGmf','AGmf',...
    'RLmf', 'ALmf',...
    ...
    'betaRG','betaAG',...
    'betaRL', 'betaAL',...
    'betaG', 'betaL',...
    'betaAll'});

%% create table for multilinear model PCA 13
tb2ml = table(tball.id(include), tball.age(include),...
    tball.caps_totalscorem(include),tball.caps_totalscorel(include),...
    tball.pclm_total(include),...
    tball.ctq_total(include),tball.ces_total(include),tball.drri2_combat_exposure_total(include),...
    tball.bdiii_total(include),tball.stai_x1_total(include), tball.stai_x2_total(include),...
    tball.bai_total(include),...
    tball.asi_alcohol(include),tball.asi_drug(include),...
    tball.ders_total(include),tball.des_total_mean(include),tball.fssii_total(include),...
    tball.rses_total(include),tball.rlocscale_total(include),...
    ...
    tb.R_F_I_PastMonth_(include),...
    tb.A_F_I_PastMonth_(include), tb.N_F_I_PastMonth_(include),...
    tb.DA_F_I_PastMonth_(include), tb.AA_F_I_PastMonth_(include),...
    ...
    tball.cp1(include),tball.cp2(include),tball.cp3(include),...
    tball.cp4(include),tball.cp5(include),...
    ...
    tball.alpha_t(include_gain), tball.beta_t(include_gain),...
    tball.alpha_t(include_loss), tball.beta_t(include_loss),...
    tball.r(include_gain), tball.a_r50(include_gain),...
    tball.r(include_loss), tball.a_r50(include_loss),...
    ...
    tball.Risk_gains_Display(include), tball.Amb_gains_Display(include),...
    tball.Risk_loss_Display(include), tball.Amb_loss_Display(include),...
    tball.beta_gain(include), tball.beta_loss(include),...
    tball.beta_all(include),...
    ...
    'VariableNames',...
    {'id', 'age',...
    'CAPSpm','CAPSlt', 'PCLM',...       % PTSD
    'CTQ','CES','DRRI',...                      % Trauma 
    'BDI','STAI1','STAI2','BAI',...             % Axiety and depression
    'ASIalcohol','ASIdrug',...             
    'DERS','DES','FSS','RSES','RLOCS',...
    ...
    'Reex','Avoid','Numb','Dysph','Anx',...
    ...
    'CP1', 'CP2', 'CP3', 'CP4', 'CP5',...
    ...
    'RGmb','AGmb',...
    'RLmb', 'ALmb',...
    'RGmf','AGmf',...
    'RLmf', 'ALmf',...
    ...
    'betaRG','betaAG',...
    'betaRL', 'betaAL',...
    'betaG', 'betaL',...
    'betaAll'});

%% create table for multilinear model PCA 11
tb2ml = table(tball.id(include), tball.age(include),...
    tball.caps_totalscorem(include),tball.caps_totalscorel(include),...
    tball.pclm_total(include),...
    tball.ctq_total(include),tball.ces_total(include),tball.drri2_combat_exposure_total(include),...
    tball.bdiii_total(include),tball.stai_x1_total(include), tball.stai_x2_total(include),...
    tball.bai_total(include),...
    tball.asi_alcohol(include),tball.asi_drug(include),...
    tball.ders_total(include),tball.des_total_mean(include),tball.fssii_total(include),...
    tball.rses_total(include),tball.rlocscale_total(include),...
    ...
    tb.R_F_I_PastMonth_(include),...
    tb.A_F_I_PastMonth_(include), tb.N_F_I_PastMonth_(include),...
    tb.DA_F_I_PastMonth_(include), tb.AA_F_I_PastMonth_(include),...
    ...
    tball.cp1(include),tball.cp2(include),tball.cp3(include),...
    tball.cp4(include),tball.cp5(include),...
    ...
    tball.alpha_t(include_gain), tball.beta_t(include_gain),...
    tball.alpha_t(include_loss), tball.beta_t(include_loss),...
    tball.r(include_gain), tball.a_r50(include_gain),...
    tball.r(include_loss), tball.a_r50(include_loss),...
    ...
    tball.Risk_gains_Display(include), tball.Amb_gains_Display(include),...
    tball.Risk_loss_Display(include), tball.Amb_loss_Display(include),...
    tball.beta_gain(include), tball.beta_loss(include),...
    tball.beta_all(include),...
    ...
    'VariableNames',...
    {'id', 'age',...
    'CAPSpm','CAPSlt', 'PCLM',...       % PTSD
    'CTQ','CES','DRRI',...                      % Trauma 
    'BDI','STAI1','STAI2','BAI',...             % Axiety and depression
    'ASIalcohol','ASIdrug',...             
    'DERS','DES','FSS','RSES','RLOCS',...
    ...
    'Reex','Avoid','Numb','Dysph','Anx',...
    ...
    'CP1', 'CP2', 'CP3', 'CP4', 'CP5',...
    ...
    'RGmb','AGmb',...
    'RLmb', 'ALmb',...
    'RGmf','AGmf',...
    'RLmf', 'ALmf',...
    ...
    'betaRG','betaAG',...
    'betaRL', 'betaAL',...
    'betaG', 'betaL',...
    'betaAll'});


%% plot activation correlation matrix
tb2plot1 = table(tb2ml.betaRG, tb2ml.betaAG, tb2ml.betaRL, tb2ml.betaAL,...
    'VariableNames',{'betaRG','betaAG','betaRL','betaAL'});

plotcorrmat1(tb2plot1,1)

% correlation matrix between activation and model based attitudes
tb2plot2 = table(tb2ml.RGmb, tb2ml.AGmb, tb2ml.RLmb, tb2ml.ALmb,...
    'VariableNames',{'RGmb','AGmb','RLmb', 'ALmb'});
plotcorrmat1(tb2plot2,1)

plotcorrmat2(tb2plot1,tb2plot2,1)


%% correlation bwtween components and behavior
tb2plot2 = table(tb2ml.RGmb, tb2ml.AGmb, tb2ml.RLmb, tb2ml.ALmb,...
    'VariableNames',{'RGmb','AGmb','RLmb', 'ALmb'});
 
tb2plot1 = table(tb2ml.CP1, tb2ml.CP2, tb2ml.CP3,tb2ml.CP4,tb2ml.CP5,...
    'VariableNames',{'CP1','CP2','CP3','CP4','CP5'});

plotcorrmat2(tb2plot1, tb2plot2,1)

%% correlation bwtween components and activation
tb2plot2 = table(tb2ml.betaRG, tb2ml.betaAG, tb2ml.betaRL, tb2ml.betaAL,...
    'VariableNames',{'betaRG','betaAG','betaRL','betaAL'});
 
tb2plot1 = table(tb2ml.CP1, tb2ml.CP2, tb2ml.CP3,tb2ml.CP4,tb2ml.CP5,...
    'VariableNames',{'CP1','CP2','CP3','CP4','CP5'});

plotcorrmat2(tb2plot1, tb2plot2, 1)

%% correlation between caps and activation
tb2plot2 = table(tb2ml.betaRG, tb2ml.betaAG, tb2ml.betaRL, tb2ml.betaAL,...
    'VariableNames',{'betaRG','betaAG','betaRL','betaAL'});
 
tb2plot1 = table(tb2ml.CAPSpm, tb2ml.CAPSlt,...
    'VariableNames',{'CAPSpm','CAPSlt'});

plotcorrmat2(tb2plot1, tb2plot2)

%% correlation between activation and symptom clusters
tb2plot2 = table(tb2ml.betaRG, tb2ml.betaAG, tb2ml.betaRL, tb2ml.betaAL,...
    'VariableNames',{'betaRG','betaAG','betaRL','betaAL'});

tb2plot1 = table(tb2ml.Reex,...
    tb2ml.Avoid, tb2ml.Numb,...
    tb2ml.Dysph, tb2ml.Anx,...
    'VariableNames',{'Reex','Avoid','Numb','Dysph','Anx'});

plotcorrmat2(tb2plot1, tb2plot2)

%% symptoms (PCA component) ~ imaging risk gain + behavioral risk gain + ...
definem = 'CP1 ~ age+ betaRG + RGmb + betaAG + AGmb + betaRL + RLmb + betaAL + ALmb';
definem = 'CP2 ~ age+ betaRG + RGmb + betaAG + AGmb + betaRL + RLmb + betaAL + ALmb';
definem = 'CP3 ~ age+ betaRG + RGmb + betaAG + AGmb + betaRL + RLmb + betaAL + ALmb';
definem = 'CP4 ~ age+ betaRG + RGmb + betaAG + AGmb + betaRL + RLmb + betaAL + ALmb';
definem = 'CP5 ~ age+ betaRG + RGmb + betaAG + AGmb + betaRL + RLmb + betaAL + ALmb';
definem = 'CAPSpm ~ age+ betaRG + RGmb + betaAG + AGmb + betaRL + RLmb + betaAL + ALmb';
definem = 'CAPSlt ~ age+ betaRG + RGmb + betaAG + AGmb + betaRL + RLmb + betaAL + ALmb';

definem = 'betaRG ~ age + CP1 + RGmb';
definem = 'betaAG ~ age + CP1 + AGmb';
definem = 'betaRL ~ age + CP1 + RLmb';
definem = 'betaAL ~ age + CP1 + ALmb';

definem = 'betaRG ~ age + CP1';
definem = 'betaAG ~ age + CP1';
definem = 'betaRL ~ age + CP1';
definem = 'betaAL ~ age + CP1';
definem = 'betaG ~ age + CP1';
definem = 'betaL ~ age + CP1';
definem = 'betaAll ~ age + CP1';

definem = 'betaRG ~ age + CP2';
definem = 'betaAG ~ age + CP2';
definem = 'betaRL ~ age + CP2';
definem = 'betaAL ~ age + CP2';
definem = 'betaG ~ age + CP2';
definem = 'betaL ~ age + CP2';
definem = 'betaAll ~ age + CP2';

definem = 'betaG ~ age + Reex + Avoid + Numb + Dysph + Anx';
definem = 'betaL ~ age + Reex + Avoid + Numb + Dysph + Anx';
definem = 'betaRG ~ age + Reex + Avoid + Numb + Dysph + Anx';
definem = 'betaRL ~ age + Reex + Avoid + Numb + Dysph + Anx';
definem = 'betaAG ~ age + Reex + Avoid + Numb + Dysph + Anx';
definem = 'betaAL ~ age + Reex + Avoid + Numb + Dysph + Anx';
definem = 'betaAll ~ age + Reex + Avoid + Numb + Dysph + Anx';

multiLm = fitlm(tb2ml, definem)

