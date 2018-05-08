clearvars

root_image= 'D:\Ruonan\Projects in the lab\VA_RA_PTB\Imaging analysis\Imaging_analysis_041218';
root_behav = 'D:\Ruonan\Projects in the lab\VA_RA_PTB\Clinical and behavioral';

file = 'BetaExtracts_noneGLM_ROInoneGLM_CovariateTotal-All4Conds_0.001Alphasim8mm-vmPFC92.xlsx';
filename = fullfile(root_image,file);
tbbeta = readtable(filename);
tbbeta.Properties.VariableNames{1} = 'id';

tbbeta.beta_all = (tbbeta.Amb_gains_Display + tbbeta.Risk_gains_Display + tbbeta.Amb_loss_Display + tbbeta.Risk_loss_Display) ./ 4;
tbbeta.beta_gain = (tbbeta.Amb_gains_Display + tbbeta.Risk_gains_Display) ./ 2;
tbbeta.beta_loss = (tbbeta.Amb_loss_Display + tbbeta.Risk_loss_Display) ./ 2;

%exclude subject 1300
% tb = tbold(tbold.Subject ~= 1300,:);

% behavioral and clinical data
tbfile =fullfile(root_behav, 'all data.mat');
load(tbfile);

tb = tb(tb.isExcluded_imaging == 0,:);
%exclude subject 1300
% cov = covold(~strcmp(covold.id, 'subj1300'), :);

% tb and cov should have a same subject list in the same order

% combine 
tball = join(tb, tbbeta, 'Keys', {'id'});

%% correlation 
betaNames = {'Amb_gains_Display', 'Risk_gains_Display', 'Amb_loss_Display', 'Risk_loss_Display','All', 'Gain', 'Loss'};
clusterNames = {'R_F_I_PastMonth_', 'A_F_I_PastMonth_', 'N_F_I_PastMonth_', 'DA_F_I_PastMonth_', 'AA_F_I_PastMonth_', 'caps_totalscorem'};
attNames = {'alpha_gain_cstr_trf', 'beta_gain_cstr_trf', 'alpha_loss_cstr_trf', 'beta_loss_cstr_trf'};
fitTypes = {'ordinary', 'robust'};

% individual plot and fitting
betaName = 'Amb_gains_Display';
clusterName = 'caps_total_pm';
attName = 'beta_loss_cstr_trf';
fitType = 'robust';

%% Scatter plot and regression fitting
% plot regression with symptom or behavioral attitude
regrName = clusterName;

y = tbbeta.(betaName);
x = cov.(regrName);
% x = log(x);

figure    
scatter(x,y, 'filled', 'LineWidth',5);
hold on

ax=gca;
ax.FontSize = 20;
ax.LineWidth = 3;
% ax.YLim = [-0.3,0.3];
% ax.YTick = [ -0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3];
 
                
if strcmp(fitType,'robust')
    % robust regression
    [b,robustmdl1]= robustfit(x,y); 
    linex = linspace(min(x),max(x));
    liney = b(2)*linex+b(1);
    plot(linex, liney, 'color','k', 'LineWidth', 2);
    [corrmat, pmat] = corrcoef([x,y],'rows','complete');
    
    
 
    % print text of coeff and p value
    txt1 = ['regression coeff = ', num2str(b(2))];
    txt2 = ['p = ', num2str(round(robustmdl1.p(2),4,'significant'))];

    txt3 = ['correlation coeff =',num2str(corrmat(1,2))];
    txt4 = ['p = ', num2str(round(pmat(1,2),4,'significant'))];
    xlab = xlim;
    ylab = ylim;
    txt = {txt1;txt2;[];txt3;txt4};
    text(xlab(2)-(xlab(2)-xlab(1))/4, ylab(2)-(ylab(2)-ylab(1))/8, txt, 'FontSize',8)

    title([regrName ' with ' betaName ' Robust'])

% elseif strcmp(fitType, 'pearson')
%     [corrmat, pmat] = corrcoef([x,y],'rows','complete')
    
elseif strcmp(fitType, 'ordinary')
    % ordinary linear regression
    mdl1 = LinearModel.fit(x,y); % creates a linear model of the responses y to a tb matrix x
    coeff = table2array(mdl1.Coefficients);
    linex = linspace(min(x)-(max(x)-min(x))/15,max(x)+(max(x)-min(x))/15);
    liney = coeff(2,1)*linex+coeff(1,1);
    plot(linex, liney, 'color','k', 'LineWidth', 2);

    % print text of coeff, r2 and p value
    txt1 = ['R^{2} = ',num2str(mdl1.Rsquared.Ordinary)];
    txt2 = ['p = ', num2str(round(coeff(2,4),4,'significant'))];
    txt3 = ['coeff = ', num2str(coeff(2,1))];
    xlab = xlim;
    ylab = ylim;
    txt = {txt3;txt1;txt2};
    text(xlab(2)-(xlab(2)-xlab(1))/4, ylab(2), txt, 'FontSize',8)

    title([regrName ' with ' betaName ' OLS'])
    
    % residual plot
    figure
    scatter(x,mdl1.Residuals.Raw)
    hold on
    title([regrName ' with ' betaName ' OLS ' 'Residuals'])

end

%% Multilinear model
% cluster2fit = {'R_fi_pm', 'A_fi_pm', 'N_fi_pm', 'DA_fi_pm', 'AA_fi_pm', 'caps_total_pm'};
% cluster2fit = {'caps_total_pm'};
% cluster2fit = {'N_fi_pm', 'caps_total_pm'};
% cluster2fit = {'N_fi_pm'};
% cluster2fit = {'DA_fi_pm', 'caps_total_pm'};
% 
% xMulti = zeros(length(cov.caps_total_pm), length(cluster2fit)+1);
% xMulti(:,1) = ones(length(cov.caps_total_pm),1);
% for i = 1:length(cluster2fit)
%     xMulti(:,i+1) = cov.(cluster2fit{i});    
% end
% 
% [b,bint,r,rint,stats] = regress(y,xMulti);

% combine covariates and beta tables
cov.Subject = tbbeta.Subject; % two tables must share a same column
multiTb = join(cov,tbbeta); % table for multi linear regression

lm = fitlm(multiTb, 'Loss~R_fi_pm+A_fi_pm+N_fi_pm+DA_fi_pm+AA_fi_pm')

lm = fitlm(multiTb, 'Amb_gains_Display~R_fi_pm+A_fi_pm+N_fi_pm+DA_fi_pm+AA_fi_pm')
lm = fitlm(multiTb, 'Risk_gains_Display~R_fi_pm+A_fi_pm+N_fi_pm+DA_fi_pm+AA_fi_pm')
lm = fitlm(multiTb, 'Amb_loss_Display~R_fi_pm+A_fi_pm+N_fi_pm+DA_fi_pm+AA_fi_pm')
lm = fitlm(multiTb, 'Risk_loss_Display~R_fi_pm+A_fi_pm+N_fi_pm+DA_fi_pm+AA_fi_pm')

%%
lm = fitlm(multiTb, 'Gain~R_fi_pm+A_fi_pm+N_fi_pm+DA_fi_pm+AA_fi_pm')
% plot mean and se for the predictors
plotmean = lm.Coefficients.Estimate(2:6);
plotsem = lm.Coefficients.SE(2:6);
fig = figure
bplot = bar(plotmean);
hold on
errorbar(plotmean, plotsem, '.','Color',[0,0,0],'LineWidth',2);

ax = gca;
ax.XTickLabel = '';
ax.Box = 'off';
ax.FontSize = 25;
ax.LineWidth =3;

%%
lm = fitlm(multiTb, 'Loss~R_fi_pm+A_fi_pm+N_fi_pm+DA_fi_pm+AA_fi_pm')
plotmean = lm.Coefficients.Estimate(2:6);
plotsem = lm.Coefficients.SE(2:6);
fig = figure
bplot = bar(plotmean);
hold on
errorbar(plotmean, plotsem, '.','Color',[0,0,0],'LineWidth',2);

ax = gca;
ax.XTickLabel = '';
ax.Box = 'off';
ax.FontSize = 25;
ax.LineWidth =3;

lm = fitlm(multiTb, 'All~caps_total_pm')
lm = fitlm(multiTb, 'All~N_fi_pm')
lm = fitlm(multiTb, 'All~caps_total_pm+N_fi_pm')

lm = fitlm(multiTb, 'Amb_gains_Display~caps_total_pm')
lm = fitlm(multiTb, 'Amb_gains_Display~N_fi_pm')
lm = fitlm(multiTb, 'Amb_gains_Display~caps_total_pm+N_fi_pm')

lm = fitlm(multiTb, 'Risk_gains_Display~caps_total_pm')
lm = fitlm(multiTb, 'Risk_gains_Display~N_fi_pm')
lm = fitlm(multiTb, 'Risk_gains_Display~caps_total_pm+N_fi_pm')

lm = fitlm(multiTb, 'Amb_loss_Display~caps_total_pm')
lm = fitlm(multiTb, 'Amb_loss_Display~N_fi_pm')
lm = fitlm(multiTb, 'Amb_loss_Display~caps_total_pm+N_fi_pm')

lm = fitlm(multiTb, 'Risk_loss_Display~caps_total_pm')
lm = fitlm(multiTb, 'Risk_loss_Display~N_fi_pm')
lm = fitlm(multiTb, 'Risk_loss_Display~caps_total_pm+N_fi_pm')





    
    
    
    
    
    
    
    
    
    
    
    
