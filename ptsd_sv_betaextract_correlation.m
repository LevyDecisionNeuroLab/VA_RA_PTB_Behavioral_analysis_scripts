clearvars

root=pwd;

file = 'BetaExtracts_SVModel_ROIBartra-vStr.txt';
filename = fullfile(root,file);
tbold = readtable(filename);
%exclude subject 1300
tb = tbold(tbold.Subject ~= 1300,:);

covfile =fullfile(root, 'covariates_091517.txt');
covold = readtable(covfile);
%exclude subject 1300
cov = covold(~strcmp(covold.id, 'subj1300'), :);

% tb and cov should have a same subject list in the same order

%% correlation 
betaNames = {'Amb_gains_Display', 'Amb_gains_DisplayXP1','Risk_gains_Display','Risk_gains_DisplayXP1',...
            'Amb_loss_Display','Amb_loss_DisplayXP1', 'Risk_loss_Display', 'Risk_loss_DisplayXP1'};
clusterNames = {'R_fi_pm', 'A_fi_pm', 'N_fi_pm', 'DA_fi_pm', 'AA_fi_pm', 'caps_total_pm'};
fitTypes = {'ordinary', 'robust'};

% individual plot and fitting
betaName = 'Risk_gains_DisplayXP1';
clusterName = 'caps_total_pm';
fitType = 'ordinary';

%% Scatter plot and regression fitting
y = tb.(betaName);
x = cov.(clusterName);
% x = log(x);

figure    
scatter(x,y, 'filled');
hold on
ax = gca;
% ax.XTickLabel = '';
ax.Box = 'off';
ax.FontSize = 20;
ax.LineWidth =3;

                
if strcmp(fitType,'robust')
    % robust regression
    [b,robustmdl1]= robustfit(x,y); 
    linex = linspace(0,max(x)+5);
    liney = b(2)*linex+b(1);
    plot(linex, liney, 'color','k', 'LineWidth', 2);

    % print text of coeff and p value
    txt1 = ['coeff = ', num2str(b(2))];
    txt2 = ['p = ', num2str(round(robustmdl1.p(2),4,'significant'))];
    xlab = xlim;
    ylab = ylim;
    txt = {txt1;txt2};
    text(xlab(2)-(xlab(2)-xlab(1))/4, ylab(2), txt, 'FontSize',8)
   
    title([clusterName ' with ' betaName ' Robust'], 'FontSize', 10)
    
    

    
elseif strcmp(fitType, 'ordinary')
    % ordinary linear regression
    mdl1 = LinearModel.fit(x,y); % creates a linear model of the responses y to a tb matrix x
    coeff = table2array(mdl1.Coefficients);
    linex = linspace(0,max(x)+5);
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

    title([clusterName ' with ' betaName ' OLS'], 'FontSize', 10)
    
    % residual plot
    figure
    scatter(x,mdl1.Residuals.Raw)
    hold on
    title([clusterName ' with ' betaName ' OLS ' 'Residuals'])

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
cov.Subject = tb.Subject; % two tables must share a same column
multiTb = join(cov,tb); % table for multi linear regression
% 
lm = fitlm(multiTb, 'Risk_loss_DisplayXP1~R_fi_pm+A_fi_pm+N_fi_pm+DA_fi_pm+AA_fi_pm')
% 
% lm = fitlm(multiTb, 'Amb_gains_Display~R_fi_pm+A_fi_pm+N_fi_pm+DA_fi_pm+AA_fi_pm')
% lm = fitlm(multiTb, 'Risk_gains_Display~R_fi_pm+A_fi_pm+N_fi_pm+DA_fi_pm+AA_fi_pm')
% lm = fitlm(multiTb, 'Amb_loss_Display~R_fi_pm+A_fi_pm+N_fi_pm+DA_fi_pm+AA_fi_pm')
% lm = fitlm(multiTb, 'Risk_loss_Display~R_fi_pm+A_fi_pm+N_fi_pm+DA_fi_pm+AA_fi_pm')
% 
% 
% 
% lm = fitlm(multiTb, 'All~caps_total_pm')
% lm = fitlm(multiTb, 'All~N_fi_pm')
% lm = fitlm(multiTb, 'All~caps_total_pm+N_fi_pm')
% 
% lm = fitlm(multiTb, 'Amb_gains_Display~caps_total_pm')
% lm = fitlm(multiTb, 'Amb_gains_Display~N_fi_pm')
% lm = fitlm(multiTb, 'Amb_gains_Display~caps_total_pm+N_fi_pm')
% 
% lm = fitlm(multiTb, 'Risk_gains_Display~caps_total_pm')
% lm = fitlm(multiTb, 'Risk_gains_Display~N_fi_pm')
% lm = fitlm(multiTb, 'Risk_gains_Display~caps_total_pm+N_fi_pm')
% 
% lm = fitlm(multiTb, 'Amb_loss_Display~caps_total_pm')
% lm = fitlm(multiTb, 'Amb_loss_Display~N_fi_pm')
% lm = fitlm(multiTb, 'Amb_loss_Display~caps_total_pm+N_fi_pm')
% 
% lm = fitlm(multiTb, 'Risk_loss_Display~caps_total_pm')
% lm = fitlm(multiTb, 'Risk_loss_Display~N_fi_pm')
% lm = fitlm(multiTb, 'Risk_loss_Display~caps_total_pm+N_fi_pm')





    
    
    
    
    
    
    
    
    
    
    
    
