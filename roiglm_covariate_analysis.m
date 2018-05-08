clearvars

root = 'D:\Ruonan\Projects in the lab\VA_RA_PTB\Imaging analysis\Imaging_analysis_091117';

roiglmFile = 'ROIGLM_Bartra-vSTR_SV.txt';
roiglmTable = readtable(fullfile(root, roiglmFile));

covFile = 'covariates_091517.txt';
capsTable = readtable(fullfile(root, covFile));

%% correlation 
betaName = 'Amb_loss_Display x p1';
cluster = 'caps_total_pm';

%% Scatter plot
x = capsTable.(cluster);
y = roiglmTable.beta(strcmp(roiglmTable.Predictor, betaName));

figure    
scatter(x,y);
hold on
                
%% robust regression
[b,robustmdl1]= robustfit(x,y); %robust regression
linex = linspace(0,max(x)+5);
liney = b(2)*linex+b(1);
plot(linex, liney, 'color','k');
                
% print text of r2 and p value
txt1 = ['coeff = ', num2str(robustmdl1.coeffcorr(1,2))];
txt2 = ['p = ', num2str(round(robustmdl1.p(2),4,'significant'))];
xlab = xlim;
ylab = ylim;
txt = {txt1;txt2};
text(xlab(2)-(xlab(2)-xlab(1))/4, ylab(2), txt, 'FontSize',8)

title([cluster ' with ' betaName ' Robust']) 

                
%% ordinary linear regression
mdl1 = LinearModel.fit(x,y); % creates a linear model of the responses y to a tb matrix x
coeff = table2array(mdl1.Coefficients);
linex = linspace(0,max(x)+5);
liney = coeff(2,1)*linex+coeff(1,1);
plot(linex, liney, 'color','k');

% print text of r2 and p value
txt1 = ['R^{2} = ',num2str(mdl1.Rsquared.Adjusted)];
txt2 = ['p = ', num2str(round(coeff(2,4),4,'significant'))];
xlab = xlim;
ylab = ylim;
txt = {txt1;txt2};
text(xlab(2)-(xlab(2)-xlab(1))/4, ylab(2), txt, 'FontSize',8)

title([cluster ' with ' betaName]) 


