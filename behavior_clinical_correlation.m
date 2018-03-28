clearvars
close all

root=pwd;
filename = [root,'\clinical_behavioral_091517.txt'];
% data=tdfread(filename);
tb = readtable(filename);

%% regression alpha/beta with symptoms, auto
cluster = {'Re-experiencing','Avoidance','Emotional Numbing','Dysphoric Arousal','Anxious Arousal','Total'};
param = {'alpha','beta'};
domain = {'gain','loss'};

% which group to look at
include = strcmp(tb.group, 'PTSD');
% % Include all subjects
include = strcmp(tb.group, 'CC') | strcmp(tb.group, 'PTSD') | strcmp(tb.group, 'FPTSD');

%% Multilinear regression
% risk gain
multiTb = tb(tb.isGain == 1 & include, :);
multiLm = fitlm(multiTb, 'alpha_cstr_trf~R_fi_pm+A_fi_pm+N_fi_pm+DA_fi_pm+AA_fi_pm')

% ambig gain
multiTb = tb(tb.isGain == 1 & include, :);
multiLm = fitlm(multiTb, 'beta_cstr_trf~R_fi_pm+A_fi_pm+N_fi_pm+DA_fi_pm+AA_fi_pm')

% risk loss
multiTb = tb(tb.isGain == 0 & include, :);
multiLm = fitlm(multiTb, 'alpha_cstr_trf~R_fi_pm+A_fi_pm+N_fi_pm+DA_fi_pm+AA_fi_pm')

% ambig loss
multiTb = tb(tb.isGain == 0 & include, :);
multiLm = fitlm(multiTb, 'beta_cstr_trf~R_fi_pm+A_fi_pm+N_fi_pm+DA_fi_pm+AA_fi_pm')



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
                y = tb.alpha_cstr_trf(tb.isExcluded_behavior == 0 & domainIdx & include);
            elseif strcmp(param2plot, 'beta')
                y = tb.beta_cstr_trf(tb.isExcluded_behavior == 0 & domainIdx & include);
            end



            %     x = tb.N_fi_pm(tb.isExcluded_behavior == 0 & tb.isGain == 0);
            %     x = tb.caps_total_pm(tb.isExcluded_behavior == 0 & tb.isGain == 0);
            %     y = tb.beta_cstr_trf(tb.isExcluded_behavior == 0 & tb.isGain == 0);
                figure    
                scatter(x,y, 'filled');
                hold on
                % linear regression
                mdl1 = LinearModel.fit(x,y); % creates a linear model of the responses y to a tb matrix x
                coeff = table2array(mdl1.Coefficients);
                linex = linspace(0,max(x)+5);
                liney = coeff(2,1)*linex+coeff(1,1);
                plot(linex, liney, 'color','k');

                % print text of r2 and p value
                txt1 = ['R^{2} = ',num2str(mdl1.Rsquared.Ordinary)];
                txt2 = ['p = ', num2str(round(coeff(2,4),4,'significant'))];
                xlab = xlim;
                ylab = ylim;
                %dim = [0.8 0.8 0.3 0.1]; %[x y w h]
                txt = {txt1;txt2};
            %     if i==1|i==2
            %         text(xlab(2)-(xlab(2)-xlab(1))/4, ylab(2)+0.2, txt, 'FontSize',8)
            %     else
                text(xlab(2)-(xlab(2)-xlab(1))/4, ylab(2), txt, 'FontSize',8)
            %     end

                %text(xlab(2)-(xlab(2)-xlab(1))/4, ylab(2)+0.07, txt1, 'FontSize',8)
                %text(xlab(2)-(xlab(2)-xlab(1))/4, ylab(2)+0.02,txt2,'FontSize',8)

                %annotation('textbox',dim,'string',txt,'FitBoxToText','on');
                title(cluster2plot) % title of subplot
                ylabel([param2plot '-' domain2plot]) 
                
                % save the plot
        end
    end
end

% %% specify plot
% cluster2plot = 'Emotional Numbing';
% param2plot = 'beta';
% domain2plot = 'loss';
% 
% if strcmp(domain2plot,'gain')
%     domainIdx = (tb.isGain == 1);
% else
%     domainIdx = (tb.isGain == 0);
% end
% 
% if strcmp(cluster2plot,'Total')
%     x = tb.caps_total_pm(tb.isExcluded_behavior == 0 & domainIdx);
% elseif strcmp(cluster2plot,'Re-experiencing')
%     x = tb.R_fi_pm(tb.isExcluded_behavior == 0 & domainIdx);
% elseif strcmp(cluster2plot,'Avoidance')
%     x = tb.A_fi_pm(tb.isExcluded_behavior == 0 & domainIdx);
% elseif strcmp(cluster2plot,'Emotional Numbing')
%     x = tb.N_fi_pm(tb.isExcluded_behavior == 0 & domainIdx);
% elseif strcmp(cluster2plot,'Dysphoric Arousal')
%     x = tb.DA_fi_pm(tb.isExcluded_behavior == 0 & domainIdx);
% elseif strcmp(cluster2plot,'Anxious Arousal')
%     x = tb.AA_fi_pm(tb.isExcluded_behavior == 0 & domainIdx);
% end
% 
% if strcmp(param2plot, 'alpha')
%     y = tb.alpha_cstr_trf(tb.isExcluded_behavior == 0 & domainIdx);
% elseif strcmp(param2plot, 'beta')
%     y = tb.beta_cstr_trf(tb.isExcluded_behavior == 0 & domainIdx);
% end
% 
% 
% 
%     figure    
%     scatter(x,y);
%     hold on
%     % linear regression
%     mdl1 = LinearModel.fit(x,y); % creates a linear model of the responses y to a tb matrix x
%     coeff = table2array(mdl1.Coefficients);
%     linex = linspace(0,max(x)+5);
%     liney = coeff(2,1)*linex+coeff(1,1);
%     plot(linex, liney, 'color','k');
%     
%     % print text of r2 and p value
%     txt1 = ['R^{2} = ',num2str(mdl1.Rsquared.Adjusted)];
%     txt2 = ['p = ', num2str(round(coeff(2,4),4,'significant'))];
%     xlab = xlim;
%     ylab = ylim;
%     %dim = [0.8 0.8 0.3 0.1]; %[x y w h]
%     txt = {txt1;txt2};
% %     if i==1|i==2
% %         text(xlab(2)-(xlab(2)-xlab(1))/4, ylab(2)+0.2, txt, 'FontSize',8)
% %     else
%     text(xlab(2)-(xlab(2)-xlab(1))/4, ylab(2), txt, 'FontSize',8)
% %     end
% 
%     %text(xlab(2)-(xlab(2)-xlab(1))/4, ylab(2)+0.07, txt1, 'FontSize',8)
%     %text(xlab(2)-(xlab(2)-xlab(1))/4, ylab(2)+0.02,txt2,'FontSize',8)
%     
%     %annotation('textbox',dim,'string',txt,'FitBoxToText','on');
%     title(cluster2plot) % title of subplot
%     ylabel([param2plot '-' domain2plot]) 
% 
