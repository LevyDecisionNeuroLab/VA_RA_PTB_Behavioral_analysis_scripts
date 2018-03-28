clear

% load beta values
root = 'D:\Ruonan\VA_RA_PTB\Imaging analysis\Model_disp_resp\VA_PTB_imaging_analysis _report_101616\Beta values';

% % for single ROI
% roiname = 'N_AG_ACC22_Exclude3HighNumbing'; % need to change 
% fname = [roiname, '.xlsx'];
% roi = xlsread(fullfile(root,fname));


% if combing multiple ROIs, weith-average each ROI:
roiname1 = 'N_AG_ACC22_Exclude3HighNumbing'; % need to change 
fname1 = [roiname1, '.xlsx'];
roi1 = xlsread(fullfile(root,fname1));
roiname2 = 'N_AG_ACC14_Exclude3HighNumbing'; % need to change 
fname2 = [roiname2, '.xlsx'];
roi2 = xlsread(fullfile(root,fname2));

roiname = 'N_AG_ACC36_Exclude3HighNumbing';

roi(:,1) = roi1(:,1);
for i=2:size(roi1,2)
   roi(:,i) = (22*roi1(:,i)+14*roi2(:,i))/36; 
end
covar = 'Numbing'; % need to change

%% exclude outliers. This should be enabled only when outliers are found in the graph
% roi(find(roi(:,2) >1.25),4) = NaN;
% roi(find(roi(:,2) >1.25),6) = NaN;
% roi(find(roi(:,2) >1.25),8) = NaN;
% roi(find(roi(:,2) >1.25),2) = NaN;
exclude = [45 82 98];

%% load CAPS_5-factors
% 1-ID, 2-Re-exp, 3-Avoid, 4-Numb, 5-DysArous, 6-AnxArous, 7-Total
caps_5f_raw = xlsread(fullfile(root,'CAPS_5-factor_47.xlsx'));


% reorder, match subjects 
for i = 1:length(roi)
    caps_5f(i,:) = caps_5f_raw(find(caps_5f_raw(:,1)==roi(i,1)),:);
end

caps_5f(find(ismember(caps_5f(:,1),exclude)),2:size(caps_5f,2))=NaN;
clusters = {'ID','Re-experiencing','Avoidance','Numbing','Dysphoric Arousal','Anxious Arousal','Total'};
conditions = {'ID','AG','AG_R', 'RG','RG_R', 'AL','AL_R', 'RL', 'RL_R','Const'}; %none model
% conditions = {'ID','AG','AGxP','AG_R', 'AG_RxP','RG','RGxP','RG_R', 'RG_RxP','AL','ALxP','AL_R','AL_RxP', 'RL', 'RLxP','RL_R','RL_RxP','Const'}; %none model

%% figure without param
% figure for four conditions
fig = figure(2);
set(fig, 'Position', [90 200 1120 700])
set(fig, 'name', ['ROI: ', roiname])

for i = 1:4
    cond = conditions(i*2); % need to change
    subplot(2,2,i) % add first plot in 2 x 2 grid
    
    
    x = caps_5f(:,find(ismember(clusters,covar)));
    y = roi(:,find(ismember(conditions, cond)));
    scatter(x,y);
    hold on
    % linear regression
    mdl1 = LinearModel.fit(x,y); % creates a linear model of the responses y to a data matrix x
    coeff = table2array(mdl1.Coefficients);
    linex = linspace(0,max(x)+5);
    liney = coeff(2,1)*linex+coeff(1,1);
    plot(linex, liney, 'color','k');
    
    % print text of r2 and p value
    txt1 = ['R^{2} = ',num2str(mdl1.Rsquared.Adjusted)];
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
    title([cond]) % title of subplot
    
%     if i ==1
%         text(xlab(1)-5,ylab(2)+0.1,{'ROI: ';roiname})
%     end
end

% add x, y labels
subplot(2,2,3)
ylabel('beta') % label for y axis
xlabel(['CAPS-',covar]) % label for x axis

% save figure
%   print(['ROI_', roiname,],'-dpng')
  print(['ROI_', roiname, '_excluding 3 high numbing'],'-dpng')

%  print(['ROI_', 'AA_AL_lSTG16',],'-dpng')

%% % figure witrh param
% % figure for four conditions
% fig = figure(2);
% set(fig, 'Position', [90 200 1120 700])
% set(fig, 'name', ['ROI: ', roiname])
% 
% for i = 1:4
%     cond = conditions(4*(i-1)+3); % need to change
%     subplot(2,2,i) % add first plot in 2 x 2 grid
% 
%     x = caps_5f(:,find(ismember(clusters,covar)));
%     y = roi(:,find(ismember(conditions, cond)));
%     scatter(x,y);
%     hold on
%     % linear regression
%     mdl1 = LinearModel.fit(x,y); % creates a linear model of the responses y to a data matrix x
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
%     title([cond]) % title of subplot
%     
% %     if i ==1
% %         text(xlab(1)-5,ylab(2)+0.1,{'ROI: ';roiname})
% %     end
% end
% 
% % add x, y labels
% subplot(2,2,3)
% ylabel('beta') % label for y axis
% xlabel(['CAPS-',covar]) % label for x axis
% 
% % save figure
%   print(['ROI_', roiname,],'-dpng')
%  % print(['ROI_', roiname, '_excluding outliers'],'-dpng')
% 
% %  print(['ROI_', 'AA_AL_lSTG16',],'-dpng')







