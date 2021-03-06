clearvars

root='D:\Ruonan\Projects in the lab\VA_RA_PTB\Clinical and behavioral';

% filename = [root,'\Clinical_Behavioral_040518.txt'];
% tb = readtable(filename);

load(fullfile(root,'all data.mat'));
load(fullfile(root,'all data_male.mat'));

% survey = readtable(fullfile(root, 'survey_091517.txt'));

%% two-way anova seperate for risk and ambiguity
% risk, all
[p,tbl,stats,terms]=anovan(tb.alpha_t(tb.isExcluded_behavior == 0),{tb.group(tb.isExcluded_behavior == 0);tb.isGain(tb.isExcluded_behavior == 0)},'full');

% risk, CC and PTSD
[p,tbl,stats,terms]=anovan(tb.alpha_t(tb.isExcluded_behavior == 0 & ~strcmp(tb.group, 'R')),{tb.group(tb.isExcluded_behavior == 0 & ~strcmp(tb.group, 'R') );tb.isGain(tb.isExcluded_behavior == 0 & ~strcmp(tb.group, 'R'))},'full');

% risk gain, CC and PTSD
[p,tbl,stats,terms]=anovan(tb.alpha_t(tb.isExcluded_behavior == 0 & ~strcmp(tb.group, 'R') & tb.isGain==1),{tb.group(tb.isExcluded_behavior == 0 & ~strcmp(tb.group, 'R') & tb.isGain==1 )},'full');

% risk gain, CC and PTSD and RPTSD
[p,tbl,stats,terms]=anovan(tb.alpha_t(tb.isExcluded_behavior == 0  & tb.isGain==1),{tb.group(tb.isExcluded_behavior == 0  & tb.isGain==1 )},'full');


% ambig, all
[p,tbl,stats,terms]=anovan(tb.beta_t(tb.isExcluded_behavior == 0),{tb.group(tb.isExcluded_behavior == 0);tb.isGain(tb.isExcluded_behavior == 0)},'full');

% ambig, CC and PTSD
[p,tbl,stats,terms]=anovan(tb.beta_t(tb.isExcluded_behavior == 0 & ~strcmp(tb.group, 'R')),{tb.group(tb.isExcluded_behavior == 0 & ~strcmp(tb.group, 'R') );tb.isGain(tb.isExcluded_behavior == 0 & ~strcmp(tb.group, 'R'))},'full');

% ambig, loss
[p,tbl,stats,terms]=anovan(tb.beta_t(tb.isExcluded_behavior == 0 & tb.isGain==0 ),{tb.group(tb.isExcluded_behavior == 0 & tb.isGain==0) },'full');

% ambig, loss, CC and PTSD
[p,tbl,stats,terms]=anovan(tb.beta_t(tb.isExcluded_behavior == 0 & tb.isGain==0 & ~strcmp(tb.group, 'R')),{tb.group(tb.isExcluded_behavior == 0 & tb.isGain==0 & ~strcmp(tb.group, 'R')) },'full');

% ambig, loss, CC and PTSD and RPTSD
[p,tbl,stats,terms]=anovan(tb.beta_t(tb.isExcluded_behavior == 0 & tb.isGain==0 ),{tb.group(tb.isExcluded_behavior == 0 & tb.isGain==0) },'full');


% three-way anova
tb3 = readtable([root,'\clinical_behavioral_091517_alphaBeta.txt']);
tb4 = tb3(~strcmp(tb3.group, 'R'), :);

tb2 = tb4; % only cc and ptsd
tb2=tb3;% all groups

interterms = [1 0 0 0;0 1 0 0; 0 0 1 0;0 0 0 1; 1 1 0 0 ; 1 0 1 0; 0 1 1 0; 1 1 1 0];
[p,tbl,stats,terms]=anovan(tb2.cstr_trf(tb2.isExcluded_behavior == 0),{tb2.group(tb2.isExcluded_behavior == 0);tb2.isGain(tb2.isExcluded_behavior == 0); tb2.isAlpha(tb2.isExcluded_behavior == 0); tb2.id(tb2.isExcluded_behavior == 0);},...
                    'random', 4, 'nested', [0 0 0 0; 0 0 0 0; 0 0 0 0; 1 0 0 0], 'model',interterms);
   % risk
[p,tbl,stats,terms]=anovan(tb2.cstr_trf(tb2.isExcluded_behavior == 0 & tb2.isAlpha == 1),{tb2.group(tb2.isExcluded_behavior == 0 & tb2.isAlpha == 1);tb2.isGain(tb2.isExcluded_behavior == 0 & tb2.isAlpha == 1); tb2.id(tb2.isExcluded_behavior == 0 & tb2.isAlpha == 1);},...
                    'random', 3, 'nested', [0 0 0 ; 0 0 0 ; 1 0 0 ], 'model',[1 0 0; 0 1 0; 0 0 1; 1 1 0]);
   % ambiguity
[p,tbl,stats,terms]=anovan(tb2.cstr_trf(tb2.isExcluded_behavior == 0 & tb2.isAlpha == 0),{tb2.group(tb2.isExcluded_behavior == 0 & tb2.isAlpha == 0);tb2.isGain(tb2.isExcluded_behavior == 0 & tb2.isAlpha == 0); tb2.id(tb2.isExcluded_behavior == 0 & tb2.isAlpha == 0);},...
                    'random', 3, 'nested', [0 0 0 ; 0 0 0 ; 1 0 0 ], 'model',[1 0 0; 0 1 0; 0 0 1; 1 1 0]);
% cross tab
[tbl,chi2,p,factorvals] = crosstab(tb2.group(tb2.isExcluded_behavior == 0), tb2.isGain(tb2.isExcluded_behavior == 0), tb2.isAlpha(tb2.isExcluded_behavior == 0))

%% bar graph of paramatric uncertainty attitudes of different groups
% find groups
include =find(tb.isExcluded_behavior == 0 & tb.isGain == 1);

% find out all the corresponding subj number of each group VCC/PTSD
vccsubj = [];
ptsdsubj = [];
fptsdsubj = [];
for i = 1:length(include)
   if strcmp(tb.group{include(i)},'C')==1
       vccsubj = [vccsubj,tb.id(include(i))];
   end
  
   if strcmp(tb.group{include(i)},'P')==1
       ptsdsubj = [ptsdsubj,tb.id(include(i))];
   end
   
   if strcmp(tb.group{include(i)},'R')==1
       fptsdsubj = [fptsdsubj,tb.id(include(i))];
   end
end

tbnames = fieldnames(tb);

% for gains
includegain =find(tb.isExcluded_behavior == 0 & tb.isGain == 1);
tbgain = tb(includegain,:);

% for i = 1:length(tbnames)
%    tbgain.(tbnames{i})= tb.(tbnames{i})(includegain);
% end

vccidxgain = [];
ptsdidxgain = [];
fptsdidxgain = [];
for i=1:length(tbgain.id)
   if ismember(tbgain.id(i),vccsubj) ==1
       vccidxgain = [vccidxgain,i];
   elseif ismember(tbgain.id(i),ptsdsubj)==1
       ptsdidxgain = [ptsdidxgain,i];
   elseif ismember(tbgain.id(i),fptsdsubj)==1
       fptsdidxgain = [fptsdidxgain,i];
   end
end

% for loss
includeloss =find(tb.isExcluded_behavior == 0 & tb.isGain == 0);
tbloss = tb(includeloss,:);


vccidxloss = [];
ptsdidxloss = [];
fptsdidxloss = [];
for i=1:length(tbloss.id)
   if ismember(tbloss.id(i),vccsubj) ==1
       vccidxloss = [vccidxloss,i];
   elseif ismember(tbloss.id(i),ptsdsubj)==1
       ptsdidxloss = [ptsdidxloss,i];
   elseif ismember(tbloss.id(i),fptsdsubj)==1
       fptsdidxloss = [fptsdidxloss,i];
   end
end

% bar graph
% Risk- gain and loss
% ambig-gain and loss
% group 1: vcc, group2: ptsd, group3: fptsd

%risk graph:
plotmeanRisk = [nanmean(tbgain.alpha_t(vccidxgain)),nanmean(tbgain.alpha_t(ptsdidxgain)),nanmean(tbgain.alpha_t(fptsdidxgain));...
    nanmean(tbloss.alpha_t(vccidxloss)),nanmean(tbloss.alpha_t(ptsdidxloss)),nanmean(tbloss.alpha_t(fptsdidxloss))];

plotsemRisk = [std(tbgain.alpha_t(vccidxgain))/sqrt(length(tbgain.alpha_t(vccidxgain))),...
    std(tbgain.alpha_t(ptsdidxgain))/sqrt(length(tbgain.alpha_t(ptsdidxgain))),...
    std(tbgain.alpha_t(fptsdidxgain))/sqrt(length(tbgain.alpha_t(fptsdidxgain)));...
    ...
    std(tbloss.alpha_t(vccidxloss))/sqrt(length(tbloss.alpha_t(vccidxloss))),...
    std(tbloss.alpha_t(ptsdidxloss))/sqrt(length(tbloss.alpha_t(ptsdidxloss))),...
    std(tbloss.alpha_t(fptsdidxloss))/sqrt(length(tbloss.alpha_t(fptsdidxloss)))];


fig = figure
set(fig, 'Position', [90 200 1120 700])
bplot = bar(plotmeanRisk);
hold on
errorbar([1,2]-0.22,plotmeanRisk(:,1),plotsemRisk(:,1),'.','Color',[0,0,0],'LineWidth',2);
hold on
errorbar([1,2],plotmeanRisk(:,2),plotsemRisk(:,2),'.','Color',[0,0,0],'LineWidth',2);
hold on
errorbar([1,2]+0.22,plotmeanRisk(:,3),plotsemRisk(:,3),'.','Color',[0,0,0],'LineWidth',2);

%bar color
bplot(1).FaceColor = [104,160,66]/255;
bplot(1).EdgeColor = [104,160,66]/255;
bplot(2).FaceColor = [237,125,49]/255;
bplot(2).EdgeColor = [237,125,49]/255;
bplot(3).FaceColor = [165,165,165]/255;
bplot(3).EdgeColor = [165,165,165]/255;

bplot(1).BarWidth = 0.9;


%axis property
ax = gca;
ax.XTickLabel = {'Gain','Loss'};
% ax.XTick = [1,2,3,4];
ax.Box = 'off';
ax.FontSize = 35;
ax.LineWidth =3;
ax.YLabel.String = 'transformed alpha'; 
ax.YLabel.FontSize = 35;
ax.YLim = [-0.7,0.6];

title('Parametric estimate of Risk attitude')

leg = legend('CC','PTSD','RPTSD');
leg.FontSize = 20;



%ambig graph:
plotmeanAmbig = [nanmean(tbgain.beta_t(vccidxgain)),nanmean(tbgain.beta_t(ptsdidxgain)),nanmean(tbgain.beta_t(fptsdidxgain));...
    nanmean(tbloss.beta_t(vccidxloss)),nanmean(tbloss.beta_t(ptsdidxloss)),nanmean(tbloss.beta_t(fptsdidxloss))];

% note that subject 120 does not have beta for gains. have to use nanstd to
% calculate sem
plotsemAmbig = [std(tbgain.beta_t(vccidxgain))/sqrt(length(tbgain.beta_t(vccidxgain))),...
    nanstd(tbgain.beta_t(ptsdidxgain))/sqrt(length(tbgain.beta_t(ptsdidxgain))-1),...
    std(tbgain.beta_t(fptsdidxgain))/sqrt(length(tbgain.beta_t(fptsdidxgain)));...
    ...
    std(tbloss.beta_t(vccidxloss))/sqrt(length(tbloss.beta_t(vccidxloss))),...
    std(tbloss.beta_t(ptsdidxloss))/sqrt(length(tbloss.beta_t(ptsdidxloss))),...
    std(tbloss.beta_t(fptsdidxloss))/sqrt(length(tbloss.beta_t(fptsdidxloss)))];


fig = figure
set(fig, 'Position', [90 200 1120 700])
bplot = bar(plotmeanAmbig);
hold on
errorbar([1,2]-0.22,plotmeanAmbig(:,1),plotsemAmbig(:,1),'.','Color',[0,0,0],'LineWidth',2);
hold on
errorbar([1,2],plotmeanAmbig(:,2),plotsemAmbig(:,2),'.','Color',[0,0,0],'LineWidth',2);
hold on
errorbar([1,2]+0.22,plotmeanAmbig(:,3),plotsemAmbig(:,3),'.','Color',[0,0,0],'LineWidth',2);

%bar color
bplot(1).FaceColor = [104,160,66]/255;
bplot(1).EdgeColor = [104,160,66]/255;
bplot(2).FaceColor = [237,125,49]/255;
bplot(2).EdgeColor = [237,125,49]/255;
bplot(3).FaceColor = [165,165,165]/255;
bplot(3).EdgeColor = [165,165,165]/255;

bplot(1).BarWidth = 0.9;


%axis property
ax = gca;
ax.XTickLabel = {'Gain','Loss'};
% ax.XTick = [1,2,3,4];
ax.Box = 'off';
ax.FontSize = 35;
ax.LineWidth =3;
ax.YLabel.String = 'transformed beta'; 
ax.YLabel.FontSize = 35;
ax.YLim = [-1.2,0.6];

title('Parametric estimate of Ambiguity attitude')

leg = legend('CC','PTSD','RPTSD');
leg.FontSize = 20;

% % one sample t test
% [h,p,ci,stats] = ttest(tbloss.alpha_t(vccidxloss))
% 
% [h,p,ci,stats] = ttest(tbgain.beta_t(vccidxgain))
% 
% 
% % ambig, loss anova on way
% [p,tbl,stats,terms]=anovan(tb.beta_t(tb.isExcluded_behavior == 0 & tb.isGain==0 ),{tb.group(tb.isExcluded_behavior == 0 & tb.isGain==0)},'full');
% 

    %% FI there are outliers to exclude
    outexcluded = find(tb.beta_t(tb.isExcluded_behavior == 0 & tb.isGain == 0)>-1.5);
    hold on
    % linear regression
    mdl2 = LinearModel.fit(x(outexcluded),y(outexcluded)); % creates a linear model of the responses y to a tb matrix x
    coeff = table2array(mdl2.Coefficients);
    linex = linspace(0,max(x)+5);
    liney = coeff(2,1)*linex+coeff(1,1);
    plot(linex, liney, 'color','r');
    
    % print text of r2 and p value
    txt1 = ['R^{2} = ',num2str(mdl2.Rsquared.Adjusted)];
    txt2 = ['p = ', num2str(round(coeff(2,4),4,'significant'))];
    xlab = xlim;
    ylab = ylim;
    %dim = [0.8 0.8 0.3 0.1]; %[x y w h]
    txt = {txt1;txt2};
%     if i==1|i==2
%         text(xlab(2)-(xlab(2)-xlab(1))/4, ylab(2)+0.2, txt, 'FontSize',8)
%     else
    text(xlab(2)-(xlab(2)-xlab(1))/4, ylab(2)-(ylab(2)-ylab(1))/4, txt, 'FontSize',8, 'color', 'r')
    

