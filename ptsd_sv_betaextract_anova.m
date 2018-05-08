clearvars

root=pwd;
file = 'BetaExtracts_SVModel_ROIBartra-vStr.txt';
filename = fullfile(root,file);
tbold = readtable(filename);

%exclude subject 1300
tb = tbold(tbold.Subject ~= 1300,:);

groupfile = [root,'\subject_group_091517.txt'];
grouptb = readtable(groupfile);

groups = {'CC', 'PTSD', 'FPTSD'};

tb.group = cell(length(tb.Subject),1);

for i = 1:length(tb.Subject)
    tb.group{i} = grouptb.group{grouptb.id == tb.Subject(i)};
end

% creat data table that is not cross-tabed, for anova
analytbNames = {'id', 'group', 'isGain', 'isRisk', 'svbeta'};
id = repmat(tb.Subject,4,1);
group = repmat(tb.group,4,1);
isGain = [ones(height(tb)*2,1);zeros(height(tb)*2,1)];
isRisk = [zeros(height(tb),1); ones(height(tb),1); zeros(height(tb),1); ones(height(tb),1)];
svBeta = [tb.Amb_gains_DisplayXP1; tb.Risk_gains_DisplayXP1; tb.Amb_loss_DisplayXP1; tb.Risk_loss_DisplayXP1];

analytb = table(id, group, isGain, isRisk, svBeta);
%% Avona
% gain/loss, risk/ambig, CC/PTSD
[p,tbl,stats,terms]=anovan(analytb.svBeta(~strcmp(analytb.group, 'FPTSD')),...
    {analytb.isGain(~strcmp(analytb.group, 'FPTSD'));...
    analytb.isRisk(~strcmp(analytb.group, 'FPTSD'));...
    analytb.group(~strcmp(analytb.group, 'FPTSD'))},'full');

% gain, risk/ambig, CC/PTSD
[p,tbl,stats,terms]=anovan(analytb.svBeta(~strcmp(analytb.group, 'FPTSD') & analytb.isGain ==1),...
    {analytb.isRisk(~strcmp(analytb.group, 'FPTSD')& analytb.isGain ==1);...
    analytb.group(~strcmp(analytb.group, 'FPTSD')& analytb.isGain ==1)},'full');

% loss, risk/ambig, CC/PTSD
[p,tbl,stats,terms]=anovan(analytb.svBeta(~strcmp(analytb.group, 'FPTSD') & analytb.isGain ==0),...
    {analytb.isRisk(~strcmp(analytb.group, 'FPTSD')& analytb.isGain ==0);...
    analytb.group(~strcmp(analytb.group, 'FPTSD')& analytb.isGain ==0)},'full');


%% Bargraph
% each row represents: AG ,RG,AL,RL(CC),  AG ,RG,AL,RL(PTSD),
% each column represents: CC, PTSD
plotmean = [nanmean(analytb.svBeta(analytb.isGain == 1 & analytb.isRisk ==0 & strcmp(analytb.group, 'CC'))),...
    nanmean(analytb.svBeta(analytb.isGain == 1 & analytb.isRisk ==0 & strcmp(analytb.group, 'PTSD')));,...
    nanmean(analytb.svBeta(analytb.isGain == 1 & analytb.isRisk ==1 & strcmp(analytb.group, 'CC'))),...
    nanmean(analytb.svBeta(analytb.isGain == 1 & analytb.isRisk ==1 & strcmp(analytb.group, 'PTSD')));...
    nanmean(analytb.svBeta(analytb.isGain == 0 & analytb.isRisk ==0 & strcmp(analytb.group, 'CC'))),...
    nanmean(analytb.svBeta(analytb.isGain == 0 & analytb.isRisk ==0 & strcmp(analytb.group, 'PTSD')));...    
    nanmean(analytb.svBeta(analytb.isGain == 0 & analytb.isRisk ==1 & strcmp(analytb.group, 'CC'))),...
    nanmean(analytb.svBeta(analytb.isGain == 0 & analytb.isRisk ==1 & strcmp(analytb.group, 'PTSD')))...
    ];
plotstd = [nanstd(analytb.svBeta(analytb.isGain == 1 & analytb.isRisk ==0 & strcmp(analytb.group, 'CC'))),...
    nanstd(analytb.svBeta(analytb.isGain == 1 & analytb.isRisk ==0 & strcmp(analytb.group, 'PTSD')));...  
    nanstd(analytb.svBeta(analytb.isGain == 1 & analytb.isRisk ==1 & strcmp(analytb.group, 'CC'))),...
    nanstd(analytb.svBeta(analytb.isGain == 1 & analytb.isRisk ==1 & strcmp(analytb.group, 'PTSD')));...   
    nanstd(analytb.svBeta(analytb.isGain == 0 & analytb.isRisk ==0 & strcmp(analytb.group, 'CC'))),...
    nanstd(analytb.svBeta(analytb.isGain == 0 & analytb.isRisk ==0 & strcmp(analytb.group, 'PTSD')));,...    
    nanstd(analytb.svBeta(analytb.isGain == 0 & analytb.isRisk ==1 & strcmp(analytb.group, 'CC'))),...
    nanstd(analytb.svBeta(analytb.isGain == 0 & analytb.isRisk ==1 & strcmp(analytb.group, 'PTSD')))...
    ];
plotcount = [length(analytb.svBeta(analytb.isGain == 1 & analytb.isRisk ==0 & strcmp(analytb.group, 'CC'))),...
    length(analytb.svBeta(analytb.isGain == 1 & analytb.isRisk ==0 & strcmp(analytb.group, 'PTSD')));...    
    length(analytb.svBeta(analytb.isGain == 1 & analytb.isRisk ==1 & strcmp(analytb.group, 'CC'))),...
    length(analytb.svBeta(analytb.isGain == 1 & analytb.isRisk ==1 & strcmp(analytb.group, 'PTSD')));...    
    length(analytb.svBeta(analytb.isGain == 0 & analytb.isRisk ==0 & strcmp(analytb.group, 'CC'))),...
     length(analytb.svBeta(analytb.isGain == 0 & analytb.isRisk ==0 & strcmp(analytb.group, 'PTSD')));...   
    length(analytb.svBeta(analytb.isGain == 0 & analytb.isRisk ==1 & strcmp(analytb.group, 'CC'))),...
    length(analytb.svBeta(analytb.isGain == 0 & analytb.isRisk ==1 & strcmp(analytb.group, 'PTSD')))...
    ];
plotsem = plotstd ./ sqrt(plotcount);

% figure, beta bar plots for four conditions

fig = figure
set(fig, 'Position', [90 200 1120 700])
bplot = bar(plotmean);
hold on
errorbar([1,2,3,4]-0.145, plotmean(:,1), plotsem(:,2), '.','Color',[0,0,0],'LineWidth',2);
hold on
errorbar([1,2,3,4]+0.145,plotmean(:,2),plotsem(:,2),'.','Color',[0,0,0],'LineWidth',2);


%bar color
bplot(1).FaceColor = [104,160,66]/255;
bplot(1).EdgeColor = [104,160,66]/255;
bplot(2).FaceColor = [237,125,49]/255;
bplot(2).EdgeColor = [237,125,49]/255;
bplot(1).BarWidth = 0.9;

%axis property
ax = gca;
% ax.XTickLabel = {'AG','RG','AL','RL'};
ax.XTickLabel = '';
ax.Box = 'off';
ax.FontSize = 35;
ax.LineWidth =4;
% ax.YLabel.String = 'beta';
% ax.YLabel.FontSize = 18;
ax.YLim = [-0.05, 0.08];

title(file, 'FontSize',16)

leg = legend('CC','PTSD');
leg.FontSize = 25;
