clearvars
close all

root='D:\Ruonan\Projects in the lab\VA_RA_PTB\Clinical and behavioral';
% filename = [root,'\Clinical_Behavioral_040518.txt'];
% % data=tdfread(filename);
% tb = readtable(filename);

load(fullfile(root,'all data.mat'));

%% caculate model free scores
tb.r = nanmean([tb.r25, tb.r50, tb.r75],2);
tb.a_r50 = nanmean([tb.a24_r50, tb.a50_r50, tb.a74_r50],2);

%% include subjects
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

%% two-way anova seperate for risk and ambiguity
% risk, all
[p,tbl,stats,terms]=anovan(tb.r(tb.isExcluded_behavior == 0),{tb.group(tb.isExcluded_behavior == 0);tb.isGain(tb.isExcluded_behavior == 0)},'full');

% risk, CC and PTSD
[p,tbl,stats,terms]=anovan(tb.r(tb.isExcluded_behavior == 0 & ~strcmp(tb.group, 'R')),{tb.group(tb.isExcluded_behavior == 0 & ~strcmp(tb.group, 'R') );tb.isGain(tb.isExcluded_behavior == 0 & ~strcmp(tb.group, 'R'))},'full');

% risk gain, CC and PTSD
[p,tbl,stats,terms]=anovan(tb.r(tb.isExcluded_behavior == 0 & ~strcmp(tb.group, 'R') & tb.isGain==1),{tb.group(tb.isExcluded_behavior == 0 & ~strcmp(tb.group, 'R') & tb.isGain==1 )},'full');


% ambig, all
[p,tbl,stats,terms]=anovan(tb.a_r50(tb.isExcluded_behavior == 0),{tb.group(tb.isExcluded_behavior == 0);tb.isGain(tb.isExcluded_behavior == 0)},'full');

% ambig, CC and PTSD
[p,tbl,stats,terms]=anovan(tb.a_r50(tb.isExcluded_behavior == 0 & ~strcmp(tb.group, 'R')),{tb.group(tb.isExcluded_behavior == 0 & ~strcmp(tb.group, 'R') );tb.isGain(tb.isExcluded_behavior == 0 & ~strcmp(tb.group, 'R'))},'full');

% ambig, loss
[p,tbl,stats,terms]=anovan(tb.a_r50(tb.isExcluded_behavior == 0 & tb.isGain==0 ),{tb.group(tb.isExcluded_behavior == 0 & tb.isGain==0) },'full');

% ambig, loss, CC and PTSD
[p,tbl,stats,terms]=anovan(tb.a_r50(tb.isExcluded_behavior == 0 & tb.isGain==0 & ~strcmp(tb.group, 'R')),{tb.group(tb.isExcluded_behavior == 0 & tb.isGain==0 & ~strcmp(tb.group, 'R')) },'full');


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
plotmeanRisk = [nanmean(tbgain.r(vccidxgain)),nanmean(tbgain.r(ptsdidxgain)),nanmean(tbgain.r(fptsdidxgain));...
    nanmean(tbloss.r(vccidxloss)),nanmean(tbloss.r(ptsdidxloss)),nanmean(tbloss.r(fptsdidxloss))];

plotsemRisk = [std(tbgain.r(vccidxgain))/sqrt(length(tbgain.r(vccidxgain))),...
    std(tbgain.r(ptsdidxgain))/sqrt(length(tbgain.r(ptsdidxgain))),...
    std(tbgain.r(fptsdidxgain))/sqrt(length(tbgain.r(fptsdidxgain)));...
    ...
    std(tbloss.r(vccidxloss))/sqrt(length(tbloss.r(vccidxloss))),...
    std(tbloss.r(ptsdidxloss))/sqrt(length(tbloss.r(ptsdidxloss))),...
    std(tbloss.r(fptsdidxloss))/sqrt(length(tbloss.r(fptsdidxloss)))];


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
% ax.YLim = [-0.7,0.6];

title('model free risk')

leg = legend('CC','PTSD','RPTSD');
leg.FontSize = 20;



%ambig graph:
plotmeanAmbig = [nanmean(tbgain.a_r50(vccidxgain)),nanmean(tbgain.a_r50(ptsdidxgain)),nanmean(tbgain.a_r50(fptsdidxgain));...
    nanmean(tbloss.a_r50(vccidxloss)),nanmean(tbloss.a_r50(ptsdidxloss)),nanmean(tbloss.a_r50(fptsdidxloss))];

% note that subject 120 does not have beta for gains. have to use nanstd to
% calculate sem
plotsemAmbig = [std(tbgain.a_r50(vccidxgain))/sqrt(length(tbgain.a_r50(vccidxgain))),...
    nanstd(tbgain.a_r50(ptsdidxgain))/sqrt(length(tbgain.a_r50(ptsdidxgain))-1),...
    std(tbgain.a_r50(fptsdidxgain))/sqrt(length(tbgain.a_r50(fptsdidxgain)));...
    ...
    std(tbloss.a_r50(vccidxloss))/sqrt(length(tbloss.a_r50(vccidxloss))),...
    std(tbloss.a_r50(ptsdidxloss))/sqrt(length(tbloss.a_r50(ptsdidxloss))),...
    std(tbloss.a_r50(fptsdidxloss))/sqrt(length(tbloss.a_r50(fptsdidxloss)))];


fig = figure
set(fig, 'Position', [90 200 1120 700])
bplot = bar(plotmeanAmbig);
hold on
errorbar([1,2]-0.22,plotmeanAmbig(:,1),plotsemAmbig(:,1),'.','Color',[0,0,0],'LineWidth',2);
hold on
errorbar([1,2],plotmeanAmbig(:,2),plotsemAmbig(:,2),'.','Color',[0,0,0],'LineWidth',2);
hold on
errorbar([1,2]+0.22,plotmeanAmbig(:,3),plotsemAmbig(:,3),'.','Color',[0,0,0],'LineWidth',2);

% bar color
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
% ax.YLim = [-1.2,0.6];

title('model free ambiguity')

leg = legend('CC','PTSD','RPTSD');
leg.FontSize = 20;

% % one sample t test
% [h,p,ci,stats] = ttest(tbloss.r(vccidxloss))
% 
% [h,p,ci,stats] = ttest(tbgain.a_r50(vccidxgain))
% 
% 
% % ambig, loss anova on way
% [p,tbl,stats,terms]=anovan(tb.a_r50(tb.isExcluded_behavior == 0 & tb.isGain==0 ),{tb.group(tb.isExcluded_behavior == 0 & tb.isGain==0)},'full');
