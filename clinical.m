% symptoms
clearvars
close all

root='D:\Ruonan\Projects in the lab\VA_RA_PTB\Clinical and behavioral';
% filename = [root,'\Clinical_Behavioral_040518.txt'];
% % data=tdfread(filename);
% tb = readtable(filename);

load(fullfile(root,'all data.mat'));

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

%%

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

% tbnames = fieldnames(tb);

% for gains
% includegain =find(tb.isExcluded_behavior == 0 & tb.isGain == 1);
tb2plot = tb(include,:);

% for i = 1:length(tbnames)
%    tbgain.(tbnames{i})= tb.(tbnames{i})(includegain);
% end

vccidx = [];
ptsdidx = [];
fptsdidx = [];

for i=1:length(tb2plot.id)
   if ismember(tb2plot.id(i),vccsubj) ==1
       vccidx = [vccidx,i];
   elseif ismember(tb2plot.id(i),ptsdsubj)==1
       ptsdidx = [ptsdidx,i];
   elseif ismember(tb2plot.id(i),fptsdsubj)==1
       fptsdidx = [fptsdidx,i];
   end
end


% which clinical measurement to plot
symptomname = 'BDI';
symptom = tb2plot.bdiii_total;

plotmean = [nanmean(symptom(vccidx));nanmean(symptom(ptsdidx));nanmean(symptom(fptsdidx))];

plotsem = [nanstd(symptom(vccidx))/sqrt(sum(~isnan(symptom(vccidx))));...
    nanstd(symptom(ptsdidx))/sqrt(sum(~isnan(symptom(ptsdidx))));...
    nanstd(symptom(fptsdidx))/sqrt(sum(~isnan(symptom(fptsdidx))))];

fig = figure
aHand = axes('parent',fig);
hold(aHand,'on')
set(fig, 'Position', [90 200 1120 700])
bar(1,plotmean(1), 'parent',  aHand ,'facecolor', [104,160,66]/255,'edgecolor', [104,160,66]/255);
bar(2,plotmean(2), 'parent',  aHand ,'facecolor', [237,125,49]/255,'edgecolor', [237,125,49]/255);
bar(3,plotmean(3), 'parent',  aHand ,'facecolor', [165,165,165]/255,'edgecolor', [165,165,165]/255);

% bplot = bar(plotmean);
% bplot1 = bar(1,plotmean(1));
% hold on
% bplot2 = bar(2,plotmean(2));
% hold on
% bplot3 = bar(2,plotmean(3));
errorbar([2]-1,plotmean(1),plotsem(1),'.','Color',[0,0,0],'LineWidth',2);
errorbar([2],plotmean(2),plotsem(2),'.','Color',[0,0,0],'LineWidth',2);
errorbar([2]+1,plotmean(3),plotsem(3),'.','Color',[0,0,0],'LineWidth',2);

%axis property
ax = gca;
% ax.XTick = [1,2,3,4];
ax.Box = 'off';
ax.FontSize = 35;
ax.LineWidth =3;
% ax.YLabel.String = 'transformed alpha'; 
ax.YLabel.FontSize = 35;
ax.XTickLabel = [];
% ax.YLim = [-0.7,0.6];


bplot(1).BarWidth = 0.9;



title(symptomname)

leg = legend('CC','PTSD','RPTSD');
leg.FontSize = 20;

