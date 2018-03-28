clearvars

root=pwd;
filename = [root,'\clinical_behavioral_091517.txt'];
tb = readtable(filename);

survey = readtable(fullfile(root, 'survey_091517.txt'));

%% bar graph of survey of different groups
% find groups
include =find(tb.isExcluded_behavior == 0 & tb.isGain == 1);

% find out all the corresponding subj number of each group VCC/PTSD
vccsubj = [];
ptsdsubj = [];
fptsdsubj = [];
for i = 1:length(include)
   if strcmp(tb.group{include(i)},'CC')==1
       vccsubj = [vccsubj,tb.id(include(i))];
   end
  
   if strcmp(tb.group{include(i)},'PTSD')==1
       ptsdsubj = [ptsdsubj,tb.id(include(i))];
   end
   
   if strcmp(tb.group{include(i)},'FPTSD')==1
       fptsdsubj = [fptsdsubj,tb.id(include(i))];
   end
end

% row number of sujbects in the survey table
vccidx = [];
ptsdidx = [];
fptsdidx = [];
for i=1:length(survey.id)
   if ismember(survey.id(i),vccsubj) ==1
       vccidx = [vccidx,i];
   elseif ismember(survey.id(i),ptsdsubj)==1
       ptsdidx = [ptsdidx,i];
   elseif ismember(survey.id(i),fptsdsubj)==1
       fptsdidx = [fptsdidx,i];
   end
end


% Chose the score to plot
surveyNames = fieldnames(survey);
surveyNames = surveyNames(2:length(surveyNames)-3);

surveyName = 'dospert_rt_fina';

%% bar graph
% Risk- gain and loss
% ambig-gain and loss
% group 1: vcc, group2: ptsd, group3: fptsd

%risk graph:
plotmean = [nanmean(survey.(surveyName)(vccidx));nanmean(survey.(surveyName)(ptsdidx));nanmean(survey.(surveyName)(fptsdidx))];

plotsem = [nanstd(survey.(surveyName)(vccidx))/sqrt(length(survey.(surveyName)(ismember(survey.id, vccsubj) & ~isnan(survey.(surveyName)))));...
    nanstd(survey.(surveyName)(ptsdidx))/sqrt(length(survey.(surveyName)(ismember(survey.id, ptsdsubj) & ~isnan(survey.(surveyName)))));...
    nanstd(survey.(surveyName)(fptsdidx))/sqrt(length(survey.(surveyName)(ismember(survey.id, fptsdsubj) & ~isnan(survey.(surveyName)))))...
    ];


fig = figure
bplot = bar(plotmean);
hold on
errorbar(plotmean,plotsem,'.','Color',[0,0,0],'LineWidth',2);

% errorbar([1,2]-0.22,plotmean(:,1),plotsem(:,1),'.','Color',[0,0,0],'LineWidth',2);
% hold on
% errorbar([1,2],plotmean(2),plotsem(2),'.','Color',[0,0,0],'LineWidth',2);
% hold on
% errorbar([1,2]+0.22,plotmean(:,3),plotsem(:,3),'.','Color',[0,0,0],'LineWidth',2);

%bar color
bplot(1).FaceColor = [104,160,66]/255;
bplot(1).EdgeColor = [104,160,66]/255;
% bplot(2).FaceColor = [237,125,49]/255;
% bplot(2).EdgeColor = [237,125,49]/255;
% bplot(3).FaceColor = [165,165,165]/255;
% bplot(3).EdgeColor = [165,165,165]/255;

bplot(1).BarWidth = 0.9;


%axis property
ax = gca;
ax.XTickLabel = {'CC','PTSD', 'FPTSD'};
% ax.XTick = [1,2,3,4];
ax.Box = 'off';
ax.FontSize = 35;
ax.LineWidth =3;
% ax.YLabel.String = surveyName; 
ax.YLabel.FontSize = 35;
% ax.YLim = [-0.7,0.6];

title(surveyName)

leg = legend('VCC','PTSD','FPTSD');
leg.FontSize = 20;

%% correlation with behavior
% creat correlation table
x2plot = 'dospert_rt_fina';
y2plot = 'alpha_cstr_trf';
domain = ;

