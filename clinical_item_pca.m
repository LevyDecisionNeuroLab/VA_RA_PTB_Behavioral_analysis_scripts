clearvars

root='D:\Ruonan\Projects in the lab\VA_RA_PTB\Clinical and behavioral';

%% combine data
logfilename = fullfile(root,'log_allSubj.xlsx');
errorfilename = fullfile(root, 'error miss_allSubj.xlsx');
attfilename = fullfile(root, 'par nonpar att_allSubj.xlsx');
% clinicalfilename = fullfile(root, 'quantitative clinical_allSubj.xlsx');
clinicalfilename = fullfile(root, 'question items PCA.xlsx');


tblog = readtable(logfilename);
tberror = readtable(errorfilename);
tbatt = readtable(attfilename);
tbclinical = readtable(clinicalfilename);

% subject 61 does not have data in the clinical sheet, for convenience of
% combing table, add an empty row for 61 in the .xlsx file


% calculate transformed alpha and beta
tbatt.alpha_t = tbatt.alpha - 1;
tbatt.alpha_t(tbatt.isGain ~= 1,:) = - tbatt.alpha_t(tbatt.isGain ~= 1,:);
tbatt.beta_t = -tbatt.beta;
tbatt.beta_t(tbatt.isGain ~= 1,:) = -tbatt.beta_t(tbatt.isGain ~= 1,:);
% calculate model free ambig att
tbatt.a24_r50 = tbatt.a24-tbatt.r50;
tbatt.a50_r50 = tbatt.a50-tbatt.r50;
tbatt.a74_r50 = tbatt.a74-tbatt.r50;

% caculate model free scores
tbatt.r = nanmean([tbatt.r25, tbatt.r50, tbatt.r75],2);
tbatt.a_r50 = nanmean([tbatt.a24_r50, tbatt.a50_r50, tbatt.a74_r50],2);

% combine error and att sheets
tb1 = join(tbatt, tberror, 'Keys', {'id', 'isGain'});

% combine behavioral and log
tb2 = join(tb1, tblog, 'Keys', {'id'});
subjects = unique(tb2.id);

% combine behavioral log, and clinical
tb = join(tb2, tbclinical(ismember(tbclinical.id, subjects),:), 'Keys', {'id'});

% write the full data table into xlsx file in the root folder
writetable(tb,fullfile(root,'item_pca_all data.xlsx'));

% save data table
save(fullfile(root, 'item_pca_all data.mat'),'tb');

%% read data
load(fullfile(root,'item_pca_all data.mat'));

%% PCA with items
% exclude subjects becaue of missing a complete questionnaire
exclude = [3, 117, 50, 1285, 100, 102];
include = tb.isExcluded_behavior == 0 & tb.isGain == 1 & tb.isMale == 1 & ~ismember(tb.id,exclude);

tb2pca = tb(include,34:282);

array2pca = nanzscore(table2array(tb2pca));

% PCA
[coeff1,score1,latent1,tsquared1,explained1,mu1] = pca(array2pca);
coeff = coeff1;
score = score1;
latent = latent1;
tsquared = tsquared1;
explained = explained1;
mu = mu1;

% factor analysis
m = 3; % how many factors
[lambda,psi,T,stats,F] = factoran(array2pca,m);
coeff = lambda;

% how many missigng data excluded
sum(isnan(score(:,1)))

% predictor names
names = tb2pca.Properties.VariableNames';

% plot variance explained
figure
plot(1:size(explained),cumsum(explained),'-bo');
xlabel('Number of PCA components');
ylabel('Percent Variance Explained');
ax = gca;
ax.XLim = [0,length(explained)];
% ax.YLim = [0,105];

% plot loadings for components
ncomp = 3;
screensize = get( groot, 'Screensize' );
figure('Position', [0.1*screensize(3) 0.07*screensize(4) 0.8*screensize(3) 0.7*screensize(4)])
bar(1:size(coeff,1),coeff(:,1:ncomp))
ax = gca;
ax.XTick = [1:1:size(coeff,1)];
xlabel('Predictors', 'FontSize', 18);
ylabel('Loadings', 'FontSize', 18);
ax.XTickLabels = tb2pca.Properties.VariableNames;
ax.FontSize = 18;
legendtxt = {};
for i=1:ncomp    
    legendtxt = [legendtxt, ['Component ' num2str(i)]];
end
leg = legend(legendtxt);
leg.FontSize = 10;

% plot loadings with color differentiate questionnaire
for PC = 1:5 % which component to plot
    loadings = coeff(:,PC);
    figure
    hold on
    for i = 1:length(loadings)
        h=bar(i,loadings(i));
        if strcmp(names{i}(1:4),'caps') 
            set(h,'FaceColor',[230 25 75]/255);
            hc(1) = h;
        elseif strcmp(names{i}(1:5),'pcl_m')
            set(h,'FaceColor',[245 130 48]/255);
            hc(2) = h;
        elseif strcmp(names{i}(1:3),'ctq')
            set(h,'FaceColor',[60 180 75]/255);
            hc(3) = h;
        elseif strcmp(names{i}(1:3),'ces')
            set(h,'FaceColor',[0 130 200]/255);
            hc(4) = h;
        elseif strcmp(names{i}(1:4),'drri')
            set(h,'FaceColor',[70 240 240]/255);
            hc(5) = h;
        elseif strcmp(names{i}(1:3),'bdi')
            set(h,'FaceColor',[255 255 25]/255);
            hc(6) = h;
        elseif strcmp(names{i}(1:3),'bai')
            set(h,'FaceColor',[0 0 128]/255);
            hc(9) = h;
        elseif strcmp(names{i}(1:4),'ders')
            set(h,'FaceColor',[240 50 230]/255);
            hc(10) = h;
        elseif strcmp(names{i}(1:3),'des')
            set(h,'FaceColor',[230 190 255]/255);
            hc(11) = h;
        elseif strcmp(names{i}(1:6),'staix1')
            set(h,'FaceColor',[210 245 60]/255);
            hc(7) = h;
        elseif strcmp(names{i}(1:6),'staix2')
            set(h,'FaceColor',[128 128 0]/255);
            hc(8) = h;
        end
    end
    legendtxt = {'CAPS','PCLM','CTQ','CES','DRRI','BDI','STAI1','STAI2','BAI','DERS','DES'};
    leg = legend(hc,legendtxt, 'Location', 'northeastoutside');
    leg.FontSize = 10;
    title(['PC' num2str(PC)])
    hold off
end
