clearvars

root='D:\Ruonan\Projects in the lab\VA_RA_PTB\Clinical and behavioral';

%% combine data
logfilename = fullfile(root,'log_allSubj.xlsx');
errorfilename = fullfile(root, 'error miss_allSubj.xlsx');
attfilename = fullfile(root, 'par nonpar att_allSubj.xlsx');
% clinicalfilename = fullfile(root, 'quantitative clinical_allSubj.xlsx');
% clinicalfilename = fullfile(root, 'question items PCA 466.xlsx');
clinicalfilename = fullfile(root, 'question items PCA 158.xlsx');

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
writetable(tb,fullfile(root,'item_pca158_all data.xlsx'));

% save data table
save(fullfile(root, 'item_pca158_all data.mat'),'tb');

%% read data
load(fullfile(root,'item_pca158_all data.mat'));

%% PCA with items
% exclude subjects becaue of missing a complete questionnaire
% exclude = [3, 117, 50, 1285, 100, 102, 1232, 45, 78, 79, 82, 88, 76, 80, 85, 87, 95]; % for item pca 466
exclude = [117, 1285, 100, 102, 50, 3]; % for item pca 158

include = tb.isExcluded_behavior == 0 & tb.isGain == 1 & tb.isMale == 1 & ~ismember(tb.id,exclude);

tb2pca = tb(include,34:size(tb,2));

array2pca = nanzscore(table2array(tb2pca));

% PCA
[coeff1,score1,latent1,tsquared1,explained1,mu1] = pca(array2pca);
coeff = coeff1;
score = score1;
latent = latent1;
tsquared = tsquared1;
explained = explained1;
mu = mu1;

% % factor analysis
% m = 3; % how many factors
% [lambda,psi,T,stats,F] = factoran(array2pca,m);
% coeff = lambda;

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
% color from https://sashat.me/2017/01/11/list-of-20-simple-distinct-colors/

% 158 items
for PC = 1:5 % which component to plot
    loadings = coeff(:,PC);
    f = figure('units','normalized','outerposition',[0 0 1 1])
    hold on
    for i = 1:length(loadings)
        h=bar(i,loadings(i));
        if strcmp(names{i}(1:3),'ctq')          
            set(h,'FaceColor',[60 180 75]/255);         % green
            set(h,'EdgeColor',[60 180 75]/255);         % green   
            hc(2) = h;
        elseif strcmp(names{i}(1:3),'ces')          
            set(h,'FaceColor',[0 130 200]/255);        % blue
            set(h,'EdgeColor',[0 130 200]/255);        % blue            
            hc(3) = h;
        elseif strcmp(names{i}(1:3),'des')
            set(h,'FaceColor',[230 190 255]/255);       % lavender
            set(h,'EdgeColor',[230 190 255]/255);       % lavender            
            hc(7) = h;            
        elseif strcmp(names{i}(1:3),'bdi')
            set(h,'FaceColor',[255 255 25]/255);        % yellow
            set(h,'EdgeColor',[255 255 25]/255);        % yellow            
            hc(4) = h;
        elseif strcmp(names{i}(1:4),'caps') 
            set(h,'FaceColor',[230 25 75]/255);         %red
            set(h,'EdgeColor',[230 25 75]/255);         %red            
            hc(1) = h;
        elseif strcmp(names{i}(1:6),'staix1')
            set(h,'FaceColor',[210 245 60]/255);        % lime
            set(h,'EdgeColor',[210 245 60]/255);        % lime            
            hc(5) = h;
        elseif strcmp(names{i}(1:6),'staix2')
            set(h,'FaceColor',[128 128 0]/255);         % olive
            set(h,'EdgeColor',[128 128 0]/255);         % olive           
            hc(6) = h;           
        end
    end
    legendtxt = {'CAPS','CTQ','CES','BDI','STAI1','STAI2','DES'};
    leg = legend(hc,legendtxt, 'Location', 'northeastoutside');
    leg.FontSize = 10;
    title(['PC' num2str(PC)])
    ax = gca;
    ax.XLim = [0,length(loadings)];
    hold off
    saveas(f,fullfile(root,['PCA 158 items_PC' num2str(PC) ' loadings.bmp']))
end



% 466 items
for PC = 1:5 % which component to plot
    loadings = coeff(:,PC);
    f = figure('units','normalized','outerposition',[0 0 1 1])
    hold on
    for i = 1:length(loadings)
        h=bar(i,loadings(i));
        if strcmp(names{i}(1:3),'ctq')          
            set(h,'FaceColor',[60 180 75]/255);         % green
            set(h,'EdgeColor',[60 180 75]/255);         % green   
            hc(3) = h;
        elseif strcmp(names{i}(1:3),'ces')          
            set(h,'FaceColor',[0 130 200]/255);        % blue
            set(h,'EdgeColor',[0 130 200]/255);        % blue            
            hc(4) = h;
        elseif strcmp(names{i}(1:3),'des')
            set(h,'FaceColor',[230 190 255]/255);       % lavender
            set(h,'EdgeColor',[230 190 255]/255);       % lavender            
            hc(11) = h;            
        elseif strcmp(names{i}(1:3),'fss')
            set(h,'FaceColor',[250 190 190]/255);       %pink
            set(h,'EdgeColor',[250 190 190]/255);       %pink            
            hc(12) = h;  
        elseif strcmp(names{i}(1:3),'bdi')
            set(h,'FaceColor',[255 255 25]/255);        % yellow
            set(h,'EdgeColor',[255 255 25]/255);        % yellow            
            hc(6) = h;
        elseif strcmp(names{i}(1:3),'bai')
            set(h,'FaceColor',[0 0 128]/255);           % navy
            set(h,'EdgeColor',[0 0 128]/255);           % navy            
            hc(9) = h;            
        elseif strcmp(names{i}(1:4),'caps') 
            set(h,'FaceColor',[230 25 75]/255);         %red
            set(h,'EdgeColor',[230 25 75]/255);         %red            
            hc(1) = h;
        elseif strcmp(names{i}(1:4),'drri')
            set(h,'FaceColor',[70 240 240]/255);        %cyan
            set(h,'EdgeColor',[70 240 240]/255);        %cyan            
            hc(5) = h;
        elseif strcmp(names{i}(1:4),'ders')
            set(h,'FaceColor',[240 50 230]/255);        % magenta
            set(h,'EdgeColor',[240 50 230]/255);        % magenta            
            hc(10) = h;
        elseif strcmp(names{i}(1:4),'rses')
            set(h,'FaceColor',[170 110 40]/255);        %brown
            set(h,'EdgeColor',[170 110 40]/255);        %brown            
            hc(13) = h;
        elseif strcmp(names{i}(1:4),'rloc')
            set(h,'FaceColor',[145 30 180]/255);        % purple
            set(h,'EdgeColor',[145 30 180]/255);        % purple          
            hc(14) = h;
        elseif strcmp(names{i}(1:4),'bis_')
            set(h,'FaceColor',[0 128 128]/255);         % Teal
            set(h,'EdgeColor',[0 128 128]/255);         % Teal            
            hc(15) = h;     
        elseif strcmp(names{i}(1:4),'bas_')
            set(h,'FaceColor',[255 215 180]/255);       %coral
            set(h,'EdgeColor',[255 215 180]/255);       %coral            
            hc(16) = h;               
        elseif strcmp(names{i}(1:5),'bis11')
            set(h,'FaceColor',[170 255 195]/255);       % mint
            set(h,'EdgeColor',[170 255 195]/255);       % mint           
            hc(17) = h;            
        elseif strcmp(names{i}(1:5),'pcl_m')            
            set(h,'FaceColor',[245 130 48]/255);        %orange
            set(h,'EdgeColor',[245 130 48]/255);        %orange            
            hc(2) = h;
        elseif strcmp(names{i}(1:6),'staix1')
            set(h,'FaceColor',[210 245 60]/255);        % lime
            set(h,'EdgeColor',[210 245 60]/255);        % lime            
            hc(7) = h;
        elseif strcmp(names{i}(1:6),'staix2')
            set(h,'FaceColor',[128 128 0]/255);         % olive
            set(h,'EdgeColor',[128 128 0]/255);         % olive           
            hc(8) = h;           
        elseif strcmp(names{i}(1:10),'dospert_rt')      
            set(h,'FaceColor',[0 0 0]/255);       % black
            set(h,'EdgeColor',[0 0 0]/255);       % black            
            hc(18) = h;
        elseif strcmp(names{i}(1:10),'dospert_rp')
            set(h,'FaceColor',[128 128 128]/255);       % grey
            set(h,'EdgeColor',[128 128 128]/255);       % grey           
            hc(19) = h; 
        end
    end
    legendtxt = {'CAPS','PCLM','CTQ','CES','DRRI','BDI','STAI1','STAI2','BAI','DERS','DES','FSS','RSES','RLOCS','BIS','BAS','BIS11','DOSPERTrt','DOSPERTrp'};
    leg = legend(hc,legendtxt, 'Location', 'northeastoutside');
    leg.FontSize = 10;
    title(['PC' num2str(PC)])
    ax = gca;
    ax.XLim = [0,length(loadings)];
    hold off
    saveas(f,fullfile(root,['PCA466 items_PC' num2str(PC) ' loadings.bmp']))
end
