% combine all data sheets
% save into both .mat file and .xlsx file in the root folder

% log, par_nonpar, error_miss, clinical, survey

clearvars

root='D:\Ruonan\Projects in the lab\VA_RA_PTB\Clinical and behavioral';

% read data
logfilename = fullfile(root,'log_allSubj.xlsx');
errorfilename = fullfile(root, 'error miss_allSubj.xlsx');
attfilename = fullfile(root, 'par nonpar att_allSubj.xlsx');
% clinicalfilename = fullfile(root, 'quantitative clinical_allSubj.xlsx');
clinicalfilename = fullfile(root, 'all clinical_allSubj.xlsx');


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
writetable(tb,fullfile(root,'all data.xlsx'));

% save data table
save(fullfile(root, 'all data.mat'),'tb');
