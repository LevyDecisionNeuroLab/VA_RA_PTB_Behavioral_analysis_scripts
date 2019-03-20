clearvars

% read data
day1_file = 'D:\Ruonan\Projects in the lab\VA_RA_PTB\Analysis Ruonan\Fitpar files\Behavior data fitpar_020519\param_nonparam_day1_021019.txt';
day2_file = 'D:\Ruonan\Projects in the lab\VA_RA_PTB\Analysis Ruonan\Fitpar files\Behavior data fitpar_020519\param_nonparam_day2_021019.txt';
day1_old = readtable(day1_file);
day2_old  =readtable(day2_file);

day1_old.Properties.VariableNames{'Var1'} = 'id';
day2_old.Properties.VariableNames{'Var1'} = 'id';

% read subject grouping
subj_log_file = 'D:\Ruonan\Projects in the lab\VA_RA_PTB\Clinical and behavioral\log_allSubj.xlsx';
log = readtable(subj_log_file);

% join table
day1 = join(day1_old, log, 'Keys', 'id');
day2 = join(day2_old, log, 'Keys', 'id');

% calculate model free attitude
day1.G_risk = mean([day1.G_risk25, day1.G_risk50, day1.G_risk75], 2);
day1.G_amb = mean([day1.G_amb24, day1.G_amb50, day1.G_amb74], 2);
day1.G_amb_risk50 = day1.G_amb - day1.G_risk50;
day1.L_risk = mean([day1.L_risk25, day1.L_risk50, day1.L_risk75], 2);
day1.L_amb = mean([day1.L_amb24, day1.L_amb50, day1.L_amb74], 2);
day1.L_amb_risk50 = day1.L_amb - day1.L_risk50;

% transform model based attitude
day1.G_alpha = day1.alpha_2 - 1;
day1.G_beta = - day1.beta_2;
day1.L_alpha = 1 - day1.alpha_3;
day1.L_beta = day1.beta_3;

% same thing for day2
day2.G_risk = mean([day2.G_risk25, day2.G_risk50, day2.G_risk75], 2);
day2.G_amb = mean([day2.G_amb24, day2.G_amb50, day2.G_amb74], 2);
day2.G_amb_risk50 = day2.G_amb - day2.G_risk50;
day2.L_risk = mean([day2.L_risk25, day2.L_risk50, day2.L_risk75], 2);
day2.L_amb = mean([day2.L_amb24, day2.L_amb50, day2.L_amb74], 2);
day2.L_amb_risk50 = day2.L_amb - day2.L_risk50;

day2.G_alpha = day2.alpha_2 - 1;
day2.G_beta = - day2.beta_2;
day2.L_alpha = 1 - day2.alpha_3;
day2.L_beta = day2.beta_3;

writetable(day1, 'day1_par_nonpar.txt', 'FileType', 'text', 'Delimiter', '\t')
writetable(day2, 'day2_par_nonpar.txt', 'FileType', 'text', 'Delimiter', '\t')




















