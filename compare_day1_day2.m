%% Compare behavior day1 and day2
% Look at unconstrained and constrained fitting

%%
clearvars

%% load data
day1_file = 'D:\Ruonan\Projects in the lab\VA_RA_PTB\Analysis Ruonan\Fitpar files\Behavior data fitpar_020519\day1_par_nonpar.txt';
day2_file = 'D:\Ruonan\Projects in the lab\VA_RA_PTB\Analysis Ruonan\Fitpar files\Behavior data fitpar_020519\day2_par_nonpar.txt';

day1_old = readtable(day1_file);
day2_old = readtable(day2_file);

%% exclude participants
day1 = day1_old(day1_old.isExcluded_behavior == 0, :);
day2 = day2_old(day2_old.isExcluded_behavior == 0, :);

%% make sure that the ids are matched for two days
scatter(day1.id, day2.id)

%% compare unconstrain and constrained fitting

scatter(day1.alpha, day1.alpha_2)
scatter(day1.beta, day1.beta_2)

scatter(day1.alpha_1, day1.alpha_3)
scatter(day1.beta_1, day1.beta_3)

scatter(day2.alpha, day2.alpha_2)
scatter(day2.beta, day2.beta_2)

scatter(day2.alpha_1, day2.alpha_3)
scatter(day2.beta_1, day2.beta_3)


%% subjects with bad fittings

% subjects whose unconstrained parameter is out ot range
% day1
% risk gain
day1.id(day1.alpha > 4.34)
day1.id(day1.alpha < 0.0894)

% ambig gain
day1.id(day1.beta > 4)
day1.id(day1.beta < -3.67)

% risk loss
day1.id(day1.alpha_1 > 4.34)
day1.id(day1.alpha_1 < 0.0894)

% ambig loss
day1.id(day1.beta_1 > 4)
day1.id(day1.beta_1 < -3.67)

% day2
% risk gain
day2.id(day2.alpha > 4.34)
day2.id(day2.alpha < 0.0894)

% ambig gain
day2.id(day2.beta > 4)
day2.id(day2.beta < -3.67)

% risk loss
day2.id(day2.alpha_1 > 4.34)
day2.id(day2.alpha_1 < 0.0894)

% ambig loss
day2.id(day2.beta_1 > 4)
day2.id(day2.beta_1 < -3.67)

%% after excluding those with bad fittings
% day1
scatter(day1.alpha(day1.alpha < 4.34 & day1.alpha > 0.0894), day1.alpha_2(day1.alpha < 4.34 & day1.alpha > 0.0894))
scatter(day1.beta(day1.beta < 4 & day1.beta > -3.67), day1.beta_2(day1.beta < 4 & day1.beta > -3.67))
scatter(day1.alpha_1(day1.alpha_1 < 4.34 & day1.alpha_1 > 0.0894), day1.alpha_3(day1.alpha_1 < 4.34 & day1.alpha_1 > 0.0894))
scatter(day1.beta_1(day1.beta_1 < 4 & day1.beta_1 > -3.67), day1.beta_3(day1.beta_1 < 4 & day1.beta_1 > -3.67))

% day2
scatter(day2.alpha(day2.alpha < 4.34 & day2.alpha > 0.0894), day2.alpha_2(day2.alpha < 4.34 & day2.alpha > 0.0894))
scatter(day2.beta(day2.beta < 4 & day2.beta > -3.67), day2.beta_2(day2.beta < 4 & day2.beta > -3.67))
scatter(day2.alpha_1(day2.alpha_1 < 4.34 & day2.alpha_1 > 0.0894), day2.alpha_3(day2.alpha_1 < 4.34 & day2.alpha_1 > 0.0894))
scatter(day2.beta_1(day2.beta_1 < 4 & day2.beta_1 > -3.67), day2.beta_3(day2.beta_1 < 4 & day2.beta_1 > -3.67))

%% compare day 1 and day2 
% model based
scatter(day1.G_alpha, day2.G_alpha)
[r,p] = corr(day1.G_alpha, day2.G_alpha)

scatter(day1.G_beta, day2.G_beta)
[r,p] = corr(day1.G_beta, day2.G_beta)

scatter(day1.L_alpha, day2.L_alpha)
[r,p] = corr(day1.L_alpha, day2.L_alpha)

scatter(day1.L_beta, day2.L_beta)
[r,p] = corr(day1.L_beta, day2.L_beta)

% model free
scatter(day1.G_risk, day2.G_risk)
[r,p] = corr(day1.G_risk, day2.G_risk)

scatter(day1.G_amb_risk50, day2.G_amb_risk50)
[r,p] = corr(day1.G_amb_risk50, day2.G_amb_risk50)

scatter(day1.L_risk, day2.L_risk)
[r,p] = corr(day1.L_risk, day2.L_risk)

scatter(day1.L_amb_risk50, day2.L_amb_risk50)
[r,p] = corr(day1.L_amb_risk50, day2.L_amb_risk50)



