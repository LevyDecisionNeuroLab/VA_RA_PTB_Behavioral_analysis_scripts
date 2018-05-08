% This scripts create COV files of CAPS and AlphaBeta from empty COV files
% created from BV on the first hand

% IMPORTANT NOTICE:
% .cov does not contain subject ID, so need to make sure the list of
% subject matches between excel and BV, because BV use a 'String' fashion
% to sort ID

clearvars

%% import the .txt file with all subjects' covariates in the right sequence as in BV
covFile = 'D:\Ruonan\Projects in the lab\VA_RA_PTB\Clinical and behavioral\covariates_042418.txt';

covTable = readtable(covFile);

%% all covariates
cov = xff('COV_67subj_empty.cov');

covVal = cov.Covariates;

covVal = table2array(covTable(:,4:17));
cov.Covariates(1:67,1:14) = covVal(1:67,1:14);

cov.SaveAs('COV_67subj.cov');


%% caps
caps = xff('COV_67subj_empty.cov');

% nrOfSubj = caps.NrOfSubjectRows;
% nrOfCov = caps.NrOfCovariateColumns;
covCaps = caps.Covariates;
% runTimeVars = caps.RunTimeVars;


covCaps = table2array(capsTable(:,4:9));
caps.Covariates(1:69,1:6) = covCaps(1:69,1:6);

caps.SaveAs('COV_67subj.cov');

%% alpha and beta

ab = xff('alphaBeta_69subj_empty.cov');

covAb = ab.Covariates;
covAb = table2array(capsTable(:,10:13));

ab.Covariates(1:69,1:4) = covAb(1:69,1:4);

ab.SaveAs('alphaBeta_69subj.cov');


