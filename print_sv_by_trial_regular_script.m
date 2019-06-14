clearvars
close all

%% data directory
cd 'D:\Ruonan\Projects in the lab\VA_RA_PTB\Clinical and behavioral'
% add path of function 'ambig_utility.m'
addpath 'D:\Ruonan\Projects in the lab\VA_RA_PTB\Analysis Ruonan\VA_PTB_Analysis-Scripts\Model fitting script'
% read data contains the fitted parameters, fitting both days together
all_data = readtable('all data_09152018.xlsx');
% read data contains the fitted parameters, fitting day1 day2 separately
all_data_all = readtable('par nonpar att_allSubj_day1day2_04082019.csv');


% output file name
% sv_loss_file = 'sv by trial loss.xls';
% sv_gain_file = 'sv by trial gain.xls';

sv_trial_file = 'sv_trial_day1day2.xls';

%% set up trial types
% values
valueP = [4 5 6 7 8 10 12 14 16 19 23 27 31 37 44 52 61 73 86 101 120];
valueN = [-4 -5 -6 -7 -8 -10 -12 -14 -16 -19 -23 -27 -31 -37 -44 -52 -61 -73 -86 -101 -120];
value = repmat(valueP,6,1);
% six risk and ambig levels
prob = [0.25; 0.5; 0.75; 0.5; 0.5; 0.5];
ambig = [0; 0; 0; 0.24; 0.5; 0.74];


%% day1 day2 separate

for isDay1 = 0:1
    
    all_data = all_data_all(table2array(all_data_all(:,3)) == isDay1, :); 
    
    %% calculate overal choice proportion
    choice_prob = mean([all_data.r25, all_data.r50, all_data.r75, all_data.a24, all_data.a50, all_data.a74],2); 

    %% get subject ids
    subject = all_data(all_data.isGain == 1,:).id;

    %% Loop through subjects to calculate subjective values
%     % plot specification
%     ssize = get(0,'ScreenSize');
%     h = ssize(4); % screen height
%     w = ssize(3); % screen width
%     scaler = 2/3; % the size of the image relative to screen size, will be displayed in the middle of the screen
    
    
    % loop for each subject
    for idx = 1:length(subject)
     
%         f = figure('Position',[(w-w*scaler)/2, (h-h*scaler)/2, w*scaler, h*scaler]);
        
        for gainloss = 0:1 % 1 = gain, 0 = loss
                    
           alpha = all_data.alpha(all_data.isGain == gainloss & all_data.id == subject(idx));
           beta = all_data.beta(all_data.isGain == gainloss & all_data.id == subject(idx));

           sv = ambig_utility(0, value, prob, ambig, alpha, beta, 'ambigNrisk');
           sv_ref = ambig_utility(0, 5, 1, 0, alpha, beta, 'ambigNrisk');

           if gainloss == 0
               % flip sign of negative subjective values
               sv = -sv;
               sv_ref = -sv_ref;
           end
        
           for uncertainty_idx = 1:length(prob)
               for value_idx = 1:length(valueP)
                   
                   % empty vector to represent a single trial's SV
                   trial_sv = zeros(1, 8);
           
                   trial_sv(1) = subject(idx);
                   trial_sv(2) = isDay1;
                   trial_sv(3) = gainloss;
                   trial_sv(4) = prob(uncertainty_idx);
                   trial_sv(5) = ambig(uncertainty_idx);
                   trial_sv(6) = valueP(value_idx);
                   trial_sv(7) = sv(uncertainty_idx, value_idx);
                   trial_sv(8) = sv_ref;
                                     
                   % write into file
                   dlmwrite(sv_trial_file, trial_sv, '-append', 'roffset', 0, 'delimiter', '\t')
                   
               end
           end
           
%            % plot heatmaps of subjective values 
%            if gainloss == 0
%                subplot(2,1,1)
%                map_loss = heatmap(sv, 'XLabel', 'Reward level',...
%                    'XData', valueN,...
%                    'YLabel', 'Uncertainty level',...
%                    'YData', ['r25'; 'r50'; 'r75'; 'a24'; 'a50'; 'a74'] ,...
%                    'Title', ['Subject ' num2str(subject(idx)) ' Loss, SVRef = ' num2str(sv_ref) '; alpha = ' num2str(alpha) ', beta = ' num2str(beta)]);
%            else
%                 subplot(2,1,2)
%                 map_gain = heatmap(sv, 'XLabel', 'Reward level',...
%                     'XData',valueP,...
%                     'YLabel', 'Uncertainty level',...
%                     'YData', ['r25'; 'r50'; 'r75'; 'a24'; 'a50'; 'a74'] ,...                
%                     'Title', ['Subject ' num2str(subject(idx)) ' Gain, SVRef = ' num2str(sv_ref) '; alpha = ' num2str(alpha) ', beta = ' num2str(beta)]);
%            end 
% 
         end
    end
    
end

% 
% %% calculate overal choice proportion
% choice_prob = mean([all_data.r25, all_data.r50, all_data.r75, all_data.a24, all_data.a50, all_data.a74],2); 
% 
% %% get subject ids
% subject = all_data(all_data.isGain == 1,:).id;
% % subject = [3]; % test for on subject
% 
% %%
% % plot specification
% ssize = get(0,'ScreenSize');
% h = ssize(4); % screen height
% w = ssize(3); % screen width
% scaler = 2/3; % the size of the image relative to screen size, will be displayed in the middle of the screen
% 
% % loop for each subject
% for idx = 1:length(subject)
%     f = figure('Position',[(w-w*scaler)/2, (h-h*scaler)/2, w*scaler, h*scaler]);
%     for gainloss = 0:1 % 1 = gain, 0 = loss
%        alpha = all_data.alpha(all_data.isGain == gainloss & all_data.id == subject(idx));
%        beta = all_data.beta(all_data.isGain == gainloss & all_data.id == subject(idx));
%        % gamma = all_data.gamma(all_data.isGain == gainloss & all_data.id == subject(idx));
%           
%        sv = ambig_utility(0, value, prob, ambig, alpha, beta, 'ambigNrisk');
%        sv_ref = ambig_utility(0, 5, 1, 0, alpha, beta, 'ambigNrisk');
%        
%        if gainloss == 0
%            sv = -sv;
%            sv_ref = -sv_ref;
%        end
%        
% 
% %        title(['Subject ' num2str(subject(idx))])
%        % print to file
%        if gainloss == 0
%            
% %            heatmap_loss = heatmap(zscore(sv,0,'all'))
%            subplot(2,1,1)
%            map_loss = heatmap(sv, 'XLabel', 'Reward level',...
%                'XData', valueN,...
%                'YLabel', 'Uncertainty level',...
%                'YData', ['r25'; 'r50'; 'r75'; 'a24'; 'a50'; 'a74'] ,...
%                'Title', ['Subject ' num2str(subject(idx)) ' Loss, SVRef = ' num2str(sv_ref) '; alpha = ' num2str(alpha) ', beta = ' num2str(beta)]);
%            
%            dlmwrite(sv_loss_file, subject(idx), '-append', 'roffset', 1, 'delimiter', ' ')
%            dlmwrite(sv_loss_file, sv_ref, '-append', 'coffset', 1, 'delimiter', '\t')
%            dlmwrite(sv_loss_file, sv, 'coffset', 1, '-append', 'delimiter', '\t')
%        else
%             subplot(2,1,2)
%             map_gain = heatmap(sv, 'XLabel', 'Reward level',...
%                 'XData',valueP,...
%                 'YLabel', 'Uncertainty level',...
%                 'YData', ['r25'; 'r50'; 'r75'; 'a24'; 'a50'; 'a74'] ,...                
%                 'Title', ['Subject ' num2str(subject(idx)) ' Gain, SVRef = ' num2str(sv_ref) '; alpha = ' num2str(alpha) ', beta = ' num2str(beta)]);
% 
%            
%            dlmwrite(sv_gain_file, subject(idx), '-append', 'roffset', 1, 'delimiter', ' ')
%            dlmwrite(sv_gain_file, sv_ref, '-append', 'coffset', 1, 'delimiter', '\t')
%            dlmwrite(sv_gain_file, sv, 'coffset', 1, '-append', 'delimiter', '\t')
%        end 
%        
%     end
%    
%     % save figure
%     saveas(f, ['Figures sv by trial by subject\Subject' num2str(subject(idx))]); 
% end