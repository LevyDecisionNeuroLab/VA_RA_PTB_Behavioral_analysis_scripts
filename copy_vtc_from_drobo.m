clearvars

dataroot = 'Z:\Levy_Lab\Projects\R&A PTSD Imaging\Data\Scans\Multiband';
vtcpath_out = 'D:\Ruonan\Projects in the lab\VA_RA_PTB\Imaging analysis\Imaging_analysis_091117\BV_VTCs';
fitparpath = 'D:\Ruonan\Projects in the lab\VA_RA_PTB\Analysis Ruonan\Fitpar files\Behavior data fitpar_091017';
exclude = [3 8 76 78 79 80 81 101 102 104 117 1210 1220 1234 1250 1272 1316 1326 1337];

% get subject number
subj_files = dir([fitparpath, filesep, 'RA_GAINS*fitpar.mat']);
SubjectNums = zeros(1, length(subj_files));

% Extract subject from filename
for file_idx = 1:length(subj_files)
  fname = subj_files(file_idx).name;
  matches = regexp(fname, 'RA_(?<domain>GAINS|LOSS)_(?<subjectNum>[\d]{1,4})', 'names');
  SubjectNums(file_idx) = str2num(matches.subjectNum); 
end

% SubjectNums = [75 82 83 1328];

SubjectNums = SubjectNums(~ismember(SubjectNums, exclude));

%% copy vtcs
% for i = 1:length(SubjectNums)
%     subject = SubjectNums(i);
%     path_in = fullfile(dataroot, ['subj' num2str(subject)]);
%     cd(path_in)
%     vtc_files = dir('*\*SD3DVSS8.00mm.vtc');
%     for j = 1:length(vtc_files)
%         copyfile(fullfile(vtc_files(j).folder, vtc_files(j).name), vtcpath_out)
%         disp(['Copied ' vtc_files(j).name])
%     end
% end

%% copy vmrs
vmrpath_out = 'D:\Ruonan\Projects in the lab\VA_RA_PTB\Imaging analysis\Imaging_analysis_091117\VMRs';

for i = 1:length(SubjectNums)
    subject = SubjectNums(i);
    path_in = fullfile(dataroot, ['subj' num2str(subject)]);
    cd(path_in)
    vmr_files = dir('*\*_TAL.vmr');
    for j = 1:length(vmr_files)
        copyfile(fullfile(vmr_files(j).folder, vmr_files(j).name), vmrpath_out)
        disp(['Copied ' vmr_files(j).name])
    end
end