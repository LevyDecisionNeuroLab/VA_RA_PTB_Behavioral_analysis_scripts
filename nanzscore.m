% standardize each column of the matrix ignoring NaN values
function zscoremat = nanzscore(mat)

% for testing the function
% mat = [1 2 3; 4 5 NaN; 3 4 5; NaN 9 3]

colmean = nanmean(mat);
colstd = nanstd(mat);
% colcount = sum(~isnan(mat),1);

% compute z-score
zscoremat = (mat-repmat(colmean,size(mat,1),1)) ./ repmat(colstd,size(mat,1),1);

