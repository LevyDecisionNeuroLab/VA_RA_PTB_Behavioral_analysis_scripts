% plot scatter plot matrix

function plotcorrmat2(tb2plot1, tb2plot2)

screensize = get( groot, 'Screensize' );

figure('Position', [0.35*(screensize(3)-0.85*screensize(4)) 0.07*screensize(4) 0.85*screensize(4) 0.85*screensize(4)])
[S,AX,BigAx,H,HAx] = plotmatrix(table2array(tb2plot1), table2array(tb2plot2),'.k');
% [S,AX,BigAx,H,HAx] = plotmatrix(table2array(tb2plot));

BigAx.Visible = 'on';
BigAx.XAxisLocation = 'top';
BigAx.XLim = [0 1];
BigAx.XTick = [0.5/size(table2array(tb2plot1),2):1/size(table2array(tb2plot1),2):1-0.5/size(table2array(tb2plot1),2)];
BigAx.XTickLabel = tb2plot1.Properties.VariableNames;
BigAx.YAxisLocation = 'right';
BigAx.YTickLabelRotation = -90;
BigAx.YLim = [0 1];
BigAx.YTick = [0.5/size(table2array(tb2plot2),2):1/size(table2array(tb2plot2),2):1-0.5/size(table2array(tb2plot2),2)];
BigAx.YTickLabel = fliplr(tb2plot2.Properties.VariableNames);

% correlation coefficient and p value
[corrmat, pmat] = corrcoef([table2array(tb2plot1),table2array(tb2plot2)],'rows','complete');

cmap = colormap(parula);
cmap = cmap(length(cmap)/2+1:length(cmap),:);
% cmap = colormap(summer);

% set background color depending on correlation coefficient
for i = 1:size(tb2plot2,2)
    for j = 1:size(tb2plot1,2)
        set(AX(i,j), 'color', cmap(ceil(abs(corrmat(j,size(tb2plot1,2)+i))*length(cmap)),:));
        txt1 = ['r=', num2str(round(corrmat(j,size(tb2plot1,2)+i),2))];
        txt2 = ['p=', num2str(round(pmat(j,size(tb2plot1,2)+i),2,'significant'))];
        txt = {txt1;txt2};
        xlab = AX(i,j).XLim;
        ylab = AX(i,j).YLim;
        text(AX(i,j), xlab(2)-(xlab(2)-xlab(1))/3, ylab(2)-(ylab(2)-ylab(1))/10, txt, 'FontSize',8)
    end
end

