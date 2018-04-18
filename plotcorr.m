% a function of scatter plot and fitting lines
function plotcorr(x,y,xname,yname)

    figure    
    scatter(x,y, 'filled');
    hold on
    % linear regression
    mdl1 = LinearModel.fit(x,y); % creates a linear model of the responses y to a tb matrix x
    coeff = table2array(mdl1.Coefficients);
    linex = linspace(0,max(x)+5);
    liney = coeff(2,1)*linex+coeff(1,1);
    plot(linex, liney, 'color','k');
    
    % print text of r2 and p value
    txt1 = ['R^{2} = ',num2str(round(mdl1.Rsquared.Ordinary,2,'significant'))];
    txt2 = ['p = ', num2str(round(coeff(2,4),2,'significant'))];
    xlab = xlim;
    ylab = ylim;
    %dim = [0.8 0.8 0.3 0.1]; %[x y w h]
    txt = {txt1;txt2};
%     if i==1|i==2
%         text(xlab(2)-(xlab(2)-xlab(1))/4, ylab(2)+0.2, txt, 'FontSize',8)
%     else
%     text(xlab(2)-(xlab(2)-xlab(1))/4, ylab(2), txt, 'FontSize',8)
    text(xlab(2), ylab(2), txt, 'FontSize',6)

%     end

    %text(xlab(2)-(xlab(2)-xlab(1))/4, ylab(2)+0.07, txt1, 'FontSize',8)
    %text(xlab(2)-(xlab(2)-xlab(1))/4, ylab(2)+0.02,txt2,'FontSize',8)
    
    %annotation('textbox',dim,'string',txt,'FitBoxToText','on');
    xlabel(xname) % title of subplot
    ylabel(yname)
                
