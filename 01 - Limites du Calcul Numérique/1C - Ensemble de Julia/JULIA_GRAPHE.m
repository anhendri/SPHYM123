for INDX = 1:3
    FIGR = figure();
    MTRX = load(['OUTPUT_' num2str(INDX) '.dat']);
    if(INDX~=2)
        imagesc(MTRX(3:2+MTRX(1)), MTRX(3+MTRX(1):2+MTRX(1)+MTRX(2)),   ...
                reshape(MTRX(3+MTRX(1)+MTRX(2):end),MTRX(1),MTRX(2))')
    else
        plot(MTRX(:,1), MTRX(:,2), '.b');
    end
    xlabel('$\Re(z)$', 'Interpreter', 'LaTex', 'FontName',              ...
           'Times New Roman', 'FontSize', 16)
    ylabel('$\Im(z)$', 'Interpreter', 'LaTex', 'FontName',              ...
           'Times New Roman', 'FontSize', 16)
    hold on
    set(FIGR,'Units','Inches');
    PSTN = get(FIGR,'Position');
    set(FIGR,'PaperPositionMode', 'Auto', 'PaperUnits', 'Inches',       ...
             'PaperSize',PSTN(3:4))
    if(INDX~=2);colorbar;end
    set(gca, 'FontName', 'Times New Roman', 'FontSize', 16)
    set(gca, 'YDir', 'Normal')
    print(FIGR,['GRAPHE_' num2str(INDX)],'-dpdf','-r0')
end
close all