library('ggplot2')
XoT <- c( 651, 1229, 0, 815, 13, 466, 833, 157, 8, 115, 18 )
labels <- c(
    'Target (3v6) SVM only',
    'Source (22v9) SVM only',
    'Target and Source',
    'TCA+SVM only',
    'TCA+SVM and Target SVM',
    'TCA+SVM and Source SVM',
    'TCA+CDSVM only',
    'TCA+CDSVM and Target SVM',
    'TCA+CDSVM and Source SVM',
    'TCA+SVM and TCA+CDSVM',
    'TCA+SVM and TCA+CDSVM and Target SVM'
)
d <- data.frame(
    x = XoT,
    cond = as.factor(labels)
)

ggplot(d, aes(x=cond, y = x), fill = C('red','blue','purple','white','white','white','green','yellow','cyan','grey','black'), color = C('red','blue','purple','green','yellow','cyan','green','yellow','cyan','grey','black')) +
    geom_bar(stat = 'identity')

colors <- c('red','blue','purple','green','yellow','cyan','green','yellow','cyan','grey','black')
fills <- c('red','blue','purple','white','white','white','green','yellow','cyan','grey','black')
densities <- c(-1,-1,-1,25,25,25,-1,-1,-1,-1,-1)
barplot(XoT, col = fills, border = colors)
barplot(XoT[1:10], col = colors[1:10], density = densities[1:10])
