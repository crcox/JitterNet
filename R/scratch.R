library('dplyr')
library('reshape2')

load('JitterNet3k/2-layer/u25000.Rdata')

hidden1 <- dcast(
    data = subset(JitterNet3k_Orth, JitterNet3k_Orth$group == 'hidden1'),
    formula = word + jitter_id ~ unit,
    value.var = 'activation')

png("ShallowHidden_25000.png", height = 6, width = 6, units = "in", res = 150)
image(as.matrix(dist(hidden1[1:100,3:52])), axes = FALSE, main="Euclidean distance among Shallow Hidden representations\nfor first 100 examples")
axis(1, at = c(0,0.25,0.5,0.75,1), labels = c(1,25,50,75,100))
axis(2, at = c(0,0.25,0.5,0.75,1), labels = c(1,25,50,75,100))
dev.off()

hidden2 <- dcast(
    data = subset(JitterNet3k_Orth, JitterNet3k_Orth$group == 'hidden2'),
    formula = word + jitter_id ~ unit,
    value.var = 'activation')

png("DeepHidden_25000.png", height = 6, width = 6, units = "in", res = 150)
image(as.matrix(dist(hidden2[1:100,3:52])), axes = FALSE, main="Euclidean distance among Deep Hidden representations\nfor first 100 examples")
axis(1, at = c(0,0.25,0.5,0.75,1), labels = c(1,25,50,75,100))
axis(2, at = c(0,0.25,0.5,0.75,1), labels = c(1,25,50,75,100))
dev.off()

words <- unique(hidden2$word)
n <- length(words)
dist_among_jitters <- numeric(length = n)
for ( i in 1:n ) {
    w <- words[i]
    M <- subset(hidden2, hidden2$word == w)[, 3:52]
    dist_among_jitters[i] <- mean(dist(M))
}

words <- unique(hidden2$word)
n <- length(words)
dist_among_jitters <- numeric(length = n)
for ( i in 1:n ) {
    w <- words[i]
    M <- subset(hidden2, hidden2$word == w)[, 3:52]
    dist_among_jitters[i] <- mean(dist(M))
}

M <- as.matrix(dist(subset(hidden2[,3:52], hidden2$jitter_id==1)))
dist_among_words <- apply(M, 1, function(x) mean(sort(x)[1:10]))
diag(M) <- NA
dist_to_NN <- apply(M, 1, min, na.rm = TRUE)

smallest_value <- min(c(dist_among_jitters, dist_among_words))
largest_value <- max(c(dist_among_jitters, dist_among_words))

png(filename = "JitterDistance_25000.png", width = 6, height = 6, units = "in", res = 150)
plot(dist_among_jitters, ylim = c(smallest_value, largest_value), xlab = "word", ylab = "Euclidean distance")
points(dist_among_words, col = 'blue')
legend(x = "topright", legend = c("Mean over jitters", "Mean over 10 closest words with slot1 onset"), fill = c('black','blue'))
dev.off()

png(filename = "JitterDistance.png", width = 6, height = 6, units = "in", res = 150)
plot(dist_among_jitters, ylim = c(smallest_value, largest_value), xlab = "word", ylab = "Euclidean distance")
points(dist_to_NN, col = 'blue')
legend(x = "topright", legend = c("Mean over jitters", "To nearest neighbor (slot1 onset)"), fill = c('black','blue'))
dev.off()

png(filename = "JitterDistance_25000.png", width = 6, height = 6, units = "in", res = 150)
x <- dist_to_NN-dist_among_jitters
col <- (x < 0) + 1
plot(x, col=col, xlab = "word", ylab = "Euclidean distance", main = "Distance to nearest neighbor (slot1 onset)\nminus mean distance among jitters")
legend(x="topright", legend = c("NN is farther (2763)", "NN is closer (118)"), fill = c("black","red"))
dev.off()

tabulate(col)

# Load Weights
g <- c(286, 50, 50, 260)
names(g) <- c('input','hidden1','hidden2','output')
W <- loadWeights("~/src/JitterNet/JitterNet3k/2-layer/u25000.wt", g, bias = TRUE)
b <- matrix(1, nrow = dim(hidden1)[1], ncol = 1)
H1 <- cbind(b, as.matrix(hidden1[,3:52]))
H2 <- cbind(b, as.matrix(hidden2[,3:52]))

Y <- psych::logistic(H1 %*% W[['hidden1 -> hidden2']])

rbind( Y[100,1:50], H2[100,2:51] )

# Average over jitters in deep layer
DeepMean <- JitterNet3k_Orth %>%
    filter(group == 'hidden2') %>%
    group_by(word,unit) %>%
    summarize(activation = mean(activation)) %>%
    ungroup()

Targets <- JitterNet3k_Orth %>%
    filter(group == 'output', jitter_id == 1) %>%
    group_by(word,unit) %>%
    summarize(target = mean(target)) %>%
    dcast(word ~ unit, value.var = 'target') %>%
    ungroup()

hidden2_means <- dcast(
    data = DeepMean,
    formula = word ~ unit,
    value.var = 'activation')

b <- matrix(1, nrow = dim(hidden2_means)[1], ncol = 1)
H2mean <- cbind(b, as.matrix(hidden2_means[,2:51]))

Y <- psych::logistic(H2mean %*% W[['hidden2 -> output']])

TargMat <- as.matrix(Targets[,2:261])
mean(rowSums( abs(Y - TargMat) < 0.5 ) == 260)

dim(Y)
dim(TargMat)
