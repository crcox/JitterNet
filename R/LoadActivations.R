library("data.table")
source("~/src/lensr/lensR.R")
tmp <- loadActivations("JitterNet3k/2-layer/u25000.out.bin", format = 'binary')
labels <- read.table(file = "JitterNet3k/2-layer/example-labels.txt", stringsAsFactors = FALSE)
labels$example <- 0:(nrow(labels)-1)
names(labels)[1] <- 'label'

tmp <- merge(x = tmp, y = labels, by = "example", all.x = TRUE)
tmp$word <- as.factor(sapply(strsplit(tmp$label, split='_'), function(x) {x[1]}))
tmp$jitter_id <- as.numeric(sapply(strsplit(tmp$label, split='_'), function(x) {x[2]}))
tmp$group <- factor(tmp$group, levels = 1:3, labels = c('hidden1','hidden2','output'))

str(tmp)
JitterNet3k_Orth <- tmp
save(JitterNet3k_Orth, file = 'JitterNet3k/2-layer/u25000.Rdata')
