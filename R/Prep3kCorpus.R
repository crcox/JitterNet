# Prep the 3k corpus
# ==================
# Load the processed 3k Dict prepared by MCB.
# Reformat the direction and homophone variables
# Select on the the forward-direction examples.
# Exclude words that duplicate the orthography of words with lower word ids.
source('writeExampleFile.r')
load("~/src/JitteredOrthography/3kdict_jitternet_training_corpus.rda")

d <- jitternet_training_corpus
d$direction <- factor(
    d$direction,
    levels = c('forward','reverse_recentered','reverse_mirror'),
    labels = c('forward','reversed','mirrored'))
d$homophone <- d$homophone == 'homophone'
d$phon_id <- as.numeric(factor(d$phon_id))

e <- subset(d, d$direction == 'forward')

# When there are several words with the same orthography, retain only the word
# associated with the lowest word id.
oid <- unique(e$orth_id)
for (i in seq(length(oid))) {
    o <- oid[i]
    w <- unique(subset(e$word_id, e$orth_id == o))
    n <- length(w)
    if (n > 1) {
        e <- subset(e, !(e$word_id %in% w[2:n]) )
    }
}

# Define the LENS Example file header
# ==================================
# defI: the default unit value for inputs
# actI: the default activation value for inputs
# defT: the default unit value for targets
# actT: the default activation value for targets
h <- list(
    defI = 0,
    actI = 1,
    defT = 0,
    actT = 1
)
writeJitterNetExampleFile(
    filename = 'train.ex',
    sampleDF = e,
    printFrequency = FALSE,
    headerList = h
)
