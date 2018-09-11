ALPHABET <- diag(26)
colnames(ALPHABET) <- LETTERS

tmp <- read.csv('C:/Users/mbmhscc4/GitHub/OrthPhon/jitter/jitternet_pilot_slots_CVC/jitternet_pilot_slots_CVC_500_16h_with_reversed_orth/prelim/run3/list_of_words_in_corpus__jitternet_pilot_slots_CVC_500_16h_with_reversed_orth.csv')
labs <- as.character(tmp$words_in_corpus)
words <- substr(labs,1,3)[seq(1,length(labs),by=7)]

word_to_letter <- matrix(0,ncol=length(words),nrow=3)
for (i in 1:length(words)) {
    w <- words[i]
    wl <- strsplit(w,split = '')[[1]]
    ix <- order(order(wl))
    for (j in length(wl)) {
        word_to_letter[j,i] <- match(wl[j], LETTERS)
    }
}

SLOTS_FIXED <- 3
ORTH <- matrix(
    0,
    ncol=length(words),
    nrow=dim(ALPHABET)[2]*SLOTS_FIXED)
for (i in 1:length(words)) {
    w2l <- word_to_letter[,i]
    ORTH[,i] <- as.vector(ALPHABET[,w2l])
}

SLOTS_JITTERED <- 9
ORTH_JITTERED <- matrix(
    0,
    ncol=length(words)*7,
    nrow=dim(ALPHABET)[2]*SLOTS_JITTERED)
ii <- 0
for (i in 1:length(words)) {
    for (j in 1:7) {
        ii <- ii + 1
        o <- ORTH[,i]
        cur <- (j - 1) * 26
        a <- cur + 1
        b <- cur + length(o)
        ORTH_JITTERED[a:b,ii] <- o
    }
}

ORTH_TARGETS <- matrix(
    0,
    ncol=length(words)*7,
    nrow=dim(ALPHABET)[2]*SLOTS_FIXED)
ii <- 0
for (i in 1:length(words)) {
    for (j in 1:7) {
        ii <- ii + 1
        o <- ORTH[,i]
        ORTH_TARGETS[,ii] <- o
    }
}

# Reversed
SLOTS_FIXED <- 3
ORTH_R <- matrix(
    0,
    ncol=length(words),
    nrow=dim(ALPHABET)[2]*SLOTS_FIXED)
for (i in rev(1:length(words))) {
    w2l <- word_to_letter[,i]
    ORTH[,i] <- as.vector(ALPHABET[,w2l])
}

SLOTS_JITTERED <- 9
ORTH_JITTERED_R <- matrix(
    0,
    ncol=length(words)*7,
    nrow=dim(ALPHABET)[2]*SLOTS_JITTERED)
ii <- 0
for (i in 1:length(words)) {
    for (j in 1:7) {
        ii <- ii + 1
        o <- ORTH[,i]
        cur <- (j - 1) * 26
        a <- cur + 1
        b <- cur + length(o)
        ORTH_JITTERED[a:b,ii] <- o
    }
}

ORTH_TARGETS_R <- matrix(
    0,
    ncol=length(words)*7,
    nrow=dim(ALPHABET)[2]*SLOTS_FIXED)
ii <- 0
for (i in 1:length(words)) {
    for (j in 1:7) {
        ii <- ii + 1
        o <- ORTH[,i]
        ORTH_TARGETS[,ii] <- o
    }
}

ORTH_TARGETS_ALL <- matrix(NA, nrow=dim(ORTH_TARGETS)[1], ncol=dim(ORTH_TARGETS)[2]*2)
ORTH_TARGETS_ALL[,seq(1,dim(ORTH_TARGETS_ALL)[2],by=2)] <- ORTH_TARGETS
ORTH_TARGETS_ALL[,seq(2,dim(ORTH_TARGETS_ALL)[2],by=2)] <- ORTH_TARGETS_R

ORTH_JITTERED_ALL <- matrix(NA,dim(ORTH_JITTERED)[1], dim(ORTH_JITTERED)[2]*2)
ORTH_JITTERED_ALL[,seq(1,dim(ORTH_JITTERED_ALL)[2],by=2)] <- ORTH_JITTERED
ORTH_JITTERED_ALL[,seq(2,dim(ORTH_JITTERED_ALL)[2],by=2)] <- ORTH_JITTERED_R

colnames(ORTH_TARGETS_ALL) <- as.vector(t(as.matrix(tmp[,c(2,3)])))
colnames(ORTH_JITTERED_ALL) <- as.vector(t(as.matrix(tmp[,c(2,3)])))
save(ORTH_TARGETS_ALL, ORTH_JITTERED_ALL, file = 'orthographic_input_output.Rdata')

load('orthographic_input_output.Rdata')

#
tmp <- read.csv('C:/Users/mbmhscc4/GitHub/OrthPhon/jitter/jitternet_pilot_slots_CVC/jitternet_pilot_slots_CVC_500_16h_with_reversed_orth/prelim/run2/')
