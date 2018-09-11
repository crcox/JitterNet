# Summarize unit usage in output layer
printSparseOutputBySlot <- function(x, onset) {
    n <- length(x);
    x <- x + (seq(onset - 1, onset + (n - 2)) * 26)
    cat("t: {output}", x, sep=' ')
    cat('\n',';','\n',sep='')
}
letters2numbers <- function(x) {
    vapply(
           toupper(x),
           FUN.VALUE=numeric(1),
           FUN=function(y) {match(y,LETTERS)}
    )
}
summarize_output <- function(d) {
    require('dplyr')
    SlotsRequired <- d %>%
        group_by(direction) %>%
        summarize(n = max(nchar(word) + target_onset - 1))
    n <- subset(SlotsRequired$n,SlotsRequired$direction == 'forward')
    cat('Slots Required:', n, '\n')
    labels <- expand.grid(letter=LETTERS, slot=seq(n))
    tmp <- subset(d[,c('word','target_onset')], d$direction == 'forward')
    z <- !duplicated(tmp$word)
    words <- tmp$word[z]
    onset <- tmp$target_onset[z]
    letter_encoding <- sapply(strsplit(words, split = ''), letters2numbers)
    unit_offset <- mapply(function(x,y) (x + (seq(y)-1)) * 26, onset-1, nchar(words))
    letter_encoding_centered <- mapply(`+`, letter_encoding, unit_offset)
    x <- tabulate(do.call(c, letter_encoding_centered))
    z <- rep(F, n * 26)
    m <- length(x)
    z[1:m] <- x > 0
    Freq <- labels[z,]
    Freq$n <- x[x>0]
    return(Freq)
}

summarize_input <- function(d) {
    require('dplyr')
    SlotsRequired <- d %>%
        group_by(direction) %>%
        summarize(n = max(nchar(word) + input_onset - 1))
    n <- subset(SlotsRequired$n,SlotsRequired$direction == 'forward')
    cat('Slots Required:', n, '\n')
    labels <- expand.grid(letter=LETTERS, slot=seq(n))
    tmp <- subset(d[,c('word','input_onset')], d$direction == 'forward')
    words <- tmp$word
    onset <- tmp$input_onset
    letter_encoding <- sapply(strsplit(words, split = ''), letters2numbers)
    unit_offset <- mapply(function(x,y) (x + (seq(y)-1)) * 26, onset-1, nchar(words))
    letter_encoding_centered <- mapply(`+`, letter_encoding, unit_offset)
    x <- tabulate(do.call(c, letter_encoding_centered))
    z <- rep(F, n * 26)
    m <- length(x)
    z[1:m] <- x > 0
    Freq <- labels[z,]
    Freq$n <- x[x>0]
    return(Freq)
}
