library('stringr')

matchFirstV <- function(x) {
    match('V', x)
}
collapseCharacters <- function(x) {
    paste(x, collapse='')
}
generateCentered <- function(corpus, variableToCenter) {
    require(stringr)
    char_vectors <- stringr::str_split(corpus[[variableToCenter]], pattern = '', simplify = T)
    m <- max(corpus$terminal_slot_after_shift)
    tmp <- matrix('_',nrow = nrow(corpus), ncol = m)
    for (i in 1:nrow(tmp)) {
        initial_slot <- corpus$orth_shift[i] + 1
        terminal_slot <- corpus$terminal_slot_after_shift[i]
        ix <- initial_slot:terminal_slot
        n <- corpus$word_lengths[i]
        tmp[i,ix] <- char_vectors[i,1:n]
    }
    return(apply(tmp,1,collapseCharacters))
}

load('jitternet_corpus.rda')

cv_vectors <- stringr::str_split(jitternet_corpus$syl_pats_orth, pattern = '', simplify = T)

jitternet_corpus$word_lengths <- nchar(jitternet_corpus$orth)
jitternet_corpus$vowel_onsets <- apply(cv_vectors,1,matchFirstV)
max_vowel_onset <- max(jitternet_corpus$vowel_onsets)
jitternet_corpus$orth_shift <- max_vowel_onset - jitternet_corpus$vowel_onsets
jitternet_corpus$terminal_slot_after_shift <- jitternet_corpus$word_lengths + jitternet_corpus$orth_shift
summary(jitternet_corpus)

jitternet_corpus$syl_pats_orth_centered_crc <- generateCentered(jitternet_corpus, 'syl_pats_orth')
jitternet_corpus$orth_centered_crc <- generateCentered(jitternet_corpus, 'orth')
head(jitternet_corpus)
