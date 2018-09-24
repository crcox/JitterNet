
x <- attr(regexpr('^_+', phoncode$code), 'match.length')
x <- ifelse(x==-1, 0, x) + 1
summary(x)

M <- matrix(0, ncol = 50, nrow = 2881)
dd <- hidden2[1:2881,]
dd$word <- words
dd$jitter_id <- x
for (i in 1:length(words)) {
    w <- as.character(words[i])
    j <- x[phoncode$word == w]
    print(c(w,j))
    dd[i,3:52] <- subset(hidden2, hidden2$word == w)[j,3:52]
}
