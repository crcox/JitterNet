nClosestToReference <- function(x, ref, n = (nrow(x) - 1)) {
    ix <- 1:nrow(x)
    ix <- ix[-ref]
    y <- x[ref, ]
    x <- x[-ref, ]
    d <- apply(x , 1, function(xi) 1-cor(as.vector(xi),y))
    ix <- ix[order(d)]
    return(ix[1:n])
}
