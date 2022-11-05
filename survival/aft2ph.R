aft2ph <- function(wf) {
    if(class(wf) != "survreg" | wf$dist != "weibull") {
      stop("Argument must be a survreg Weibull fit")
    }
    # transform coefficients
    p <- 1/wf$scale
    g <- -coef(wf)*p
    k <-length(g)
    value <- c(g, log(p))
    
    # use delta method for standard errors
    D <- matrix(0, k+1, k+1)
    D[row(D) == col(D)] <- -p
    D[, k+1] <- -c(g, 1)
    W <- D %*% vcov(wf) %*% t(D)
    se <- sqrt(diag(W))
    z <- value/se

    # return tidy data
    data.frame(
      term = c(names(g),"log.p"),
      estimate = value,
      std.error = se,
      statistic = z,
      p.value = 2*pnorm(-abs(z)), 
      row.names = NULL
    )
}
