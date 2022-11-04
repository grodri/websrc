gauher <- function(n, eps = 3e-14, maxit = 10) {
# abscissas and weights for Gauss-Hermite quadrature
# We use Gauss-Hermite code from Recipes in C p.154
# and then change x to x*sqrt(2) and w to w/sqrt(pi)
	pim4 <- 0.751125544464942
	m <- floor((n + 1)/2)
	x <- w <- matrix(0, n, 1)
	for(i in 1:m) {
		if (i==1) {
			z <- sqrt(2 * n + 1) - 1.85575 * (2 * n + 1)^(-0.16667)
		}
		else if (i==2) {
			z <- z - (1.14 * n^0.426)/z	
		}
		else if (i==3) {
			z <- 1.86 * z - 0.86 * x[1]
		}
		else if (i==4) {
			z <- 1.91 * z - 0.91 * x[2]
		}
		else {
			z <- 2 * z - x[i-2]
		}
		its <- 0
		done <- FALSE
        while(!done & its < maxit) {
			p1 <- pim4
			p2 <- 0
			for(j in 1:n) {
				p3 <- p2
				p2 <- p1
				p1 <- z * sqrt(2/j) * p2 - sqrt((j-1)/j) * p3
			}
			pp <- sqrt(2 * n) * p2
			z1 <- z
			z <- z1 - p1/pp
			its <- its + 1
			done <- abs(z - z1) <= eps
		}
		if (its > maxit) {
            stop("gauher has not converged")
		}
		x[i] <- z
		x [n + 1 - i] <- - z
		w[i] <- 2/(pp * pp)
		w[n + 1 - i] <- w[i]
	}
	if (2 * m > n) {
		x[m] <- 0
	}
	data.frame(z = x * sqrt(2), w = w/sqrt(pi))	
}
