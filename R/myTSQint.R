#' myTSQint
#'
#' A function which calculates the simultaneous T-squared intervals for component means
#'
#' @param df bivariate data frame
#' @param alpha alpha level for ci's
#'
#' @return confidence intervals and plot of the contour ellipse
#' @export
#'
#' @importFrom MVQuickGraphs confidenceEllipse
#'
#' @examples \dontrun{myTSQint(df = micr, alpha = 0.05)}
myTSQint <- function(df, alpha=0.05) {

  # n & p
  n <- dim(df)[1]
  p <- dim(df)[2]

  # xbar, S, and Sinv
  X <- as.matrix(df)
  ax <- matrix(c(1,0), ncol = 1)
  ay <- matrix(c(0,1))
  xbar <- as.matrix(colMeans(X))
  S <- stats::cov(X)
  qfval <- stats::qf(1-alpha, p, n-p)

  Lx <- t(ax) %*% xbar - sqrt( (p*(n-1))/(n*(n-p)) * qfval * t(ax) %*% S %*% ax)
  Ux <- t(ax) %*% xbar + sqrt( (p*(n-1))/(n*(n-p)) * qfval * t(ax) %*% S %*% ax)

  Ly <- t(ay) %*% xbar - sqrt( (p*(n-1))/(n*(n-p)) * qfval * t(ay) %*% S %*% ay)
  Uy <- t(ay) %*% xbar + sqrt( (p*(n-1))/(n*(n-p)) * qfval * t(ay) %*% S %*% ay)

  cix <- as.matrix(c(Lx,Ux))
  ciy <- as.matrix(c(Ly,Uy))
  row.names(cix) <- c("Lower", "Upper")
  row.names(ciy) <- c("Lower", "Upper")

  # For the plot
  eig <- eigen(S)
  confidenceEllipse(X.mean = xbar, eig = eig,
                                   axes = TRUE,
                                   center = TRUE, n = 42, p = 2)

  list(cix=t(cix), ciy=t(ciy))
}
