library(esd)

monthly.hot <- function(X, threshold) {
  n.hot <- as.monthly(X, FUN='count', threshold=threshold)
  nv <- as.monthly(X, FUN='nv')
  f.hot <- n.hot
  z <- coredata(n.hot)/coredata(nv)
  coredata(f.hot) <- z
  attr(f.hot, 'variable') <- 'f.hot'
  attr(f.hot, 'longname') <- paste('fraction of days with',
                                   attr(X,'param'), 'above', threshold)
  return(f.hot)
}

