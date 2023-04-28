# Harmonic centrality function
Hi <- function (g) {
  # take total of all nodes
  n <- vcount(g)
  # initialize lists for harmonic centrality aggregation before standardization (sdij) and after standardization (hi)
  hi <- NULL
  sdij <- NULL
  # calculate harmonic centrality for each node
  for(i in 1:n) {
    for(j in 1:n) {
      if(i != j) {
        dij <- distances(g, i, j)
        sdij[j] <- 1/dij
      } else {next}
    }
    # omit any values that do not return a numeric value
    sdij <- na.omit(sdij)
    # sum and standardize harmonic centrality values
    hi[i] <- (1/(n-1))*sum(sdij)
  }
  return(hi)
}