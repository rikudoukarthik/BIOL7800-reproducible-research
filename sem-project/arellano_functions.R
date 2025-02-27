# copied over (and tidied) from Appendix 1 in the Supplementary Material of Arellano et al. 2014

# two-dimensional commonness of species
hp.function <- function(M) {
  
  hp <- apply(M /rowSums(M), 
              2, 
              function(x) max((1:nrow(M)/nrow(M))[sort(x, decreasing=T)>=(1:nrow(M)/nrow(M))])
              )
  
  hp [which(hp =="-Inf")] <- 0 # assigns hp=0 to very uncommon species
  names(hp) <- colnames(M) # assigns the name of the species
  
  return(hp)

}

# observed proportion of common species, and ordered list of common species
hh.function <- function(M) {
  
  hp <- hp.function(M) # calculates hp indexes
  hh <- max(((1:ncol(M))/ncol(M))[(sort(hp, decreasing=T)/max(hp)) >=
                                    ((1:ncol(M))/ncol(M))]) # calculates hh index
  
  a <- cbind(hp, colSums(M))
  # names of common species (ordered by hp indexes and total abundances)
  common <- rownames(a[order(-a[, 1], -a[, 2]), ])[1:(hh*ncol(M))] 
  
  # output with the hh index of the community and the names of the common species
  return(list(hh=hh, common=common)) 
  
}