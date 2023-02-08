get_maf <-
function (gt) {
  x <- gt
  f0hom <- function(x) {
    length(which(x == 0))
  }
  fhet <- function(x) {
    length(which(x == 1))
  }
  f1hom <- function(x) {
    length(which(x == 2))
  }

  hom0 <- apply(x, 2, f0hom)
  het <- apply(x, 2, fhet)
  hom1 <- apply(x, 2, f1hom)
  alt_freq <- (2 * hom1 + het)/(2 * (hom0 + het + hom1))
  ref_freq <- 1-(2 * hom1 + het)/(2 * (hom0 + het + hom1))
  min_freq <- alt_freq

  for ( i in 1:ncol(gt) ) {
    
    if ( alt_freq[i] > ref_freq[i] ) {
      
      min_freq[i] <- ref_freq[i]
      
    } 
    
  }
  return(min_freq)
  }
