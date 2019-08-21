#' Function to compute the Jeffries-Matusita and Transformed divergence spectral distance for a set of samples
#' @param samples a data frame of samples with the band measurement and a class 
#' @param class the name of the column containing the class for each sample. Recommended to turn it into a character vector if integer, 
#'              otherwise turned to character with the prefix 'b'.
#' @param band_names the name of the bands to calculate separability on, must be a vector matching the names in the samples              
#' @param n if the number of samples is not equal for every class, randomly select a subset for each class
#' 

separability <- function(samples, class, band_names, n = 200, overall_only = T) {
  require(data.table)
  require(proxy) 
  
  ## Function to calculate the Jeffries-Matusita spectral distance
  jm.dist = function ( Vector.1 , Vector.2 ) {
    # this function adapted from: 
    # https://stats.stackexchange.com/questions/78849/measure-for-separability
    Matrix.1 <- as.matrix (Vector.1)
    Matrix.2 <- as.matrix (Vector.2)
    mean.Matrix.1 <- mean ( Matrix.1 )
    mean.Matrix.2 <- mean ( Matrix.2 )
    mean.difference <- mean.Matrix.1 - mean.Matrix.2
    cv.Matrix.1 <- cov ( Matrix.1 )
    cv.Matrix.2 <- cov ( Matrix.2 )
    p <- ( cv.Matrix.1 + cv.Matrix.2 ) / 2
    # calculate the Bhattacharryya index
    bh.distance <- 0.125 *t ( mean.difference ) * p^ ( -1 ) * mean.difference +
      0.5 * log (det ( p ) / sqrt (det ( cv.Matrix.1 ) * det ( cv.Matrix.2 )))
    # calculate Jeffries-Matusita
    # following formula is bound between 0 and 2.0
    jm.distance <- 2 * ( 1 - exp ( -bh.distance ) )
    # also found in the bibliography:
    # jm.distance <- 1000 * sqrt (   2 * ( 1 - exp ( -bh.distance ) )   )
    # the latter formula is bound between 0 and 1414.0
    return(jm.distance)
  }
  
  ## Function to calculate the transformed divergence spectral distance
  transformed.divergence = function( Vector.1 , Vector.2 ) {
    # this function adapted from: 
    # https://stats.stackexchange.com/questions/78849/measure-for-separability
    Matrix.1 <- as.matrix (Vector.1)
    Matrix.2 <- as.matrix (Vector.2)
    mean.Matrix.1 <- mean ( Matrix.1 )
    mean.Matrix.2 <- mean ( Matrix.2 )
    mean.difference <- mean.Matrix.1 - mean.Matrix.2
    cv.Matrix.1 <- cov ( Matrix.1 )
    cv.Matrix.2 <- cov ( Matrix.2 )
    
    # calculate the divergence 
    # trace (is the sum of the diagonal elements) of a square matrix 
    trace.of.matrix <- function ( SquareMatrix ) { 
      sum ( diag ( SquareMatrix ) ) } 
    
    # term 1 
    divergence.term.1 <- 1/2 * 
      trace.of.matrix ( 
        ( cv.Matrix.1 - cv.Matrix.2 ) * 
          ( cv.Matrix.2^ (-1) - cv.Matrix.1^ (-1) ) 
      ) 
    
    # term 2 
    divergence.term.2 <- 1/2 * 
      trace.of.matrix ( 
        ( cv.Matrix.1^ (-1) + cv.Matrix.2^ (-1) ) * 
          ( mean.Matrix.1 - mean.Matrix.2 ) * 
          t ( mean.Matrix.1 - mean.Matrix.2 ) 
      ) 
    
    # divergence 
    divergence <- divergence.term.1 + divergence.term.2 
    
    
    # --%<------------------------------------------------------------------------ 
    # and the transformed divergence 
    transformed.divergence  <- 2 * ( 1 - exp ( - ( divergence / 8 ) ) ) 
    
    return(transformed.divergence)
  }
  
  setDT(samples)
  
  if(class(samples$class) == 'integer') {
    samples$class = paste0('b', samples$class)
  }
  
  if(length(unique(table(samplesGEE$class))) > 1) {
    df = samples[, .SD[sample(.N, n)] , by=.(class)]
  } else {
    df = samples
  }
  
  sep = function(x, method){
    band_df = df[, c(class, x), with = F]
    band_unstack = unstack(band_df, reformulate(class, x))
    band_dist = dist(band_unstack, method = method, by_rows = F)
  }
  
  # Jeffries-Matusita distance
  bands_jmdist = lapply(bands, FUN = sep, method = jm.dist)
  names(bands_jmdist) = bands
  ov = Reduce('+', bands_jmdist) / length(bands_jmdist)
  bands_jmdist[['overall']] = ov
  
  # Transformed divergence distance
  bands_tddist = lapply(bands, FUN = sep, method = transformed.divergence)
  names(bands_tddist) = bands
  ov = Reduce('+', bands_tddist) / length(bands_tddist)
  bands_tddist[['overall']] = ov
  
  res = list(jmdist = bands_jmdist, tddist = bands_tddist)
  
  if(overall_only == T){
    list(jmdist = res$jmdist$overall, tddist = res$tddist$overall)
  } else {res}
}

library(dplyr)
samplesGEE <- read.csv('samples2015forRF.csv') %>% 
  mutate(class2 = ifelse(class == 0, 'water', ifelse(class == 1, 'shadow', 
                         ifelse(class == 2, 'glacier', ifelse(class == 3, 'bare', 
                                       ifelse(class == 4, 'vegetation', 'clouds'))))))
bands <- names(df[,-c('system.index', 'class', 'class2', '.geo'),  with = F])

separability(samplesGEE, class = 'class2', band_names = bands, n = 195)