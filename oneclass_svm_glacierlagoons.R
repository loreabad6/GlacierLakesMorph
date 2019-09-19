samples <- read.csv('samplesforOCSVM.csv')

library(dplyr)
library(corrplot)
corrplot(cor(samples[,2:16]))
index <- c('AWEI1','AWEI2','MSAVI2','NDSI','NDVI','NDWI','ILI','WRI','Thermal')
samp_ind <- samples[,index]
corrplot(cor(samp_ind),order = "hclust", addrect = 3)
subset <- sample_frac(samples, 0.001)

library(e1071)
##------- tune nu and gamma -----------
#make vectors containing the options for nu and gamma
vec.nu <- seq(from = 0.001, to = 0.5, by = 0.001)
vec.gamma <- c(2^(c(-10:10)))

#create a matrix to store results in
res.tuned <- matrix(
  0,
  ncol = length(vec.nu),
  nrow = length(vec.gamma)
)


#create a value to store the highest accuracy
max.acc <- 0

#run a one-class svm for every combination of nu and gamma and store accuracies 
for(n in 1:length(vec.nu)) {
  for(g in 1:length(vec.gamma)) {
    svm.tuned <- svm(
      x = samples[,2:16],
      y = samples[,"class"],
      type = "one-classification",
      kernel = "radial",
      nu = vec.nu[n],
      gamma = vec.gamma[g],
      scale = FALSE,
      cross = 10
    )
    
    res.tuned[g,n] <- svm.tuned$tot.accuracy
    
    max.acc <- ifelse(
      svm.tuned$tot.accuracy > max.acc,
      svm.tuned$tot.accuracy,
      max.acc
    )
  }
}


#obtain row and column number for maximum accuracy value in results matrix
max <- which(res.tuned == max(res.tuned), arr.ind = TRUE)

#obtain optimal values for nu and gamma
opt.nu <- vec.nu[max[[1,2]]]
opt.ga <- vec.gamma[max[[1,1]]]

rm(svm.tuned,g,n,vec.gamma,vec.nu,max)

##------- test different kernels -----------
#create a matrix to store results in for shrimp
res.kernel <- matrix(
  0,
  ncol = 4,
  nrow = 2
)
res.kernel[2,] <- c("linear","polynomial","radial","sigmoid")
rownames(res.kernel) <- c("accuracy","kernel")

#create a vector with the different values to be tested
vec.kernel <- c("linear","polynomial","radial","sigmoid")

#run an one-class svm for every different kernel function
for(k in 1:4) {
  svm.tuned <- svm(
    x = samples[,2:16],
    y = samples[,"class"],
    type = "one-classification",
    kernel = vec.kernel[k],
    nu = opt.nu,
    gamma = opt.ga,
    scale = FALSE,
    cross = 10
  )
  
  res.kernel[1,k] <- svm.tuned$tot.accuracy
  
  opt.k <- ifelse(
    svm.tuned$tot.accuracy > max.acc,
    vec.kernel[k],
    "radial"
  )
}

##-------------- final models --------------
#create the final model with all optimal parameters
model <- svm(
  class ~.,
  data = samples[,2:17],
  type = "one-classification",
  kernel = opt.k,
  nu = opt.nu,
  gamma = opt,
  scale = FALSE,
  cross = 10
)

model