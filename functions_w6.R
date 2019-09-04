# accuracy
Acc <- function(mat) {
  apply(mat, 2, function(x) {
    TN <- x[1]
    FP <- x[2]
    TP <- x[3]
    FN <- x[4]
    (TP+TN)/(TP+FP+TN+FN)
  })
}

# sensitivity
Se <- function(mat) {
  apply(mat, 2, function(x) {
    TN <- x[1]
    FP <- x[2]
    TP <- x[3]
    FN <- x[4]
    TP/(TP+FN)
  })
}

# specificity
Sp <- function(mat) {
  apply(mat, 2, function(x) {
    TN <- x[1]
    FP <- x[2]
    TP <- x[3]
    FN <- x[4]
    TN/(FP+TN)
  })
}

# F1 score
F1 <- function(mat) {
  apply(mat, 2, function(x){
    TN <- x[1]
    FP <- x[2]
    TP <- x[3]
    FN <- x[4]
    2*TP/(2*TP+FP+FN)
  })
}

# geometric mean
GMean <- function(mat) {
  apply(mat, 2, function(x){
    TN <- x[1]
    FP <- x[2]
    TP <- x[3]
    FN <- x[4]
    sqrt((TP/(TP+FN))*(TP/(TP+FP)))
  })
}   

## create a wrapper function for evaluation
evaluate <- function(TN, FP, TP, FN) {
  mat <- rbind(TN, FP, TP, FN)
  cat(c("acc:", round(mean(Acc(mat)), digits=3)))
  cat(" ")
  
  cat(c("sen:", round(mean(Se(mat)), digits=3)))
  cat(" ")
  
  cat(c("sep:", round(mean(Sp(mat)), digits=3)))
  cat(" ")
  
  cat(c("F1:", round(mean(F1(mat)), digits=3)))
  cat(" ")
  
  cat(c("GM:", round(mean(GMean(mat)), digits=3)))
  cat(" ")
}