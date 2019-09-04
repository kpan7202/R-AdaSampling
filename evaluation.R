data.2016.akt <- read.csv("2016 result (Akt).csv")
data.2016.mtor <- read.csv("2016 result (mTOR).csv")
prob.result <- read.csv("prob_result.csv")

evaluation = function(data.2016,result_from_model,threshold = 0.5) {
  data.2016$modelResult <- with(result_from_model,
                                result_from_model$avg.prob[match(data.2016$Name,
                                                                 result_from_model$identifier)])
  #truth = c()
  #result = c()
  #for (i in 1:nrow(data.2016)){
  #  truth = c(truth, ifelse(data.2016$Full.model.predict[i]>threshold,1,0))
  #  result = c(result, ifelse(data.2016$modelResult[i]>threshold,1,0))
  #}
  data.2016$modelResult <- as.numeric(levels(data.2016$modelResult))[data.2016$modelResult]
  truth <- ifelse(data.2016$Full.model.predict>threshold,1,0)
  result <- ifelse(data.2016$modelResult>threshold,1,0)
  
  TP <- c(sum((truth == result)[truth == 1]))
  TN <- c(sum((truth == result)[truth == 0]))
  FP <-  c(sum((truth != result)[truth == 0]))
  FN <-  c(sum((truth != result)[truth == 1]))
  
  acc <- (TN+TP)/(TN+TP+FP+FN)*100
  spec <- TN/(TN+FP)*100
  sens <- TP/(TP+FN)*100
  f1 <- 2*TP/(2*TP+FP+FN)*100
  geo <- sqrt((TP/(TP+FN))*(TP/(TP+FP)))*100
  
  cat("accuracy =", round(acc,2),", ")
  cat("sensitivity =",round(sens,2)," specificity =",round(spec,2),", ")
  cat("f1 =" ,round(f1,2) , "geo-mean =",round(geo,2),"\n")
  c(acc,sens,spec,f1,geo)
}

scores = matrix(nrow = ncol(prob.result)-3,ncol = 6)
i <- 1
for (col in colnames(prob.result[,4:ncol(prob.result)])) {
  model.result <- data.frame(cbind(as.character(prob.result$Identifier), prob.result[[col]]))
  colnames(model.result) <- c("identifier", "avg.prob")
  
  if (substr(col,1,3) == "Akt") {
    scores[i,] <- c(col,evaluation(data.2016.akt,model.result,0.5))
  }
  else {
    scores[i,] <- c(col,evaluation(data.2016.mtor,model.result,0.5))
  }
  
  i <- i + 1
}

colnames(scores) <- c("ID","Accuracy","Sensitivity","Specificity","F1-score", "Geo-mean")
write.csv(scores, "scores_result.csv", row.names = FALSE)
