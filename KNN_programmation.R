

#### Prediction for accuracy of knn

prdt <- function(predicted, labl1,labl2){
  for (i in 1:nrow(predicted))
  {
    
    if (predicted[i,1]== 1) pred[i] <- "labl1" else pred[i] <- "labl2"
    
  }
}



knn_k_choosing <- function(){
 
  li
  library(caret)
  Acc=c()
  for (n in 1:20) {
    
    ############## model fitting w
    
    fit <- knn3(x =  X_train, y= y_train,k=n )
    ####### Predicting
    predicted= predict(fit, X_test)
    pred <- c()
    
    for (i in 1:nrow(predicted))
    {
      
      if (predicted[i,1]== 1) pred[i] <- "setosa" else pred[i] <- "versicolor"
      
    }
    pred <- as.factor(pred)
    ######## accuracy
    
    Acc [n] <- accuracy(obs = y_test, pred = pred) 
    
  }
   
}




{
Acc=c()
for (n in 1:20) {
  
  ############## model fitting w
  
  fit <- knn3(x =  X_train, y= y_train,k=n )
  ####### Predicting
  predicted= predict(fit, X_test)
  pred <- c()
  
  for (i in 1:nrow(predicted))
    {
    
    if (predicted[i,1]== 1) pred[i] <- "setosa" else pred[i] <- "versicolor"
    
    }
  pred <- as.factor(pred)
  ######## accuracy
  
  Acc [n] <- accuracy(obs = y_test, pred = pred) 
  
              }
}
