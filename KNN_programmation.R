

# Library loading
library(caret)
library(dplyr)
library(metrica)
###################################### Prediction for accuracy of knn

prdt <- function(predicted, labl1, labl2) {
  a <- labl1
  b <- labl2
  pred <- c()
  for (i in 1:nrow(predicted))
  {
    if (predicted[i, 1] >= 0.5)
      pred[i] <- a
    else
      pred[i] <- b
    
  }
  pred <- as.factor(pred)
}



########################

plt <- function(n, Acc) {
  d = as.data.frame(cbind(x = 1:n, Y = as.double(Acc)))
  g <- slice_max(d, Y)
  ggplot(data = d) +
    xlab("Number of k (Neighbors) ") +
    ylab("Accuracy") +
    geom_line( aes(x = x , y = Y) ,color = "purple") +
    scale_y_continuous(limits = c(0, 1)) +
    labs(title = " Plot of the accuracy") +
    theme(plot.title = element_text(face = "bold", hjust = 0.5))+
    geom_point(data=g,aes(x=x ,    y=Y))
  
  
  
  cat("The neighbor's number that maximize the accuracy
  is/are showed by point(s) in black " )
  
}


##########################################################################


### Accuracy to find the best number of neighbors


knn_k_choosing  <-
  function(X_train,
           y_train,
          
           n = (1 / 4) * nrow(X_train) ,
           c1,
           c2) {
    Acc = c()
    for (i in 1:n) {
      fit <- knn3(x =  X_train, y = y_train, k = i)
      
      predicted = predict(fit, X_train)
      
      pred <- prdt(predicted = predicted,
                   labl1 = c1,
                   labl2 = c2)
      
      Acc [i] <- accuracy(obs = y_train, pred = pred)
      
    }
    
    return(plt(n, Acc))
    
  }




##########################################################################
