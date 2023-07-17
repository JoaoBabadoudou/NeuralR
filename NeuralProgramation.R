
##### Initialization of parameter values
initialisation<-function(x){
  W= matrix( runif(dim(x)),nrow =dim(x)[2],ncol = 1)
  b= vector("numeric",  dim(x)[2]  )
  list(W,b)
}


##### gradient descent function definition
gradient<-function(A,X,y){
  dW=(1/dim(y)[1])*(t( X)  %*%(A-y))
  db=(1/dim(y)[1])* sum(A-y)
  list(dW,db)
}

###### Model definition
modelss<-function(X,W,b){
  Z=X  %*%W + b
  A= 1/(1+exp(-Z))
  return(A)
}




######### Cost function

log_loss<-function(A,y){
  o<-(1 / dim(y)[1]) * sum(-y * log(A) - (1 - y) * log(1 - A ))
  o
}

### Updating function 
updatess<-function(dW,db,W,b,learning_rate){
  W=W-learning_rate*dW
  b=b-learning_rate*db
  list(W,b)
}


##### Prediction function
predicts<- function(X,W,b){
  P=modelss(X,W,b)
  P=as.data.frame (P)
  for (i in 1:nrow(P)){
    if (P[i,]>=0.5) (P[i,]<-1)
    else P[i,]<-0
  }
  P<-as.matrix(P[,1])
  
  return(P)
}

#### Accuracy
accuracy<-function (data = NULL, obs, pred, tidy = FALSE, na.rm = TRUE) 
{
  matrix <- rlang::eval_tidy(data = data, rlang::quo(table({
    {pred}
  }, {
    { obs}
  })))
  correct <- sum(diag(matrix))
  total <- sum(matrix)
  accuracy <- correct/total
  return(accuracy)
}


########## Artificial neural

artificial_neural<-function(X,y,learning_rate,n_iter=1000){
  
  #initialisation
  s=initialisation(X)
  W=s[[1]]
  b=s[[2]]
  Loss=c()
  p_b<-txtProgressBar(min=1,max=n_iter , style=3)
  for (i in 1:n_iter) {
    A=modelss(X,W,b) 
    Loss[i]=log_loss(A,y)
    dW=gradient(A,X,y)[[1]]
    db=gradient(A,X,y)[[2]]
    W=updatess(dW,db,W,b,learning_rate)[[1]]
    b=updatess(dW,db,W,b,learning_rate)[[2]]
    Sys.sleep(0.01) 
    setTxtProgressBar(p_b,i)
  }
  
  y_pred=predicts(X,W,b)
  Accuracy=accuracy(obs=y, pred = y_pred)
  cat("        The accuracy is :", Accuracy )

  # How can I use ggplot to plot the cost?
  #da=c(Loss,1:n_iter)
  #p<-ggplot(da)%>% aes(x=1:n_iter,y=Loss)%>%geom_line()
  #p
  #figure=ploot(da,x=1:n_iter,y=Loss)
  plot(x=1:n_iter,y=Loss,type="l",col="red", lwd = 1,xlab="Number of iteration", 
       ylab="The cost")
  return(list(W=W,b=b,Loss=Loss,A=A))
}

artificial_neural(X,y,learning_rate = 0.1  ,n_iter=150)
