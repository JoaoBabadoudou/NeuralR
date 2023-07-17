<<<<<<< HEAD
###############################################################################
############################ First Neural Network #############################
###############################################################################


# Let's jump to our first Neural Network 


##### Initialization of parameter values
initialisation<-function(n0,n1,n2,X){
  W1= matrix( data=c(0.1),nrow =n1,ncol = n0)
  b1= vector("numeric",  min(dim(X))  )
  W2= matrix( data=c(0.1),nrow =n2,ncol = n1)
  b2= vector("numeric",  n2  )
  parametres<-list(W1=W1,b1=b1,W2=W2,b2=b2)
  return(parametres)
}


# The forward propagation

forward_propagation<-function(X,parametres){
  W1=parametres[[1]]
  b1=parametres[[2]]
  W2=parametres[[3]]
  b2=parametres[[4]]
  
  
  
  Z1=W1  %*% X + b1
  A1= 1/(1+exp(-Z1))
  
  Z2=W2  %*% A1 + b2
  A2= 1/(1+exp(-Z2))
  
  activations=list(A1=A1,A2=A2)
  return(activations)
}


# The back propagation

back_propagation<-function(X,y,activations,parametres){
  
  A1=activations[[1]]
  A2=activations[[2]]
  W2=parametres[[3]]
  m=dim(y)[1]
  
  dZ2=A2-y
  dW2=(1/m)*(dZ2  %*% (t(A1)))
  db2=(1/m)* colSums(dZ2)
  
  
  dZ1=(t(W2) %*% dZ2)* A1 * (1-A1)
  dW1=(1/m)*(dZ1  %*%(t(X)))
  db1=(1/m)* colSums(dZ1)
  
  gradients= list(dW1=dW1,db1=db1,dW2=dW2,db2=db2)
  return(gradients)
}

updatess<-function(gradients, parametres,learning_rate){
  W1=parametres[[1]]
  b1=parametres[[2]]
  W2=parametres[[3]]
  b2=parametres[[4]]
  
  dW1=gradients[[1]]
  db1=gradients[[2]]
  dW2=gradients[[3]]
  db2=gradients[[4]]
  
  W1=W1-learning_rate*dW1
  b1=b1-learning_rate*db1
  W2=W2-learning_rate*dW2
  b2=b2-learning_rate*db2
  parametres<-list(W1=W1,b1=b1,W2=W2,b2=b2) 
  return(parametres)
}


log_loss<-function(A,y){
  o<-(1 / dim(y)[1]) * sum(-y * log(A) - (1 - y) * log(1 - A ))
  o
}

predictss<- function(X,parametres,activations){
  ac=forward_propagation(X,parametres )
  A2=t(activations[[2]])
  A2=as.data.frame (A2)
  for (i in 1:nrow(A2)){
    if (A2[i,]>=0.5) (A2[i,]<-1)
    else A2[i,]<-0
  }
  A2<-as.matrix(A2[,1])
  
  return(A2)
} 

predictClass <- function(X,parametres){
  ac=forward_propagation(X,parametres )
  A2=t(ac[[2]])
  A2=as.data.frame (A2)
  for (i in 1:nrow(A2)){
    if (A2[i,]>=0.5) (A2[i,]<-1)
    else A2[i,]<-0
  }
  A2<-as.matrix(A2[,1])
  
  return(A2)
}

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





neural_network<-function(X,y,n1=2,learning_rate,n_iter=1000){
  
  #initialisation
  n0=dim(X)[1]
  n2=dim(y)[1]
  parametres=initialisation(n0,n1,n2,X)
  
  Loss=c()
  p_b<-txtProgressBar(min=1,max=n_iter , style=3)
  for (i in 1:n_iter) {
    activations=forward_propagation(X,parametres) 
    gradients=back_propagation(X,y,activations,parametres)
    parametres=updatess(gradients,parametres,learning_rate)
    Loss[i]=log_loss(activations[[2]],y)
    
    
    Sys.sleep(0.01) 
    setTxtProgressBar(p_b,i)
  }
  
  y_pred=predictss(X,parametres,activations)
  Accuracy=accuracy(obs=t(y), pred = y_pred)
  
  cat("        The accuracy is :", Accuracy )
  da=c(Loss,1:n_iter)
  # p<-ggplot2::ggplot(da)%>% aes(x=1:n_iter,y=Loss)%>%geom_line()
  # p
  #figure=ploot(da,x=1:n_iter,y=Loss)
  plot(x=1:n_iter,y=Loss,type="l",col="red", lwd = 1,xlab="Nombre d'iteration", 
        ylab="Cout")
  return(list(Parameters=parametres, A=activations[[2]], prediction=y_pred))
}
=======
###############################################################################
############################ First Neural Network #############################
###############################################################################


# Let's jump to our first Neural Network 


##### Initialization of parameter values
initialisation<-function(n0,n1,n2,X){
  W1= matrix( data=c(0.1),nrow =n1,ncol = n0)
  b1= vector("numeric",  min(dim(X))  )
  W2= matrix( data=c(0.1),nrow =n2,ncol = n1)
  b2= vector("numeric",  n2  )
  parametres<-list(W1=W1,b1=b1,W2=W2,b2=b2)
  return(parametres)
}


# The forward propagation

forward_propagation<-function(X,parametres){
  W1=parametres[[1]]
  b1=parametres[[2]]
  W2=parametres[[3]]
  b2=parametres[[4]]
  
  
  
  Z1=W1  %*% X + b1
  A1= 1/(1+exp(-Z1))
  
  Z2=W2  %*% A1 + b2
  A2= 1/(1+exp(-Z2))
  
  activations=list(A1=A1,A2=A2)
  return(activations)
}


# The back propagation

back_propagation<-function(X,y,activations,parametres){
  
  A1=activations[[1]]
  A2=activations[[2]]
  W2=parametres[[3]]
  m=dim(y)[1]
  
  dZ2=A2-y
  dW2=(1/m)*(dZ2  %*% (t(A1)))
  db2=(1/m)* colSums(dZ2)
  
  
  dZ1=(t(W2) %*% dZ2)* A1 * (1-A1)
  dW1=(1/m)*(dZ1  %*%(t(X)))
  db1=(1/m)* colSums(dZ1)
  
  gradients= list(dW1=dW1,db1=db1,dW2=dW2,db2=db2)
  return(gradients)
}

updatess<-function(gradients, parametres,learning_rate){
  W1=parametres[[1]]
  b1=parametres[[2]]
  W2=parametres[[3]]
  b2=parametres[[4]]
  
  dW1=gradients[[1]]
  db1=gradients[[2]]
  dW2=gradients[[3]]
  db2=gradients[[4]]
  
  W1=W1-learning_rate*dW1
  b1=b1-learning_rate*db1
  W2=W2-learning_rate*dW2
  b2=b2-learning_rate*db2
  parametres<-list(W1=W1,b1=b1,W2=W2,b2=b2) 
  return(parametres)
}

log_loss<-function(A,y){
  o<-(1 / dim(y)[1]) * sum(-y * log(A) - (1 - y) * log(1 - A ))
  o
}

predictss<- function(X,parametres,activations){
  ac=forward_propagation(X,parametres )
  A2=t(activations[[2]])
  A2=as.data.frame (A2)
  for (i in 1:nrow(A2)){
    if (A2[i,]>=0.5) (A2[i,]<-1)
    else A2[i,]<-0
  }
  A2<-as.matrix(A2[,1])
  
  return(A2)
} 

predictClass <- function(X,parametres){
  ac=forward_propagation(X,parametres )
  A2=t(ac[[2]])
  A2=as.data.frame (A2)
  for (i in 1:nrow(A2)){
    if (A2[i,]>=0.5) (A2[i,]<-1)
    else A2[i,]<-0
  }
  A2<-as.matrix(A2[,1])
  
  return(A2)
}

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





neural_network<-function(X,y,n1=2,learning_rate,n_iter=1000){
  
  #initialisation
  n0=dim(X)[1]
  n2=dim(y)[1]
  parametres=initialisation(n0,n1,n2,X)
  
  Loss=c()
  p_b<-txtProgressBar(min=1,max=n_iter , style=3)
  for (i in 1:n_iter) {
    activations=forward_propagation(X,parametres) 
    gradients=back_propagation(X,y,activations,parametres)
    parametres=updatess(gradients,parametres,learning_rate)
    Loss[i]=log_loss(activations[[2]],y)
    
    
    Sys.sleep(0.01) 
    setTxtProgressBar(p_b,i)
  }
  
  y_pred=predictss(X,parametres,activations)
  Accuracy=accuracy(obs=t(y), pred = y_pred)
  
  cat("        The accuracy is :", Accuracy )
  da=c(Loss,1:n_iter)
  p<-ggplot2::ggplot(da)%>% aes(x=1:n_iter,y=Loss)%>%geom_line()
  p
  #figure=ploot(da,x=1:n_iter,y=Loss)
  # plot(x=1:n_iter,y=Loss,type="l",col="red", lwd = 1,xlab="Nombre d'iteration", 
       # ylab="Cout")
  return(list(Parameters=parametres, A=activations[[2]], prediction=y_pred))
}
>>>>>>> 0637ac59f7b025070f6dd9a7fa355697cc2ccc7a
