#########################################################################
################# Applicaton with iris dataset ##########################
#########################################################################
# Library loading
library(forcats)
library(caret)
library(dplyr)
library(metrica)

#########################################################################


df=iris#Load the dataset
# Now let's recode our data
## Here, we've changed all the setosa in 1 and gathered the versicolor and 
# virginica into the same group 0
df$Species_rec <- df$Species %>%
  fct_recode(
    "1" = "setosa",
    "0" = "versicolor",
    "0" = "virginica"
  ) %>%
  as.character() %>%
  as.numeric()
df<-df[,-5]
# Let's split our data, 80% for the training and 20% for testing
split_size	=	0.9
sample_size	=	floor(split_size	*	nrow(df)) 
set.seed(123)
train_indices	<-	sample(seq_len(nrow(df)),	size	=	
                          sample_size)
train	<-	df[train_indices,	] 
test	<-	df[-train_indices,	]

# now we're going to transform our dataset in matrix  and use only two explanatory variable
# for best representation

########Train
X=as.matrix(train[,c(1,2)])
y=as.matrix(train[,5])
######## Test
X_test=as.matrix(test[,c(1,2)])
y_test=as.matrix(test[,5])




#################################### Application with 100 iteration

a=artificial_neural(X,y,learning_rate = 0.1  ,n_iter=100)
# We will represent our training set and draw the decision boundary after the training
attach(df)
x1 = c(min(Sepal.Length),max(Sepal.Length))
pm=a[[1]]
dev=a[[2]]
x2=(-pm[1]*x1-dev[1])/pm[2]

x1=as.data.frame(x1)
x2=as.data.frame(x2)
pldf= as.data.frame( c(train,x1,x2))

p<-ggplot(pldf) +
  aes(x = Sepal.Length, y = Petal.Width, colour = Species_rec) +
  geom_point(shape = "circle", 
             size = 1.5) +
  
  labs(title = "This is a scatterplot of the data") +
  theme_gray() +
  scale_color_gradient(low = "red", high = "blue") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))+
  geom_line(aes(x1,x2), color="black", linetype=1,size=1)

p

## The cost function continues to decrase. This inplies that the learning is not yet finished and that 
# we can therefore do better
# We can see that two flowers were misclassified in the learning

# Let's play  a little with the parameters

#### Application with 1000 iteration
a=artificial_neural(X,y,learning_rate = 0.5  ,n_iter=1000)
# The cost function is roughly stable
# We will represent our training set and draw the decision boundary after the training
attach(df)
x1 = c(min(Sepal.Length),max(Sepal.Length))
pm=a[[1]]
dev=a[[2]]
x2=(-pm[1]*x1-dev[1])/pm[2]

x1=as.data.frame(x1)
x2=as.data.frame(x2)
pldf= as.data.frame( c(train,x1,x2))

p<-ggplot(pldf) +
  aes(x = Sepal.Length, y = Petal.Width, colour = Species_rec) +
  geom_point(shape = "circle", 
             size = 1.5) +
  
  labs(title = "This is a scatterplot of the data") +
  theme_gray() +
  scale_color_gradient(low = "red", high = "blue") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))+
  geom_line(aes(x1,x2), color="black", linetype=1,size=1)

p

#This time all the flowers have been well classified
#Let's try to predict the class of the other flowers using the test sample
######## Prediction
pred<-predicts(X_test,a[[1]],a[[2]])
pred
confusion_matrix(obs = y_test,pred=pred,plot=TRUE,print_metrics = TRUE)
# The modele performed well on data it has never encountered
#