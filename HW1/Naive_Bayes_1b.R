
#Read csv data
pimadata<-read.csv('pima-indians-diabetes.csv', header=TRUE)

#Treating the value 0 in columns 3,4,6 and 8 as NAs
for( i in c(3,4,6,8))
{
  treatzero <- pimadata[,i] == 0
  pimadata[treatzero,i] <- NA
}

#Allocate empty vectors to store accuracies of each individual iterations 
trainaccuracy <- rep(0,10)
testaccuracy <- rep(0,10)

# run training/testing 10 times
for (i in 1:10)
{
  # Split data into test and train
  index <- sample(seq_len(nrow(pimadata)), size = floor(0.80 * nrow(pimadata)))
  train <- pimadata[index, ]
  test <- pimadata[-index, ]
  
  #Extracting positive and negative labels for training dataset
  pos_labels <- subset(train,Class > 0)
  neg_labels <- subset(train,Class == 0)
  
  #Extracting the labels and data for test/train and storing them seperately
  y_test <- test[,c(9)]
  x_test <- test[,-c(9)]
  y_train <- train[,c(9)]
  x_train <- train[,-c(9)]
  
  
  x_train_pos <- pos_labels[,-c(9)]
  x_train_neg <- neg_labels[,-c(9)]
  
  #For positive label
  # P(positive_label)
  p_pos <- nrow(x_train_pos) / (nrow(x_train_pos)+nrow(x_train_neg))
  # Mean
  pos_mean <- sapply(x_train_pos, mean, na.rm=TRUE)
  # Std deviation
  pos_sd <- sapply(x_train_pos, sd,na.rm=TRUE)
  
  #For negative label
  # P(Negative_label)
  p_neg <- nrow(x_train_neg) / (nrow(x_train_pos)+nrow(x_train_neg))
  # Mean
  neg_mean <- sapply(x_train_neg, mean,na.rm=TRUE)
  # Std deviation
  neg_sd <- sapply(x_train_neg, sd,na.rm=TRUE)
  
  function(fun,mean,sd,p)
  {
    pos_log_prob <- rowSums(log(mapply(dnorm,x_train,pos_mean,pos_sd))) + log(p_pos)
  }
  ## For training
  # Finding normal distribution cdf - log probabilities P(x|Y)
  pos_log_prob <- rowSums(log(mapply(dnorm,x_train,pos_mean,pos_sd)), na.rm=TRUE) + log(p_pos)
  neg_log_prob <- rowSums(log(mapply(dnorm,x_train,neg_mean,neg_sd)), na.rm=TRUE) + log(p_neg)
  
  #Checking prediction accuracy
  correct_pos_train <- ifelse(pos_log_prob>neg_log_prob,1,0)
  trainaccuracy[i] <- (sum(correct_pos_train == y_train))/length(y_train)
  
  ## For test data
  # Finding normal distribution cdf - log probabilities P(x|Y)
  pos_log_prob <- rowSums(log(mapply(dnorm,x_test,pos_mean,pos_sd)), na.rm=TRUE) + log(p_pos)
  neg_log_prob <- rowSums(log(mapply(dnorm,x_test,neg_mean,neg_sd)), na.rm=TRUE) + log(p_neg)
  
  #Checking prediction accuracy
  correct_pos_test <- ifelse(pos_log_prob>neg_log_prob,1,0)
  testaccuracy[i] <- (sum(correct_pos_test == y_test))/length(y_test)
}


train_accuracy_with_treatment <- mean(trainaccuracy)
test_accuracy_with_treatmnent <- mean(testaccuracy)