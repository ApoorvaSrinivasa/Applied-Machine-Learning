#Homework 3
setwd("C:/JuliaT/Univ of I/CS 498 AML/Homework 3")
#get the data
data_I = read.csv("dataI.csv", header = TRUE)
data_iris = read.csv("iris.csv", header = TRUE)


########################################## Part 1 ##################################################
#store results
error_0n = rep(0, 5)
error_0c = rep(0, 5)
total_error_n = matrix(ncol=4)
total_error_c = matrix(ncol=4)
vec = c("I","II","III","IV","V")
#mean for noiseless iris
mean_iris = colMeans(data_iris)
#pca on noiseless
pca_iris =  prcomp(data_iris, center=TRUE)

#function for X_hat for Ns ref textbook
calcXhat_N = function(data_mat, eigen_vec, mean_vec, num_comp) {
  x_mat = matrix(0, nrow = nrow(data_mat), ncol=ncol(data_mat))
  for(i in 1:nrow(data_mat)) {
    for(j in 1:num_comp){
      x_mat[i,] = 
        x_mat[i,] + t(eigen_vec[,j])%*%(data_mat[i,]-mean_vec)%*%t(eigen_vec[,j])
    }
    x_mat[i,] = mean_vec + x_mat[i,] 
  }
  
  return (x_mat)
}

# Refernce for reconstruction
# https://stats.stackexchange.com/questions/229092/how-to-reverse-pca-and-reconstruct-original-variables-from-several-principal-com

#run for all 5 datasets
for (i in 1:length(vec))
{
  data = read.csv(paste("data",vec[i],".csv",sep=""))
  mean_data = colMeans(data)
  # pca on noisy datasets
  pca_data = prcomp(data, center=TRUE)
  #init for components
  error_n = rep(0, 4)
  error_c = rep(0, 4)
  
  for(n in 1:ncol(data))
  {
    n_comp = n
    # noiseless data
    X_hat = calcXhat_N(as.matrix(data), pca_iris$rotation, mean_iris, n_comp)
    error = mean(rowSums((data_iris-X_hat)^2))
    #store in a vector for all Ns per dataset
    error_n[n_comp] = error
    
    ## noisy data
    X_hat = pca_data$x[,1:n_comp] %*% t(pca_data$rotation[,1:n_comp])
    X_hat = scale(X_hat, center = -mean_data, scale = FALSE)
    error = mean(rowSums((data_iris-X_hat)^2))
    #store in a vector for all Cs per dataset
    error_c[n_comp] = error
    
  }  ##end of inner for loop
  
  #store for all
  total_error_n = rbind(total_error_n, error_n)
  total_error_c = rbind(total_error_c, error_c)
 
  ######################### For 0N and 0c ################################
  len = as.vector(mean_data)
  mat = data.frame(rep(len[1],150))
  mean_data_frame = cbind(mat,len[2],len[3],len[4])
  pca_data_mean = prcomp(mean_data_frame, center=TRUE)
  
  # Noiselessdata
  X_hat = pca_data_mean$x[,4] %*% t(pca_data_mean$rotation[,4])
  X_hat = scale(X_hat, center = -mean_iris, scale = FALSE)
  
  error = mean(rowSums((data_iris-X_hat)^2))
  error_0n[i] = error
  
  ## Noisy data
  X_hat = pca_data_mean$x[,4] %*% t(pca_data_mean$rotation[,4])
  X_hat = scale(X_hat, center = -mean_data, scale = FALSE)
  
  error = mean(rowSums((data_iris-X_hat)^2))
  error_0c[i] = error
 
}
## end of outer for loop

#create the files
n = matrix(error_0n)
n = cbind(n,total_error_n[2:6,])
colnames(n) = c("0N","1N","2N","3N","4N")
rownames(n) = c("Data1","Data2","Data3","Data4","Data5")

c = matrix(error_0c)
c = cbind(c,total_error_c[2:6,])
colnames(c) = c("0c","1c","2c","3c","4c")
rownames(c) = c("Data1","Data2","Data3","Data4","Data5")

#Final numbers
final = cbind(n,c)
write.csv(final,file="apoorva6-idt2-numbers.csv", row.names = FALSE)


########################################## Part 2 ##################################################
# Retransform data 1

mean_dataI = colMeans(data_I)
pca_dataI = prcomp(data_I, center=TRUE)

n_comp = 2
X_hat = pca_dataI$x[,1:n_comp] %*% t(pca_dataI$rotation[,1:n_comp])
X_hat = scale(X_hat, center = -mean_dataI, scale = FALSE)

colnames(X_hat) <- c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width")
write.csv(X_hat,file="apoorva6-idt2-recon.csv", row.names = FALSE)


###########################################end#######################################################



