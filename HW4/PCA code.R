#Homework 4
setwd("C:/JuliaT/Univ of I/CS 498 AML/Homework 4")
#downloaded dataset from https://www.cs.toronto.edu/~kriz/cifar.html. 
##########ref for reading the files -  https://stackoverflow.com/questions/32113942/importing-cifar-10-data-set-to-r
############################################# Read Data ##############################################################
# Read binary file and convert to integer vectors
labels = read.table("cifar-10-batches-bin/batches.meta.txt")
images.rgb = list()
images.lab = list()
num.images = 10000 # Set to 10000 to retrieve all images per file to memory
flat_list = list()

# Cycle through all 5 binary files
for (f in 1:5) {
  to.read = file(paste("cifar-10-batches-bin/data_batch_", f, ".bin", sep=""), "rb")
  
  for(i in 1:num.images) {
    l = readBin(to.read, integer(), size=1, n=1, endian="big")
    r = as.integer(readBin(to.read, raw(), size=1, n=1024, endian="big"))
    g = as.integer(readBin(to.read, raw(), size=1, n=1024, endian="big"))
    b = as.integer(readBin(to.read, raw(), size=1, n=1024, endian="big"))
    index = num.images * (f-1) + i
    images.rgb[[index]] = data.frame(r, g, b)
    images.lab[[index]] = l+1
  }
  close(to.read)
  remove(l,r,g,b,f,i,index, to.read)
}
# add the test data 
to.read = file("cifar-10-batches-bin/test_batch.bin", "rb")
for(i in 1:num.images) {
  l = readBin(to.read, integer(), size=1, n=1, endian="big")
  r = as.integer(readBin(to.read, raw(), size=1, n=1024, endian="big"))
  g = as.integer(readBin(to.read, raw(), size=1, n=1024, endian="big"))
  b = as.integer(readBin(to.read, raw(), size=1, n=1024, endian="big"))
  index = 50000 + i
  images.rgb[[index]] = data.frame(r, g, b)
  images.lab[[index]] = l+1
}
close(to.read)
remove(l,r,g,b,i,index, to.read)

############################################ Flatten Iamges ########################################################################
numSamples = length(images.rgb)
#flatten the dataframes to vectors
for(i in 1:numSamples){
   test = images.rgb[[i]]
   test = c(t(test))
   flat_list[[i]] = as.numeric(test)
   
}

#matrix for images only
images_mat = matrix(unlist(flat_list), byrow=TRUE, nrow=length(flat_list) )
#matrix for the labels only
images_mat_l = matrix(unlist(images.lab), byrow=TRUE, nrow=length(images.lab) )
#image + label
images_mat_full = cbind(images_mat, images_mat_l)

########################################### Part A ###################################################################################
#num of classes
numClass = length(unique(images_mat_full[, 3073]))
#to store the mean images
store_means = matrix(NA, nrow = numClass, ncol = ncol(images_mat))

#store_means = list()
for(i in 1:numClass)
{
  #subsets by labels
  label = subset(images_mat_full,images_mat_full[,3073] == i)
  #get only the pixels
  label_px = label[,-c(3073)]
  
  image_mean = apply(label_px,2,mean)
  store_means[i, ] = image_mean
}

#convert  store_means and labels to proper format for image display input
mean_label_list = list()
mean_rgb_list = list()
img_df = data.frame(nrow = 1024, ncol = 3)
#prepare for input
for(i in 1:10){
  mean_label_list[[i]] = i
  img_df = data.frame(r = (store_means[i, 1:1024]),
                      g = (store_means[i, 1025:2048]),
                      b = (store_means[i, 2049:3072]))
  
  mean_rgb_list[[i]] = img_df
 
}

#function for image display
showImage = function(index) {
  img = mean_rgb_list[[index]]
  img_r = matrix(img$r, ncol=32, byrow = TRUE)
  img_g = matrix(img$g, ncol=32, byrow = TRUE)
  img_b = matrix(img$b, ncol=32, byrow = TRUE)
  img_col = rgb(img_r, img_g, img_b, maxColorValue = 255)
  dim(img_col) = dim(img_r)
  #display
  library(grid)
  grid.raster(img_col, interpolate=FALSE)
  remove(img, img_r, img_g, img_b, img_col)
  # plus label
  labels[[1]][mean_label_list[[index]]]
}

showImage(10)

#pca
SSE = rep(0, 10)
for (i in 1:numClass){
  #subsets by labels
  label = subset(images_mat_full,images_mat_full[,3073] == i)
  #get only the pixels
  label_px = label[,-c(3073)]
  pca_data = prcomp(label_px, center=TRUE)
  #reconstruct
  X_hat = pca_data$x[,1:20] %*% t(pca_data$rotation[,1:20])
  X_hat = scale(X_hat, center = -store_means[i,], scale = FALSE)
  error = mean(rowSums((label_px-X_hat)^2))
  SSE[i] = error
}

#barplot
barplot(SSE, main="SSE Plot Part A", col="lightblue",
        names.arg=c("airplane","automobile","bird","cat","deer","dog","frog","horse","ship","truck"))


########################################### Part B ###################################################################################


#cacl euclidean distance using dist function
euclidean = as.matrix(dist(store_means), method = "euclidean")
D_2 = euclidean*euclidean
#to a file
write.table(D_2,"partb_distances.csv", row.names=FALSE, col.names = FALSE, sep = ',')


#MDS part B
ones_mat = matrix(1, nrow = numClass, ncol = numClass)
I = diag(numClass)
A = I - 1/numClass * (ones_mat %*% t(ones_mat))
W = - 1/2 * A %*%  D_2  %*% t(A)
lamda = eigen(W)

eigen_values = sqrt(sort(lamda$values, decreasing=TRUE)[1:2])
#Construct U 
eigen_vectors = lamda$vectors[,1:2]
Y = eigen_vectors * eigen_values

colnames(Y) <- c("component1","component2")
rownames(Y) <- c("airplane","automobile","bird","cat","deer","dog","frog","horse","ship","truck")

plot(Y[, 1], Y[, 2], xlab = "Component 1", ylab = "Component 2", 
     col = "darkorange", pch = 16, cex = 2, main = "Scatter plot - part B" )

text(Y[, 1], Y[, 2], labels = rownames(Y), col = "blue", cex= 1, pos=3)

########################################### Part C #################################################################################
calcXhat = function(data_mat, eigen_vec, mean_vec, num_comp) {
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

D_c = matrix(0, nrow = numClass, ncol = numClass)

for (i in 1:numClass){
  mean_a = store_means[i,]
  #subsets by labels
  label = subset(images_mat_full,images_mat_full[,3073] == i)
  #get only the pixels
  data_a = label[,-c(3073)]
  pca_data_a = prcomp(data_a, center=TRUE)
  
  for (j in 1:numClass){
    mean_b = store_means[j,]
    label_b = subset(images_mat_full,images_mat_full[,3073] == j)
    #get only the pixels
    data_b = label_b[,-c(3073)]
    pca_data_b = prcomp(data_b, center=TRUE)
    X_hat_a = calcXhat(as.matrix(data_a), pca_data_b$rotation, mean_a, 20)
    error_a = mean(rowSums((data_a - X_hat_a)^2))
    X_hat_b = calcXhat(as.matrix(data_b), pca_data_a$rotation, mean_b, 20)
    error_b = mean(rowSums((data_b - X_hat_b)^2))
    
    D_c[i,j] = (error_a + error_b)/2
    
  }
  
}

write.table(D_c,"partc_distances.csv", row.names=FALSE, col.names = FALSE, sep = ',')


#MDS part C
#we have A already
W_c = - 1/2 * A %*%  D_c  %*% t(A)
lamda_c = eigen(W_c)

eigen_values_c = sqrt(sort(lamda_c$values, decreasing=TRUE)[1:2])
#Construct U 
eigen_vectors_c = lamda_c$vectors[,1:2]
Y_c = eigen_vectors_c * eigen_values_c

colnames(Y_c) <- c("component1","component2")
rownames(Y_c) <- c("airplane","automobile","bird","cat","deer","dog","frog","horse","ship","truck")

plot(Y_c[, 1], Y_c[, 2], xlab = "Component 1", ylab = "Component 2", 
     col = "green", pch = 16, cex = 2, main = "Scatter plot - part C" )

text(Y_c[, 1], Y_c[, 2], labels = rownames(Y_c), col = "blue", cex= 1, pos=3)

