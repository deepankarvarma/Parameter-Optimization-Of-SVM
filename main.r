  #Code by Deepankar Varma(102003431),3CO17
  #Library Downloading and Inclusion
  #install.packages("kernlab")
  library(kernlab)
  df <- read.csv("C:\\Users\\acer\\Desktop\\Thapar\\Subjects\\6th Semester\\UCS654 PREDICTIVE ANALYTICS USING STATISTICS\\PS Rana Ass\\Parameter Optimization Using SVM\\Dry_Bean_Dataset.csv")
  # Convert the Class column to a factor
  df$Class <- factor(df$Class, levels = c("SEKER", "BARBUNYA", "BOMBAY", "CALI","DERMASON","HOROZ","SIRA"))
  
  # Map the factor levels to numeric values
  df$Class <- as.numeric(df$Class)
  df$Class
  
  #Variables Declaration
  bestAccuracy=0
  bestKernel=""
  bestNu=0
  bestEpsilon=0
  iteration=100
  kernelList=c('rbfdot','polydot','vanilladot','tanhdot','laplacedot','anovadot')
  # Split the data into 70:30 train-test split
  trainSize <- floor(nrow(df)*0.7)
  trainIndex <- sample(seq_len(nrow(df)), size = trainSize)
  trainDataset <- df[trainIndex,]
  testDataset <- df[-trainIndex,]
  ncol(trainDataset)
  nrow(trainDataset)
  # Extract the formula for model training
  X <- as.matrix(trainDataset[, 1:16])
  Y <- as.factor(trainDataset$Class)
  
  #Fitness Function Definition
  fitnessFunction<-function(k,n,e){
    #k stands for Kernel,n for Nu, e for Epsilon
    #Building the model
    model<-ksvm(X, Y, kernel=k, nu=n, epsilon=e,kpar=list())
    #Prediction of Testing Dataset
    predicted<-predict(model,testDataset[, -1])
    #Model Evaluation Accuracy
    accuracy<-round(mean(as.numeric(testDataset$Class==predicted))*100,2)
    return(accuracy)  
  }
  # Initialize empty vectors to store iteration numbers and accuracies
  iter_vec <- c()
  accuracy_vec <- c()
  
  #Run the iterations
  for(i in 1:iteration){
    k <- sample(kernelList, 1)
    n <- runif(1)
    e <- runif(1)
    Accuracy <- fitnessFunction(k, n, e)
    if(Accuracy > bestAccuracy){
      bestKernel <- k
      bestNu <- n
      bestEpsilon <- e
      bestAccuracy <- Accuracy
    }
    # Append iteration number and accuracy to the vectors
    iter_vec <- c(iter_vec, i)
    accuracy_vec <- c(accuracy_vec, bestAccuracy)
  
  
  }
  print(bestAccuracy)
  print(bestNu)
  print(bestEpsilon)
  print(bestKernel)
  # Plot the convergence graph
  # plot(iter_vec, accuracy_vec, type = "l", xlab = "Iteration", ylab = "Accuracy")
