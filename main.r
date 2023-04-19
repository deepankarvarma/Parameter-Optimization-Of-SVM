#Library Downloading and Inclusion
#install.packages("kernlab")
library(kernlab)

#Read the CSV file
df <- read.csv("C:\\Users\\acer\\Desktop\\Thapar\\Subjects\\6th Semester\\UCS654 PREDICTIVE ANALYTICS USING STATISTICS\\PS Rana Ass\\Parameter Optimization Using SVM\\Dry_Bean_Dataset.csv",nrows=10000)
df <- subset(df, select = c("Area", "Perimeter", "MajorAxisLength", "MinorAxisLength", "AspectRation", "Eccentricity", "ConvexArea", "Solidity", "Class"))

head(df)

# Convert the Class column to a factor
df$Class <- factor(df$Class, levels = c("SEKER", "BARBUNYA", "BOMBAY", "CALI","DERMASON","HOROZ","SIRA"))

# Map the factor levels to numeric values
df$Class <- as.numeric(df$Class)

# Initialize empty vectors to store iteration numbers and accuracies
iter_vec <- c()
accuracy_vec <- c()
# Create an empty data frame to store the results
results_df <- data.frame(sample = integer(),
                          accuracy = numeric(),
                          kernel = character(),
                          nu = numeric(),
                          epsilon = numeric())

# Run the iterations on 10 samples
for(sample_num in 1:10){
  #Variables Declaration
  bestAccuracy=0
  bestKernel=""
  bestNu=0
  bestEpsilon=0
  iteration=10
  kernelList=c('rbfdot','polydot','vanilladot','tanhdot','laplacedot','anovadot')
  # Split the data into 70:30 train-test split
  trainSize <- floor(nrow(df)*0.7)
  trainIndex <- sample(seq_len(nrow(df)), size = trainSize)
  trainDataset <- df[trainIndex,]
  testDataset <- df[-trainIndex,]
  ncol(trainDataset)
  nrow(trainDataset)
  
  # Extract the formula for model training
  X <- as.matrix(trainDataset[, 1:8])
  Y <- as.factor(trainDataset$Class)
  
  # Run the iteration loop for each sample
  for(i in 1:iteration){
    print(paste0("Sample: ", sample_num, ", Iteration: ", i))
    k <- sample(kernelList, 1)
    n <- runif(1)
    e <- runif(1)
    Accuracy <- fitnessFunction(k, n, e)
        
    # Update best parameters if accuracy is higher than previous best
    if(Accuracy > bestAccuracy){
      bestKernel <- k
      bestNu <- n
      bestEpsilon <- e
      bestAccuracy <- Accuracy
    }
    bestAccuracy <- max(Accuracy, bestAccuracy)
    
    # Append iteration number and accuracy to the vectors
    iter_vec <- c(iter_vec, i + (sample_num-1)*iteration)
    accuracy_vec <- c(accuracy_vec, bestAccuracy)
  }
  # Add a new row to the results data frame
    new_row <- data.frame(sample = sample_num, 
                          accuracy = bestAccuracy, 
                          kernel = bestKernel, 
                          nu = bestNu, 
                          epsilon = bestEpsilon)
    results_df <- rbind(results_df, new_row)
}
write.csv(results_df, file = "results.csv", row.names = FALSE)

# Find the sample with maximum accuracy
max_sample <- which.max(results_df$accuracy)

# Extract the iteration and accuracy vectors for the sample with maximum accuracy
max_iter_vec <- iter_vec[iter_vec > (max_sample - 1)*iteration & iter_vec <= max_sample*iteration]
max_accuracy_vec <- accuracy_vec[iter_vec > (max_sample - 1)*iteration & iter_vec <= max_sample*iteration]

# Plot the convergence graph
plot(max_iter_vec, max_accuracy_vec, type = "l", xlab = "Iteration", ylab = "Accuracy")

