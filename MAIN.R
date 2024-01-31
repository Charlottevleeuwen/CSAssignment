## PRODUCT DUPLICATE DETECTION ##

## LOAD PACKAGES
install.packages("rjson")
install.packages("stringi")
install.packages("stringr")
install.packages("ggplot2")
install.packages("cluster")
library(rjson)
library(stringi)
library(stringr)
library(ggplot2)
library(cluster)

## LOAD DATA
data <- unlist(fromJSON(file = "~/Downloads/TVs-all-merged.json"), recursive = FALSE)


## CLEAN DATA AND PREPROCESS
for (i in 1:length(data)) {
  # Process title
  data[[i]][["title"]] <- custom_replace_title(tolower(data[[i]][["title"]]))
  
  # Process features
  features <- stri_join(unlist(data[[i]][["featuresMap"]]), collapse = " ")
  data[[i]][["features"]] <- custom_replace_features(tolower(features))
}


## USE SEED FOR REPRODUCIBILITY
set.seed(123)


## OBTAIN A LIST OF CHARACTERISTICS/MODEL WORDS OF ALL TVS
tvsKeywords <- getKeywords(data, "title", "Brand")
#print(tvsKeywords)


## CONSTRUCT BINARY MATRICES FOR CHARACTERISTICS
binaryMatrixTitles <- createBinaryMatrixTitle(data, tvsKeywords, "title")
binaryMatrixFeatures <- createBinaryMatrixFeatures(data, tvsKeywords)
#print(binaryMatrixTitles)
#print(binaryMatrixFeatures)

binaryMatrix <- binaryMatrixTitles #transform to continue coding easier
#binaryMatrix <- binaryMatrixFeatures


## CONSTRUCT MINHASH MATRIX AND SIGNATURE MATRIX
numberKeywords <- length(tvsKeywords)  
numberHashFunctions <- 680    # approx nrows(binarymatrix)/2
initialValue <- 1e+5

# Generate MinHash signatures
minHashingMatrix <- generateUniqueMinHashSignatures(numberKeywords, numberHashFunctions)

# Compute Signature Matrix
signatureMatrix <- computeSignatureMatrix(binaryMatrix, minHashingMatrix)


## LOCALITY-SENSITIVE HASHING
# Create a list to store LSH results
lshResults <- list()

# Define different numbers of rows to iterate over
numberRows <- c(2, 4, 6, 8, 12, 15, 20, 25, 30, 35, 40)

# Find the number of true duplicates 
numberTrueDuplicates <- countTrueDuplicates(data)
print(numberTrueDuplicates)

# Iterate over different numbers of rows
for (l in 1:length(numberRows)) {
  numRows <- numberRows[l]
  numBands <- numberHashFunctions / numRows
  
  candidatePairMatrix <- calculateLshCandidatesPairs(numBands, numRows, signatureMatrix)
  
  # Compute correct predictions 
  truePositives <- computeTruePositives(candidatePairMatrix, data)
  # Compute False Positives
  falsePositives <- computeFalsePositives(candidatePairMatrix, truePositives)
  
  # Calculate PQ, PC, and F-star scores 
  PQ <- truePositives / (truePositives + falsePositives)
  PC <- truePositives / numberTrueDuplicates
  Fstar <- (2 * PQ * PC) / (PQ + PC)
  
  # Calculate ratio
  ratio <- (truePositives + falsePositives) / (((length(data)) * (length(data) - 1)) / 2)
  
  # Store the LSH results in list
  lshResults[[as.character(numRows)]] <- list(
    numberRows = numRows,
    numberBands = numBands,
    ratio = ratio,
    PC = PC,
    PQ = PQ,
    Fstar = Fstar
  )
}

#print(lshResults)


## CREATE SIMILARITY MATRIX USING JACCARD
similarityMatrix <- calculateSimilarityMatrix(binaryMatrix, candidatePairMatrix)

## CLUSTERING METHODS
numClustersKMeans <- 500   # Aanpassen naar gewenst aantal clusters voor K-Means
thresholdHierarchical <- 0.40  # Aanpassen naar gewenste waarde voor hiërarchische clustering

evaluationResults <- evaluateBothClusterings(similarityMatrix, numberTrueDuplicates, numClustersKMeans, thresholdHierarchical)



##### USE BOOTSTRAPS ON THE CODE #####
# Set the number of bootstraps
numBootstraps <- 10

# Initialize a list to store the LSH and clustering results for each bootstrap
bootstrapResults <- vector("list", length = numBootstraps)

# Loop over each bootstrap
for (bootstrap in 1:numBootstraps) {
  # Sample indices with replacement for training (2/3 of the data)
  trainIndices <- sample(1:length(data), size = 2/3 * length(data), replace = TRUE)
  
  # Use the remaining indices for testing (1/3 of the data)
  testIndices <- setdiff(1:length(data), trainIndices)
  
  # Split the data into training and testing sets
  trainingData <- data[trainIndices]
  testingData <- data[testIndices]
  
  ## ... Insert code for preprocessing, binary matrix, minhashing, LSH, and similarity matrix ...
  ## Perform LSH and clustering on the training data
  # ... Insert LSH and clustering code ...
  
  ## Calculate similarity matrix using Jaccard for testing data
  testingBinaryMatrix <- createBinaryMatrixTitle(testingData, tvsKeywords, "title")
  testingCandidatePairMatrix <- calculateLshCandidatesPairs(numBands, numRows, computeSignatureMatrix(testingBinaryMatrix, minHashingMatrix))
  testingSimilarityMatrix <- calculateSimilarityMatrix(testingBinaryMatrix, testingCandidatePairMatrix)
  
  ## Evaluate clustering on the testing data
  numClustersKMeans <- 500  # Adjust based on your requirements
  thresholdHierarchical <- 0.40  # Adjust based on your requirements
  testingEvaluationResults <- evaluateBothClusterings(testingSimilarityMatrix, numberTrueDuplicates, numClustersKMeans, thresholdHierarchical)
  
  # Store the LSH and clustering results for this bootstrap
  bootstrapResults[[bootstrap]] <- list(lsh = lshResults, clustering = testingEvaluationResults)
}

# Average the LSH and clustering results over all bootstraps
finalLshResults <- colMeans(do.call(cbind, lapply(bootstrapResults, function(x) x$lsh)))
finalClusteringResults <- colMeans(do.call(cbind, lapply(bootstrapResults, function(x) x$clustering)))

# Print or return the final LSH and clustering results
print(finalLshResults)
print(finalClusteringResults)


# Extract data for plotting
fraction_of_comparisons <- sapply(lshResults, function(res) res$ratio)
PQ <- sapply(lshResults, function(res) res$PQ)
PQf <- sapply(lshResultsFeatures, function(res) res$PQ)
PC <- sapply(lshResults, function(res) res$PC)
PCf <- sapply(lshResultsFeatures, function(res) res$PC)
F1 <- sapply(lshResults, function(res) res$Fstar)
F1f <- sapply(lshResultsFeatures, function(res) res$Fstar)

# Create a data frame for plotting
plot_data <- data.frame(
  Fraction_of_Comparisons = fraction_of_comparisons,
  PQ = PQ,
  PQf = PQf,
  PC = PC,
  PCf = PCf,
  F1 = F1,
  F1f = F1f
)

# Plot PQ and PQf
ggplot(plot_data, aes(x = Fraction_of_Comparisons)) +
  geom_line(aes(y = PQ, color = "Without Keywords"), linetype = "solid") +
  geom_line(aes(y = PQf, color = "With Keywords"), linetype = "dashed") +
  labs(x = "Fraction of Comparisons",
       y = "Pair Quality (PQ)") +
  scale_color_manual(values = c("Without Keywords" = "blue", "With Keywords" = "red"),
                     name = "") +  # Remove legend title
  theme(legend.position = "top")  # Set legend position to the top

# Plot PC and PCf
ggplot(plot_data, aes(x = Fraction_of_Comparisons)) +
  geom_line(aes(y = PC, color = "Without Keywords"), linetype = "solid") +
  geom_line(aes(y = PCf, color = "With Keywords"), linetype = "dashed") +
  labs(x = "Fraction of Comparisons",
       y = "Pair Completeness (PC)") +
  scale_color_manual(values = c("Without Keywords" = "blue", "With Keywords" = "red"),
                     name = "") +  # Remove legend title
  theme(legend.position = "top")  # Set legend position to the top

# Plot F1 and F1f
ggplot(plot_data, aes(x = Fraction_of_Comparisons)) +
  geom_line(aes(y = F1, color = "Without Keywords"), linetype = "solid") +
  geom_line(aes(y = F1f, color = "With Keywords"), linetype = "dashed") +
  labs(x = "Fraction of Comparisons",
       y = "F1*-measure") +
  scale_color_manual(values = c("Without Keywords" = "blue", "With Keywords" = "red"),
                     name = "") +  # Remove legend title
  theme(legend.position = "top")  # Set legend position to the top
