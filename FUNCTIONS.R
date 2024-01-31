## FUNCTIONS USED FOR PRODUCT DUPLICATE DETECTION ##

## FUNCTION TO MODIFY TITLE
custom_replace_title <- function(title) {
  title <- str_replace_all(title, 
                           c("-inches" = "inch", "-inch" = "inch", "-in" = "inch", 
                             "\\sin" = "inch", " inches" = "inch", 
                             "\"" = "inch", " in$" = "inch", "inchches" = "inch"))
  title <- str_replace_all(title, 
                           c("-hertz" = "hz", "-hz" = "hz", " hz$" = "hz", 
                             " hertz" = "hz", " hz" = "hz", "hertz" = "hz"))
  return(title)
}

## FUNCTION TO MODIFY FEATURES
custom_replace_features <- function(features) {
  features <- str_replace_all(features, 
                           c("-inches" = "inch", "-inch" = "inch", "-in" = "inch", 
                             "\\sin" = "inch", " inches" = "inch", 
                             "\"" = "inch"," in$" = "inch", "inchches" = "inch"))
  features <- str_replace_all(features, 
                           c("-hertz" = "hz", "-hz" = "hz", " hz$" = "hz", 
                             " hertz" = "hz", " hz" = "hz", "hertz" = "hz"))
  return(features)
}


## FUNCTION TO CREATE LIST OF KEYWORDS OF ALL TVS
getKeywords <- function(data, title_column, brands_column) {
  allTitleWords <- character()
  
  # Collect all words from the specified title column
  for (i in 1:length(data)) {
    titleWords <- strsplit(data[[i]][[title_column]], ' ')[[1]]
    allTitleWords <- union(allTitleWords, titleWords)
  }
  
  keywords <- character()
  
  # Filter the words that match the given expression
  for (word in allTitleWords) {
    if (grepl("[a-zA-Z0-9]*(([0-9]+[^0-9, ]+)|([^0-9, ]+[0-9]+))[a-zA-Z0-9]*", word)) {
      keywords <- c(keywords, word)
    }
  }

  # Extract and add unique brand names from "featuresMap"
  brandsWords <- unique(unlist(sapply(data, function(x) {
    brand <- x[["featuresMap"]][["Brand"]]
    if (is.character(brand)) {
      return(strsplit(brand, ' ')[[1]])
    } else {
      return(character())
    }
  })))
  keywords <- union(keywords, brandsWords)
  
  return(keywords)
}

## FUNCTION TO CREATE BINARY MATRICES FOR CHARACTERISTICS

# BINARY MATRIX FOR TITLES
createBinaryMatrixTitle <- function(data, tvsKeywords, titleColumn) {
  binaryMatrix <- matrix(0, nrow = length(tvsKeywords), ncol = length(data))
  
  for (i in seq_along(data)) {
    # Extract words from the title
    titleWords <- unlist(strsplit(data[[i]][[titleColumn]], ' '))
    
    # Assign 1 to matrix cells corresponding to present words
    binaryMatrix[, i] <- as.numeric(tvsKeywords %in% titleWords)
  }
  
  return(binaryMatrix)
}

# BINARY MATRIX FOR FEATURES
createBinaryMatrixFeatures <- function(data, tvs_model_words) {
  binaryMatrixFeatures <- matrix(0, nrow = length(tvs_model_words), ncol = length(data))
  
  for (i in seq_along(data)) {
    # Extract words from the features
    features_words <- unlist(strsplit(data[[i]]$features, ' '))
    
    # Assign 1 to matrix cells corresponding to present words
    binaryMatrixFeatures[, i] <- as.numeric(tvs_model_words %in% features_words)
  }
  
  return(binaryMatrixFeatures)
}


## FUNCTION TO CREATE MINHASH MATRIX AND SIGNATURE MATRIX
# Function to generate MinHash signatures
generateUniqueMinHashSignatures <- function(numberKeywords, numberHashFunctions) {
  uniqueMinHashMatrix <- matrix(data = Inf, nrow = numberKeywords, ncol = numberHashFunctions)
  
  for (i in 1:numberHashFunctions) {
    uniqueSeedA <- sample(1:numberKeywords, 1)
    uniqueSeedB <- sample(1:numberKeywords, 1)
    uniqueHashValues <- (uniqueSeedA * (1:numberKeywords) + uniqueSeedB) %% numberKeywords + 1
    uniqueMinHashMatrix[, i] <- pmin(uniqueMinHashMatrix[, i], uniqueHashValues)
  }
  
  return(uniqueMinHashMatrix)
}

# Function to update the Signature Matrix
computeSignatureMatrix <- function(binaryMatrix, minHashingMatrix) {
  numDocs <- ncol(binaryMatrix)
  numberHashFunctions <- ncol(minHashingMatrix)
  signatureMatrix <- matrix(data = 1e+5, nrow = numberHashFunctions, ncol = numDocs)
  
  for (j in 1:numDocs) {
    hashIndices <- which(binaryMatrix[, j] == 1)
    
    # Check if hashIndices is not empty
    if (length(hashIndices) > 0) {
      signatureMatrix[, j] <- apply(minHashingMatrix[hashIndices, , drop = FALSE], 2, min)
    }
  }
  
  return(signatureMatrix)
}


## FUNCTION TO SUM UP THE TRUE DUPLICATES FROM THE DATA
countTrueDuplicates <- function(data) {
  numberDuplicates <- sum(sapply(1:(length(data) - 1), function(i) {
    sum(sapply((i + 1):length(data), function(j) data[[i]][["modelID"]] == data[[j]][["modelID"]]))
  }))
  
  return(numberDuplicates)
}


## FUNCTION TO COMPUTE LSH CANDIDATES MATRIX
calculateLshCandidatesPairs <- function(numberBands, numberRows, signatureMatrix) {
  numItems <- ncol(signatureMatrix)
  
  # Initialize hash buckets matrix
  hashBuckets <- matrix(0, nrow = numberBands, ncol = numItems)
  
  # Calculate hash buckets for each band
  for (band in 1:numberBands) {
    hashBuckets[band, ] <- colSums(signatureMatrix[((band - 1) * numberRows + 1):(band * numberRows), ])
  }
  
  # Initialize candidate pair matrix
  candidatePairMatrix <- matrix(0, nrow = numItems, ncol = numItems)
  
  # Check for candidate pairs
  for (i in 1:(numItems - 1)) {
    for (j in (i + 1):numItems) {
      if (any(hashBuckets[, i] == hashBuckets[, j])) {
        candidatePairMatrix[i, j] <- 1
        candidatePairMatrix[j, i] <- 1
      }
    }
  }
  
  return(candidatePairMatrix)
}



## FUNCTION TO COMPUTE CORRECT PREDICTIONS
computeTruePositives <- function(candidatePairsMatrix, data) {
  indices <- which(candidatePairsMatrix == 1, arr.ind = TRUE, useNames = FALSE)
  upperTriangleIndices <- indices[indices[, 1] < indices[, 2], ]
  
  modelIDs <- sapply(1:nrow(upperTriangleIndices), function(i) {
    idx1 <- upperTriangleIndices[i, 1]
    idx2 <- upperTriangleIndices[i, 2]
    c(data[[idx1]][["modelID"]], data[[idx2]][["modelID"]])
  })
  
  correctPredictionsCount <- sum(modelIDs[1, ] == modelIDs[2, ])
  
  return(correctPredictionsCount)
}

## FUNCTION TO COMPUTE FALSE POSITIVES
computeFalsePositives <- function(candidatePairsMatrix, correctPredictionsCount) {
  indices <- which(candidatePairsMatrix == 1, arr.ind = TRUE, useNames = FALSE)
  upperTriangleIndices <- indices[indices[, 1] < indices[, 2], ]
  
  falsePositivesCount <- nrow(upperTriangleIndices) - correctPredictionsCount
  
  return(falsePositivesCount)
}

## FUNCTION TO CREATE SIMILARITY MATRIX USING JACCARD 
computeJaccardSimilarity <- function(binaryMatrix, i, j) {
  intersection <- sum(((binaryMatrix[, i] == 1) & (binaryMatrix[, j] == 1)))
  union <- sum(binaryMatrix[, i]) + sum(binaryMatrix[, j]) - intersection
  return (1 - (intersection / union))
}

# Function to initialize and fill distance matrix using Jaccard Similarity
calculateSimilarityMatrix <- function(binaryMatrix, candidatePairMatrix) {
  numberItems <- ncol(binaryMatrix)
  similarityMatrix <- matrix(data = NA, nrow = numberItems, ncol = numberItems)
  
  for (i in 1:(numberItems - 1)) {
    for (j in (i + 1):numberItems) {
      if (candidatePairMatrix[i, j] == 0) {
        similarityMatrix[i, j] <- 1e+5
        similarityMatrix[j, i] <- 1e+5
      } else {
        similarityMatrix[i, j] <- computeJaccardSimilarity(binaryMatrix, i, j)
        similarityMatrix[j, i] <- similarityMatrix[i, j]
      }
    }
  }
  
  return(as.dist(similarityMatrix))
}

## CLUSTERING METHODS
# Function to perform K-Means clustering
performKMeansClustering <- function(similarityMatrix, numClusters) {
  kmeansResult <- kmeans(similarityMatrix, centers = numClusters)
  return(kmeansResult$cluster)
}

# Function to extract clusters from the K-Means clustering results
extractKMeansClusters <- function(kmeansClusters) {
  uniqueClusters <- unique(kmeansClusters)
  clusterList <- split(which(kmeansClusters %in% uniqueClusters), kmeansClusters)
  clusterList <- lapply(clusterList, as.vector)
  return(clusterList)
}

# Function to perform hierarchical clustering
performHierarchicalClustering <- function(similarityMatrix, threshold) {
  clusters <- hclust(similarityMatrix, method = "complete")
  return(cutree(clusters, h = threshold))
}

# Function to extract clusters from the hierarchical clustering results
extractHierarchicalClusters <- function(clusters) {
  uniqueClusters <- unique(clusters)
  clusterList <- split(which(clusters %in% uniqueClusters), clusters)
  clusterList <- lapply(clusterList, as.vector)
  return(clusterList)
}

# Function to evaluate clustering results for both K-Means and Hierarchical methods
evaluateBothClusterings <- function(similarityMatrix, numberTrueDuplicates, numClustersKMeans, thresholdHierarchical) {
  # Perform K-Means clustering
  kmeansClusters <- performKMeansClustering(similarityMatrix, numClusters = numClustersKMeans)
  kmeansClusterList <- extractKMeansClusters(kmeansClusters)
  
  # Evaluate K-Means clustering
  resultsKMeans <- evaluateClusters(kmeansClusterList, data, numberTrueDuplicates)
  
  # Perform hierarchical clustering
  hierarchicalClusters <- performHierarchicalClustering(similarityMatrix, threshold = thresholdHierarchical)
  hierarchicalClusterList <- extractHierarchicalClusters(hierarchicalClusters)
  
  # Evaluate hierarchical clustering
  resultsHierarchical <- evaluateClusters(hierarchicalClusterList, data, numberTrueDuplicates)
  
  return(list(kmeans = resultsKMeans, hierarchical = resultsHierarchical))
}

# Function to evaluate clustering results for a single clustering method
evaluateClusters <- function(clusters, data, numberTrueDuplicates) {
  # Identify pairs with the same modelID
  sameModelIDPairs <- combn(unlist(clusters[sapply(clusters, length) > 1]), 2)
  
  # Compute true positives
  truePositives <- computeTruePositivesCluster(sameModelIDPairs, data)
  
  # Compute false positives
  falsePositives <- computeFalsePositivesCluster(sameModelIDPairs, truePositives)
  
  # Calculate precision, recall, and F-measure
  precision <- truePositives / (truePositives + falsePositives)
  recall <- truePositives / numberTrueDuplicates
  Fmeasure <- (2 * precision * recall) / (precision + recall)
  
  return(c(precision, recall, Fmeasure))
}

# Function to compute true positives
computeTruePositivesCluster <- function(sameModelIDPairs, data) {
  modelIDs1 <- sapply(1:ncol(sameModelIDPairs), function(i) data[[sameModelIDPairs[1, i]]][["modelID"]])
  modelIDs2 <- sapply(1:ncol(sameModelIDPairs), function(i) data[[sameModelIDPairs[2, i]]][["modelID"]])
  
  truePositivesCount <- sum(modelIDs1 == modelIDs2)
  
  return(truePositivesCount)
}

# Function to compute false positives
computeFalsePositivesCluster <- function(sameModelIDPairs, truePositives) {
  falsePositivesCount <- ncol(sameModelIDPairs) - truePositives
  
  return(falsePositivesCount)
}
