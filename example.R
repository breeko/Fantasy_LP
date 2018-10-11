source("history.R")
source("solve.R")
source("available.R")

# Regression Example

# Constants
WEEKS_ID <- 22018
WEEK <- 5
YEAR <- 2018

# Get and enrich history
h <- getLatestHistory()
h <- updateHistory(h)
h <- enrichHistory(h)

# Filter relevant players
h2016 <- h[(as.numeric(as.character(h$year)) >= 2016) & (h$games > 3),]

# Break up to training and test set
trainingRowIndex <- sample(1:nrow(h2016), 0.8*nrow(h2016))
trainingData <- h2016[trainingRowIndex, ]
testData  <- h2016[-trainingRowIndex, ]

# Fit the regression
fit <- lm(dk.points ~ displayName + team + year + h.a + week + pos, data = trainingData)

# Review regression
summary(fit)

# Calculat MSE
mean((predict(fit, trainingData) - trainingData$dk.points) ^ 2)
mean((predict(fit, testData) - testData$dk.points) ^ 2)

# Get and transform available players
a <- getLatestAvailable(WEEKS_ID)
ah <- availableToHistory(a, WEEK, YEAR, history=h2016)

# Filter out players the model has never seen
ahFiltered <- ah[(ah$displayName %in% h2016$displayName),]

# Predict performance
ahFiltered$prediction <- predict(fit, ahFiltered)

# Solve for optimal team based on predictions
solve(ahFiltered)

