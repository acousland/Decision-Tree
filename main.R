#########################################################
# Decision Tree Implementation (Recursive Partitioning) 
#
# Grow a decision tree and use those results
# to identify fault conditions
# 
# - Aaron Cousland 13/05/2015
########################################################

require (RODBC)       # SQL connectivity
require (rpart)       # Decision free functions
require (rpart.plot)  # Plotting decision trees
require (rattle)      # More plotting tools
require (dplyr)       # Required for performance measurement

# Close any existing connections
odbcCloseAll()

# Create a connection to the database called
local.connection <- odbcConnect("RTV", believeNRows=FALSE)

# Query the database and put the results into the data frame logging.results
logger.results <- sqlQuery(local.connection, 
                                    "SELECT * FROM ELSPEC.RMS_TRAINING where ts between '17/MAR/15 09:00:00 AM' and '17/MAR/15 10:00:00 PM';")

# Create Training Dataset
StartTime <- force_tz(as.POSIXct("2015-03-17 09:00:00", format = "%Y-%m-%d %H:%M:%OS"),"AEST")
EndTime <- force_tz(as.POSIXct("2015-03-17 10:00:00", format = "%Y-%m-%d %H:%M:%OS"),"AEST")
logger.results.training <- subset(logger.results, logger.results$TS >= StartTime & logger.results$TS <= EndTime)

# Set up relational formula
tree.formula <- "logger.results.training$FAULT ~ (logger.results.training$RMSI1 + logger.results.training$RMSI3 + logger.results.training$RMSVN)"  

# Grow tree
tree.model <- rpart(tree.formula, data=logger.results.training, method="anova")

# Prune tree to stop over fit
tree.model$cptable[which.min(tree.model$cptable[,"xerror"]),"CP"]

# Plot a picture of the tree
fancyRpartPlot(tree.model)

# Apply decision tree results
logger.results$PrFault <- ifelse(logger.results$RMSI1<0.16,0,1)

# Measure Performance
performance <- logger.results %>%
  group_by(FAULT) %>%
  summarise (Score = sum(PrFault))

# Display Performance
print(paste("Score =",performance$Score[2]-performance$Score[1],"/",sum(logger.results$FAULT==TRUE)))
