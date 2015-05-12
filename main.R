require (RODBC)
require (rpart)

Close any existing connections
odbcCloseAll()

# Create a connection to the database called
local.connection <- odbcConnect("RTV", believeNRows=FALSE)

# Query the database and put the results into the data frame logging.results
logger.results.training <- sqlQuery(local.connection, 
                                    "SELECT * FROM ELSPEC.RMS_TRAINING where ts between '17/MAR/15 09:00:00 AM' and '17/MAR/15 11:00:00 PM';")

# Set up relational formula
tree.formula <- "logger.results.training$FAULT ~ (logger.results.training$RMSI1 + logger.results.training$RMSI3 + logger.results.training$RMSVN)"  

# Grow tree
tree.model <- rpart(tree.formula, data=logger.results.training, method="anova")

# prune tree to stop over fit
tree.model$cptable[which.min(tree.model$cptable[,"xerror"]),"CP"]

plot(tree.model)
