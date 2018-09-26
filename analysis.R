source("history.R")
library(keras)
install_keras(tensorflow = "cpu", method = "conda", conda="/home/branko/miniconda3/bin/")

setwd("~/Documents/draftkings/")

# WEEKS_ID = "21505"

# Plotting individual
# plot(history[history$displayName == "Drew Brees","dk.points"])

# fit <- lm(dk.points ~ week + displayName + pos + teamYear + h.a + opptYear, data=history)
# summary(fit)

history <- getLatestHistory()
history <- enrichHistory(history)
write.csv(history,file = "history.csv")

head(history)
numPlayers <- length(unique(history$displayName)) + 1
numTeams <- length(unique(history$team)) + 1
numYears <- length(unique(history$year)) + 1
numHome <- length(unique(history$h.a)) + 1
numWeeks <- length(unique(history$week)) + 1

name_input <- layer_input(shape = c(1), dtype = "int32", name ="name_input")
team_input <- layer_input(shape = c(1), dtype = "int8", name="team_input")
opp_input <- layer_input(shape = c(1), dtype = "int8", name="opp_input")
year_input <- layer_input(shape = c(1), dtype = "int8", name="year_input")
home_input <- layer_input(shape = c(1), dtype = "int8", name="home_input")
week_input <- layer_input(shape = c(1), dtype = "int8", name="week_input")

name_embedding <- layer_embedding(input_dim = numPlayers, output_dim = 64)(name_input)
team_embedding <- layer_embedding(input_dim = numTeams, output_dim = 64)(team_input)
opp_embedding <- layer_embedding(input_dim = numTeams, output_dim = 64)(opp_input)
year_embedding <- layer_embedding(input_dim = numYears, output_dim = 64)(year_input)
home_embedding <- layer_embedding(input_dim = numHome, output_dim = 64)(home_input)
week_embedding <- layer_embedding(input_dim = numWeeks, output_dim = 64)(week_input)


x <- layer_concatenate(c(name_embedding, team_embedding, opp_embedding, year_embedding, home_embedding, week_embedding))
x <- layer_flatten(x)
x <- layer_dense(activation = "relu", units = 32)(x)
out <- layer_dense(units = 1)(x)

model <- keras_model(inputs = c(name_input, team_input, opp_input, year_input, home_input, week_input), out)
compile(model, optimizer = "rmsprop", loss="mse")
summary(model)

temp <- head(history)
predict_on_batch(model, 
    x=as.matrix(temp$displayName, 
            temp$team, 
            temp$oppt, 
            temp$year, 
            temp$h.a, 
            temp$week)
)


predict_on_batch(model, 
                 x=list(array(c(1)),array(2),array(3),array(4),array(5),array(6)))


as.matrix(c(1),c(2), c(3), c(4), c(5), c(6))
?matrix

?predict_on_batch

temp2<-as.matrix(x=c(temp$displayName, 
      temp$team, 
      temp$oppt, 
      temp$year, 
      temp$h.a, 
      temp$week))

dtype(temp2$week_input,)

#predict_on_batch(model, c(name_input=as.factor(temp$displayName), 
#                          team_input=as.factor(temp$team), 
#                          opp_input=as.factor(temp$oppt), 
#                          year_input=as.factor(temp$year), 
#                          home_input=as.factor(temp$h.a), 
#                          week_input=as.factor(temp$week)))


mnist <- dataset_mnist()
temp2<-mnist$train$x
shape(temp2)
