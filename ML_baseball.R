setwd("~/ECON203B Project")
library(tidyverse)
library(stargazer)
library(plm)
library(baseballr)
library(AER)
library(openintro)
library(ggpubr)
library(gamlr)
library(ROCR)
rm(list = ls())

df <- mlb_teams
df <- subset(df, year == '2019')

df <-  df %>%
  mutate(division_winner = ifelse(division_winner=='N',0,1))%>%
  mutate(wild_card_winner = ifelse(wild_card_winner=='N',0,1))%>%
  mutate(league_winner = ifelse(league_winner=='N',0,1))%>%
  mutate(world_series_winner = ifelse(world_series_winner=='N',0,1))
df <- subset(df, select=-c(ball_park))
df$team_name <- factor(df$team_name)

div <- subset(df, select=-c(wild_card_winner, league_winner, world_series_winner, 
                            home_attendance, rank, wins, losses, home_games, 
                            games_played, rank, division_id, league_id))
wc <- subset(df, select=-c(division_winner, league_winner, world_series_winner, 
                            home_attendance, rank, wins, losses, home_games, 
                            games_played, rank, division_id, league_id))
league <- subset(df, select=-c(division_winner, wild_card_winner, world_series_winner, 
                           home_attendance, rank, wins, losses, home_games, 
                           games_played, rank, division_id, league_id))
ws <- subset(df, select=-c(division_winner, wild_card_winner, league_winner, 
                               home_attendance, rank, wins, losses, home_games, 
                               games_played, rank, division_id, league_id))
is.na(div)
data_omit <- na.omit(div)

data_omit_seach_PF <- subset(div, select = -c(13:22))
data_omit_seach_PF <- naref(data_omit_seach_PF)

cv.X <- sparse.model.matrix(division_winner ~ ., data = data_omit_seach_PF)[,-1]
set.seed(0)
cv.lasso_model_10 <- cv.gamlr(cv.X, data_omit_seach_PF$division_winner, family="binomial", nfold = 10)

plot(cv.lasso_model_10)
plot(cv.lasso_model_10$gamlr, main = "Lasso Regularization path")

cv.coef <- coef(cv.lasso_model_10, select="min")
cv.coef[which(cv.coef!=0)]

pred <- drop(predict(cv.lasso_model_10$gamlr, cv.X, type = "response"))
hist(pred, xlab="Winning", ylab="Probability of Winning")

boxplot(pred ~ data_omit_seach_PF$division_winner, xlab="Winning", ylab="Probability of Winning", col=c("blue","red"))

source("~/ECON203B Project/roc.R")
par(mai=c(.9,.9,.2,.1)) # format margins
roc(pred, data_omit_seach_PF$division_winner, bty="n", main="IS ROC") # from roc.R

points(x=1-mean((pred<=.02)[data_omit_seach_PF$division_winner==0]), y=mean((pred>.02)[data_omit_seach_PF$division_winner==1]), cex=1.5, pch=20, col='red') 
points(x=1-mean((pred<=.1)[data_omit_seach_PF$division_winner==0]), y=mean((pred>.1)[data_omit_seach_PF$division_winner==1]), cex=1.5, pch=20, col='blue') 
points(x=1-mean((pred<=0.33)[data_omit_seach_PF$division_winner==0]), y=mean((pred>0.33)[data_omit_seach_PF$division_winner==1]), cex=1.5, pch=20, col='green') 
points(x=1-mean((pred<=0.8)[data_omit_seach_PF$division_winner==0]), y=mean((pred>0.8)[data_omit_seach_PF$division_winner==1]), cex=1.5, pch=20, col='yellow')
points(x=1-mean((pred<=0.9)[data_omit_seach_PF$division_winner==0]), y=mean((pred>0.9)[data_omit_seach_PF$division_winner==1]), cex=1.5, pch=20, col='purple')

legend("bottomright",fill=c("red","blue", "green", "yellow", "purple"), legend=c("p=0.02","p=0.1", "p=0.33", "p=0.8", "p=0.9"),bty="n",title="cutoffs")

set.seed(0)
test <- sample.int(1000,500)
lasso_model_half <- gamlr(cv.X[-test,], data_omit_seach_PF$division_winner[-test], family = "binomial")
pred_oos <- predict(lasso_model_half, cv.X[-test,], type="response")
Y_oos <- data_omit_seach_PF$division_winner[test]
roc(pred_oos, Y_oos, bty="n", main="OOS ROC")

points(x=1-mean((pred_oos<=.02)[Y_oos==0]), y=mean((pred_oos>.02)[Y_oos==1]), cex=1.5, pch=20, col='red') 
points(x=1-mean((pred_oos<=.1)[Y_oos==0]), y=mean((pred_oos>.1)[Y_oos==1]), cex=1.5, pch=20, col='blue') 
points(x=1-mean((pred_oos<=0.33)[Y_oos==0]), y=mean((pred_oos>0.33)[Y_oos==1]), cex=1.5, pch=20, col='green') 
points(x=1-mean((pred_oos<=0.8)[Y_oos==0]), y=mean((pred_oos>0.8)[Y_oos==1]), cex=1.5, pch=20, col='yellow')
points(x=1-mean((pred_oos<=0.9)[Y_oos==0]), y=mean((pred_oos>0.9)[Y_oos==1]), cex=1.5, pch=20, col='purple')
legend("bottomright",fill=c("red","blue", "green", "yellow", "purple"), legend=c("p=0.02","p=0.1", "p=0.33", "p=0.8", "p=0.9"),bty="n",title="cutoffs")

