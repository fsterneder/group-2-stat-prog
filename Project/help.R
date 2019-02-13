library(tidyverse)
library(lubridate)
library(zeallot)
library(modelr)
library(stargazer)
library(plotly)
library(htmltools)

#### helper functions ####

#this file contains: 
  # data setup
  # teamcolor definitions
  # functions
    # wghtWAR
    # own linear model
    # predict surface 

#data setup
setup <- list()

#File Load for all the files used in the analysis
setup$files <- c("Appearances", "People", "Teams", "warPit",
                 "warPos", "Salaries", "Salaries_US")

##read all files from setup$files
setup$data %<-% 
  (map(setup$files, function(f){
    return (read_csv(
      #"data" is the underfolder where our files are stored
      paste("./data/",f,".csv",sep=""), col_names=T)
    )})
  )

##assigns setup$data files the names of setup$files
walk2(setup$files, setup$data, assign,envir=globalenv())


#COLOR 
#here are the team colors are stored for the plots

color <- list()

color$team_main <- c("#A71930","#13274F","#DF4601","#BD3039","#0E3386","#27251F","#C6011F","#0C2340","#33006F","#0C2340","#002D62","#004687","#862633","#005A9C","#000000","#B6922E","#002B5C","#002D72","#0C2340","#003831","#E81828","#FDB827","#C41E3A","#002D62","#FD5A1E","#005C5C","#092C5C","#003278","#134A8E","#AB0003")
color$team_second <- c("#E3D4AD","#CE1141","#000000","#0C2340","#CC3433","#C4CED4","#000000","#E31937","#C4CED4","#FA4616","#EB6E1F","#BD9B60","#003263","#EF3E42","#00A3E0","#0A2351","#D31145","#FF5910","#0C2340","#EFB21E","#002D72","#27251F","#0C2340","#A2AAAD","#27251F","#0C2C56","#F5D130","#C0111F","#E8291C","#14225A")


#wghtWAR - weighted Wins Above Replacement
  #this function computes the weighted War, because for WAR of 2016, some players
  #have more than one entry. Therefore they are weighted per player by the 
  #proportion of games the player played in this team. 
  #this function will be applied to the WAR tables merged with the appearance 
  #table per player

wghtWAR <- function(df){
  #NAs are dropped in the df
  df <- drop_na(df)
  #if WAR is not numeric, it now is (and saved as WAR in the function):
  WAR <- as.numeric(df[["WAR"]])
  #the total number of games is counted (from Appearance table)
  games <- sum(df[["G_all"]], na.rm=T)
  #the number of teams the player played in, is equal to the number of rows
  nTeams <- nrow(df)
  #we create an empty (filled with NAs) war column
  war <- rep(NA, nTeams)
  #then we compute the proportional war and then sum it up 
  for (i in seq_along(war)){
    war[i] <- (df[["G_all"]][i]/games)*WAR[i]
  }
  sum(war)
}


#lm_own
  #this function constructs a linear model, and returns the plot 
  #(plot = T by default, otherwise it returns the linear model dataframe)

lm_own <- function(df, y1, x1, txt_y, txt_x, txt_title, plot = T) {
#input variables are the dataframe, x and y and the labs for x, y and the title
  #the formula for the model has to be done seperately otherwise the lm would 
  #either take the names "y1" and "x1" without finding columns
  #or find the columns df[[y1]] and df[[x1]] but using y1 and x1 also as names 
  f <- as.formula(paste(y1, x1, sep = "~"))
  #constructing the lm
  lm_df <- lm(formula = f, data = df)
  
  if(plot == T) {
    #adding the predictions and residuals
    df <- df %>%
      add_predictions(lm_df) %>%
      add_residuals(lm_df)
    
    #plotting the data
    lm_df_plot <- df %>%
      #refering to the columns with df[[x|y1]]
      ggplot(aes(df[[x1]])) +
      geom_point(aes(y=df[[y1]])) +
      #adding the prediction line
      geom_line(aes(y=pred), color = "#3068A1") +
      #naming the axes and title
      labs(x = txt_x, y = txt_y, title = txt_title)}
  #if plot is not T, it returns the linear model 
  else{return(lm_df)}
}

#there is also a safe(ly) version of the lm_own
safe_lm_own <- safely(lm_own)


#Predict Surface
predictSurface <- function(x1, x2, model) {
  predict(model, data.frame(age=x1, salary=x2))
}