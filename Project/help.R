library(tidyverse)
library(lubridate)
library(zeallot)

#### helper functions ####

setup <- list()

# CSV LOAD
setup$files <- c("AllstarFull","Appearances","AwardsManagers","AwardsPlayers","AwardsShareManagers","AwardsSharePlayers","CollegePlaying","HallOfFame","Managers","ManagersHalf","People","Teams")

## read all files from setup$files
setup$data %<-% 
  (map(setup$files, function(f){
    return (read_csv(
      paste("./data/",f,".csv",sep=""), col_names=T)
    )})
  )

## assigns setup$data files the names of setup$files
walk2(setup$files, setup$data, assign,envir=globalenv())

# COLOR

color <- list()

color$team_main <- c("#A71930","#13274F","#DF4601","#BD3039","#0E3386","#27251F","#C6011F","#0C2340","#33006F","#0C2340","#002D62","#004687","#862633","#005A9C","#000000","#B6922E","#002B5C","#002D72","#0C2340","#003831","#E81828","#FDB827","#C41E3A","#002D62","#FD5A1E","#005C5C","#092C5C","#003278","#134A8E","#AB0003")
color$team_second <- c("#E3D4AD","#CE1141","#000000","#0C2340","#CC3433","#C4CED4","#000000","#E31937","#C4CED4","#FA4616","#EB6E1F","#BD9B60","#003263","#EF3E42","#00A3E0","#0A2351","#D31145","#FF5910","#0C2340","#EFB21E","#002D72","#27251F","#0C2340","#A2AAAD","#27251F","#0C2C56","#F5D130","#C0111F","#E8291C","#14225A")
