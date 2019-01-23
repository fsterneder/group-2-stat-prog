#### helper functions ####

# File Load
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
