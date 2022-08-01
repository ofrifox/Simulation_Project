##-------------------------Load Libraries-------------------------##
# library(rlang)
# library(MASS)
# library(fitdistrplus)
# library(magrittr)
# library(dplyr)
# library(lazyeval)
# library(parallel)
# library(e1071)
# library(plotly)
# library(ggplot2)
# library(triangle)
# library(sqldf)
# library(readxl)
# library(knitr)
# library(rmarkdown)
# library(simmer)
# library(simmer.plot)
# library(DiagrammeR)
# library(fitdistrplus)
# library(magrittr)


##----------------------------------------- 1.  all functions ------------------------------------------------

## Replication Funcion##
mm1envs <- mclapply(1:36, function(i) {
  set.seed(i+456)
  
  
  #trimmed norm 
  trimmedNorm<-function(mu,sd){
    while(TRUE){
      sample<-rnorm(1,mu,sd)
      if (sample>0)
        return (sample)
    }
  }
  
  # attributes function, will give all the attributes for each entity in the simulation.
  # this function will return : 1.type (soldier or citizen) 2.start point 3.current station 4. direction
  # 5. end point 6.time distribution for each resource: security,ticket,coffee,toilet,platform and exit
    typeInef<-function(type){
    if (type==1){    #Citizens
      start<-rdiscrete(1, c(0.24,0.1,0.37,0.09,0.2), c(1,2,3,4,5))   ##   "Hedera"=1 ,"Hertzelia"=2,"TLV"=3, "Lod"=4, "Rehovot"=5
      end <- rdiscrete(1, c(0.25,0.33,0.12,0.1,0.2), c(1,2,3,4,5))   ##   "Hedera"=1 ,"Hertzelia"=2,"TLV"=3, "Lod"=4, "Rehovot"=5
      while (start==end){
        start<-rdiscrete(1, c(0.24,0.1,0.37,0.09,0.2), c(1,2,3,4,5)) 
        end <- rdiscrete(1, c(0.25,0.33,0.12,0.1,0.2), c(1,2,3,4,5))}
      if(start>end){
        direction<-1 ##north
      }
      else{
        direction<-2 ##south
      }
      current_station<-start
      security<-runif(1,2,3)
      ticket<-trimmedNorm(2,0.7)
      coffee<-trimmedNorm(5,1.5)
      toilet<-trimmedNorm(3,45/60)
      platform<-runif(1,8,14)
      exit<-rtriangle(1,2,8,4)
      return (c(type,start,direction,current_station,end,security,ticket,coffee,toilet,platform,exit))
    }
    if (type==2){   #Soldiers
      start<-rdiscrete(1, c(0.24,0.1,0.37,0.09,0.2), c(1,2,3,4,5))  ##   "Hedera"=1 ,"Hertzelia"=2,"TLV"=3, "Lod"=4, "Rehovot"=5
      end <- rdiscrete(1, c(0.25,0.33,0.12,0.1,0.2), c(1,2,3,4,5))  ##   "Hedera"=1 ,"Hertzelia"=2,"TLV"=3, "Lod"=4, "Rehovot"=5
      while (start==end){
        start<-rdiscrete(1, c(0.24,0.1,0.37,0.09,0.2), c(1,2,3,4,5)) 
        end <- rdiscrete(1, c(0.25,0.33,0.12,0.1,0.2), c(1,2,3,4,5))}
      if(start>end){
        direction<-1 ##north
      }
      else{
        direction<-2 ##south
      }
      current_station<-start
      security<-runif(1,0.5,1)
      ticket<-trimmedNorm(2,0.2)
      coffee<-trimmedNorm(5,1.5)
      toilet<-trimmedNorm(3,45/60)
      platform<-runif(1,8,14)
      exit<-rtriangle(1,2,8,4)
      return (c(type,start,direction,current_station,end,security,ticket,coffee,toilet,platform,exit))
    }
  }
  
  
  ## Drive_time function: Will calculate the ride time of the train between 2 stations.
  Drive_time <-function(){
    U1<- runif(1,0,1)
    U2<- runif(1,0,1)
    if(U1<= 1/5){
      return(sqrt(900+2700*U2))
    }
    else{
      return(1.5+sqrt(49+3200*U2)/2)
    }
  }
  
  ### check_if_last_station: this function will check if an entity is in its last station (while riding the train)
  check_if_last_station<-function(current_station, end){
    if(current_station == end){
      return(1)
    }
    else{
      return(2)
    }
  }
  
  ### was_on_train: checks if the entity was on the train in order to give priority in each platform.
  was_on_train<-function(start,direction,current_station){
    if(current_station == start){
      return(FALSE)}
    else{ return(TRUE)}
  } 
  
  
  ## give priority to soldiers (type=2) over citizens (type=1) in exit TLV
  exit_TLV<-function(type){
    if(type==1) { ##citizen
      return(c(0,0,F))
    }
    if(type==2){
      return(c(1,1,F))
    }
  }
  
  
  vector_of_cities <- c("1", "2", "3", "4" ,"5") ## Vector of stations (explaied above)
  i <- 1 
  try <- c() ## empty vector
  
  ##obstacles function: for each round hours this function will return a vector of 3 items list: 1.True\False 2.Station 3.Time
  ##TRUE - there is a malfunction in the station in this hour | False: There isn't a malfunction in the station in this hour
  obstacles <- function (hour){
    while(hour != activityTime){
      where <-vector_of_cities[i]
      obstacle_or_not <- rdiscrete(1,c(0.3,0.7),c(TRUE,FALSE))
      hour <-  hour+60
      i  <-   i+1
      if (i>length(vector_of_cities)){
        i <- 1
      }
      try <- append(try,c(obstacle_or_not,where,hour))
    }
    return(try)
  }
  
  ## A function that checks whether or not there is a malfunction in the platform.
  ## each value of the vector contains 3 values (Boolian,Station,Time)
  ## since there are 15 hours which the platform can get malfuction, the maximum lengh of the vector will be 45
  ## This function wiill return TRUE if there is malfucatoin in a spesific station and time and FALSE if not
  times_1<-3
  times_2<-6
  problem<-1 
  station_m<-2
  malfunction_in_the_train<-function(station, time){
    malfunction<-FALSE
    while(times_1 <45){
      if(time>=malfunction_vec[times_1] & time<=malfunction_vec[times_2] & station==malfunction_vec[station_m]){
        malfunction<-malfunction_vec[problem]
        times_1<-45
      }## if
      else{
        times_1<-times_1+3
        times_2<-times_2+3
        problem<-problem+3 
        station_m<-station+3
      }#else
    } ## while
    if(malfunction== TRUE){  return(1)}
    else{ return(0)}
  } ##function
  
  
  
  ## function that gets the station number and returns the station name
  station_name<-function(current_station){
    if(current_station==1){
      return("Hedera")
    }
    if(current_station==2){
      return("Hertzelia")
    }
    if(current_station==3){
      return("TLV")
    }
    if(current_station==4){
      return("Lod")
    }
    if(current_station==5){
      return("Rehovot")
    }
  }
  
  #function that returns how long you have to wait until the train arrives
  time_out_platform<- function(time,current_station){
    timeout<-train_schedule-time
    if(length(timeout[timeout>0])==0){
      platform <- paste("platform",station_name(current_station),sep = "_")
      exit<-paste(station_name(current_station),"bus",sep = "_")
      trajectory()%>%release(platform)%>%join(exit)
      return(0)
    }
    else{
      return(min(timeout[timeout>0]))
    }
  }
  
  ##  function that changes prioritization for entities that didn't finish their ride
  check_prioritization<-function(current_station,end){
    if(current_station!=end){
      return(c (1,1 ,TRUE))}
    else{
      return(c (0,1 ,TRUE))
    }
  } 
  
  
  ##----------------------------------------- 2.  all simulation parameters ------------------------------------------------
  simulationTime <- 17*60
  activityTime<-960
  coffe_big_schedule<-schedule(timetable = c(0,2*60,6*60), values = c(4,6,4), period = Inf) ## Reinforcement in rush hours
  coffe_small_schedule<-schedule(timetable = c(0,2*60,6*60), values = c(4,5,4), period = Inf) ## Reinforcement in rush hours
  train_schedule<- c(90,135,180,225,270,315,360,405,540,585,630,675,720,765,810,855,900,945)
  malfunction_vec<-obstacles(60) ### First malcantion will start in time 60. 
  
  ##----------------------------------------- 3.  Init Simulation and add all resources  ------------------------------------------------
  
  Train<- simmer("Train")%>%
    add_resource("ticket_1_Hertzelia",capacity=4,queue_size=Inf)%>%                              ## tickets
    add_resource("ticket_2_Hertzelia",capacity=4,queue_size=Inf)%>%
    add_resource("ticket_1_TLV",capacity=4,queue_size=Inf)%>%
    add_resource("ticket_2_TLV",capacity=4,queue_size=Inf)%>%
    add_resource("ticket_Hedera",capacity=6,queue_size=Inf)%>%
    add_resource("ticket_Lod",capacity=6,queue_size=Inf)%>%
    add_resource("ticket_Rehovot",capacity=6,queue_size=Inf)%>%
    add_resource("toilet_Hertzelia",capacity=15,queue_size=Inf)%>%                               ## toilets
    add_resource("toilet_TLV",capacity=15,queue_size=Inf)%>%
    add_resource("toilet_Hedera",capacity=10,queue_size=Inf)%>%
    add_resource("toilet_Lod",capacity=10,queue_size=Inf)%>%
    add_resource("toilet_Rehovot",capacity=10,queue_size=Inf)%>%
    add_resource("exit_Hertzelia",capacity=3,queue_size=Inf)%>%                               ##exits
    add_resource("exit_TLV",capacity=3,queue_size=Inf,preemptive = T)%>%
    add_resource("exit_Hedera",capacity=2,queue_size=Inf)%>%
    add_resource("exit_Lod",capacity=2,queue_size=Inf)%>%
    add_resource("exit_Rehovot",capacity=2,queue_size=Inf)%>%
    add_resource("coffee_Hertzelia",capacity=coffe_big_schedule,queue_size=Inf)%>%               ##coffee
    add_resource("coffee_TLV",capacity=coffe_big_schedule,queue_size=Inf,preemptive = F)%>%
    add_resource("coffee_Hedera",capacity=coffe_small_schedule,queue_size=Inf)%>%
    add_resource("coffee_Lod",capacity=coffe_small_schedule,queue_size=Inf)%>%
    add_resource("coffee_Rehovot",capacity=coffe_small_schedule,queue_size=Inf)%>%
    add_resource("security_Hertzelia",capacity=3,queue_size=Inf,preemptive = F)%>%             ## security
    add_resource("security_TLV",capacity=3,queue_size=Inf,preemptive = F)%>%
    add_resource("security_Hedera",capacity=3,queue_size=Inf,preemptive = F)%>%
    add_resource("security_Lod",capacity=3,queue_size=Inf,preemptive = F)%>%
    add_resource("security_Rehovot",capacity=3,queue_size=Inf,preemptive = F)%>%
    add_resource("platform_Hertzelia",capacity=100,queue_size=0,preemptive = T)%>%        ## platform 
    add_resource("platform_TLV",capacity=100,queue_size=0,preemptive = T)%>%
    add_resource("platform_Hedera",capacity=50,queue_size=0,preemptive = T)%>%
    add_resource("platform_Lod",capacity=50,queue_size=0,preemptive = T)%>%
    add_resource("platform_Rehovot",capacity=50,queue_size=0,preemptive = T)
  
  ##----------------------------------------- 4.  All trajectories, start from main trajectory and add sub-trajectories ABOVE IT it . ------------------------------------------------
  
  ## Waiting room trajectory - if there is a malfunction the entity will pass here ##
  waiting_for_repair<-trajectory("waiting_for_repair")%>% 
    set_attribute(keys=c("was_malfunction"),value=1)%>%
    log_("I'm waiting for the platform to be repaired")%>%
    timeout(function () rnorm(1,10,(50/60)))%>%
    log_("I'm going back to the platform!")
  
  
  
### All Exits Trajectories ###
  Hedera_exit <- trajectory("Hedera_exit")%>%
    set_attribute(keys=c("current_station"),value=1)%>%
    log_("Goodbye")%>%
    seize("exit_Hedera",1)%>%
    timeout(function () get_attribute(Train, "exit"))%>%
    release("exit_Hedera",1)
  
  Hertzelia_exit <- trajectory("Hertzelia_exit")%>%
    set_attribute(keys=c("current_station"),value=2)%>%
    log_("Goodbye")%>%
    seize("exit_Hertzelia",1)%>%
    timeout(function ()  get_attribute(Train, "exit"))%>%
    release("exit_Hertzelia",1)
  
  TLV_exit <- trajectory("TLV_exit")%>%
    set_attribute(keys=c("current_station"),value=3)%>%
    log_("Goodbye")%>%
    set_prioritization(function()exit_TLV(get_attribute(Train,"type")))%>% ## Making sure that soldiers will pass citizens in exit TLV !
    seize("exit_TLV",1)%>%
    timeout(function () get_attribute(Train, "exit"))%>%
    release("exit_TLV",1)
  
  Lod_exit <- trajectory("Lod_exit")%>%
    set_attribute(keys=c("current_station"),value=4)%>%
    log_("Goodbye")%>%
    seize("exit_Lod",1)%>%
    timeout(function () get_attribute(Train, "exit"))%>%
    release("exit_Lod",1)
  
  Rehovot_exit <- trajectory("Rehovot_exit")%>%
    set_attribute(keys=c("current_station"),value=5)%>%
    log_("Goodbye")%>%
    seize("exit_Rehovot",1)%>%
    timeout(function () get_attribute(Train, "exit"))%>%
    release("exit_Rehovot",1)
  
  ## Get to the bus trajectories ##
  Hedera_bus<-trajectory("Hedera_bus")%>% 
    log_("i left to the bus ")%>%
    set_attribute(keys=c("took_the_bus"),value=1)%>%
    seize("exit_Hedera",1)%>%
    timeout(function () get_attribute(Train, "exit"))%>%
    release("exit_Hedera",1)%>%
    leave(prob=1)
  
  Hertzelia_bus<-trajectory("Hertzelia_bus")%>% 
    log_("i left to the bus ")%>%
    set_attribute(keys=c("took_the_bus"),value=1)%>%
    seize("exit_Hertzelia",1)%>%
    timeout(function() get_attribute(Train, "exit"))%>%
    release("exit_Hertzelia",1)%>%
    leave(prob=1)
  
  TLV_bus<-trajectory("TLV_bus")%>%
    log_("i left to the bus ")%>%
    set_attribute(keys=c("took_the_bus"),value=1)%>%
    set_prioritization(function()exit_TLV(get_attribute(Train,"type")))%>%
    seize("exit_TLV",1)%>%
    timeout(function () get_attribute(Train, "exit"))%>%
    release("exit_TLV",1)%>%
    leave(prob=1)
  
  Lod_bus<-trajectory("Lod_bus")%>% 
    log_("i left to the bus ")%>%
    set_attribute(keys=c("took_the_bus"),value=1)%>%
    seize("exit_Lod",1)%>%
    timeout(function () get_attribute(Train, "exit"))%>%
    release("exit_Lod",1)%>%
    leave(prob=1)
  
  Rehovot_bus<-trajectory("Rehovot_bus")%>% 
    log_("i left to the bus ")%>%
    set_attribute(keys=c("took_the_bus"),value=1)%>%
    seize("exit_Rehovot",1)%>%
    timeout(function() get_attribute(Train, "exit"))%>%
    release("exit_Rehovot",1)%>%
    leave(prob=1)
  
  ## Train to south line ##
  
  Lod_platform_to_south<- trajectory("Lod_platform_to_south")%>%
    log_("hello, I'm going to Lod platform")%>%
    seize("platform_Lod",amount=1,continue=c(F),reject=Lod_bus) %>% ## if couldn't seize platform - go to the bus and leave simulation
    timeout(time_out_platform(now(Train),4))%>%  ## time out will be the time that the entity need to wait until next train
    branch(option = function () malfunction_in_the_train(4, now(Train)), continue = c(T),waiting_for_repair)%>% ## checks if there is a malfunction and if there is - go to the wait tranjectory
    release("platform_Lod",1)%>%  ## release after going to the train
    batch(n=80,name="train_to_Rehovot_from_Lod" , timeout = 5,rule = function() was_on_train( get_attribute(.env=Train, keys=c("start")), get_attribute(.env=Train, keys=c("direction")), get_attribute(.env=Train, keys=c("current_station"))))%>% ## Will check if entity was already on train and give it priotity to continue on train
    timeout(Drive_time)%>% ##drive time timeout 
    separate()%>%
    join(Rehovot_exit) #leave from Rehovot - The last station on south line
  
  Came_to_Lod_south<-trajectory("Came to Lod south")%>%  ## Trajectory that will check if the entity should continue to next station in line or exit the exit 
    timeout(Drive_time)%>% 
    separate()%>%
    log_("im in lod")%>%
    set_attribute(keys=c("current_station"),value=4)%>% ## Changing current station to Lod
    set_prioritization(function()check_prioritization(get_attribute(.env=Train,keys=c("current_station")),get_attribute(.env=Train,keys=c("end"))))%>% ## Check if Lod is last sation and give priorities according to that
    branch(option = function() check_if_last_station(get_attribute(.env=Train, keys=c("current_station")),get_attribute(.env=Train, keys=c("end"))) ,continue=c(F,F),Lod_exit,Lod_platform_to_south) ## checks if last station and decide if exit or continue to next station
  
  
  TLV_platform_to_south<- trajectory("TLV_platform_to_south")%>%
    log_("hello, I'm going to TLV platform to south")%>%
    seize("platform_TLV",amount=1,continue=c(F),reject=TLV_bus) %>% ## if couldn't seize platform - go to the bus and leave simulation
    timeout(time_out_platform(now(Train),3))%>% ## time out will be the time that the entity need to wait until next train
    branch(option = function () malfunction_in_the_train(3, now(Train)), continue = c(T),waiting_for_repair)%>% ## checks if there is a malfunction and if there is - go to the wait tranjectory
    release("platform_TLV",1)%>% ## release after going to the train
    batch(n=80,name="train_to_Lod_from_TLV" , timeout = 5,rule = function() was_on_train( get_attribute(.env=Train, keys=c("start")), get_attribute(.env=Train, keys=c("direction")), get_attribute(.env=Train, keys=c("current_station"))))%>% ## Will check if entity was already on train and give it priotity to continue on train
    join(Came_to_Lod_south) ## Continue to Lod
  
  Came_to_TLV_south<-trajectory("Came to TLV south")%>%
    timeout(Drive_time)%>%
    separate()%>%
    log_("im in TLV")%>% ## Changing current station to TLV
    set_attribute(keys=c("current_station"),value=3)%>%
    set_prioritization(function()check_prioritization(get_attribute(.env=Train,keys=c("current_station")),get_attribute(.env=Train,keys=c("end"))))%>% ## Check if TLV is last sation and give priorities according to that
    branch(option = function() check_if_last_station(get_attribute(.env=Train, keys=c("current_station")),get_attribute(.env=Train, keys=c("end"))) ,continue=c(F,F),TLV_exit,TLV_platform_to_south)  ## checks if TLV is last station and decide if exit or continue to next station
  
  Hertzelia_platform_to_south<- trajectory("Hertzelia_platform_to_south")%>%
    log_("hello, I'm going to Hertzelia platform")%>%
    seize("platform_Hertzelia",amount=1,continue=c(F),reject=Hertzelia_bus) %>%
    timeout(time_out_platform(now(Train),2))%>%
    branch(option = function () malfunction_in_the_train(2, now(Train)), continue = c(T),waiting_for_repair)%>%
    release("platform_Hertzelia",1)%>%
    batch(n=80,name="train_to_TLV_from_Hertzelia" , timeout = 5,rule = function() was_on_train( get_attribute(.env=Train, keys=c("start")), get_attribute(.env=Train, keys=c("direction")), get_attribute(.env=Train, keys=c("current_station"))))%>%
    join(Came_to_TLV_south)
  
  Came_to_Hertzelia_south<-trajectory("Came to Hertzelia south")%>%
    timeout(Drive_time)%>%
    separate()%>%
    log_("im in Hertzelia")%>%
    set_attribute(keys=c("current_station"),value=2)%>%
    set_prioritization(function()check_prioritization(get_attribute(.env=Train,keys=c("current_station")),get_attribute(.env=Train,keys=c("end"))))%>%
    branch(option = function() check_if_last_station(get_attribute(.env=Train, keys=c("current_station")),get_attribute(.env=Train, keys=c("end"))) ,continue=c(F,F),Hertzelia_exit,Hertzelia_platform_to_south)
  
  
  
  ##Train to north line##
  
  Hertzelia_platform_to_north<- trajectory("Hertzelia_platform_to_north")%>%
    log_("hello, I'm going to Hertzelia platform")%>%
    seize("platform_Hertzelia",amount=1,continue=c(F),reject=Hertzelia_bus) %>%
    timeout(time_out_platform(now(Train),2))%>%
    branch(option = function () malfunction_in_the_train(2, now(Train)), continue = c(T),waiting_for_repair)%>%
    release("platform_Hertzelia",1)%>%
    batch(n=80,name="train_to_hedera_from_Hertzelia" , timeout = 5,rule = function() was_on_train( get_attribute(.env=Train, keys=c("start")),get_attribute(.env=Train, keys=c("direction")), get_attribute(.env=Train, keys=c("current_station"))))%>%
    timeout(Drive_time)%>%
    separate()%>%
    join(Hedera_exit)
  
  
  Came_to_Hertzelia_north<-trajectory("Came to Hertzelia north")%>%
    timeout(Drive_time)%>%
    separate()%>%
    log_("im in Hertzelia")%>%
    set_prioritization(function()check_prioritization(get_attribute(.env=Train,keys=c("current_station")),get_attribute(.env=Train,keys=c("end"))))%>%
    set_attribute(keys=c("current_station"),value=2)%>%
    branch(option = function() check_if_last_station(get_attribute(.env=Train, keys=c("current_station")),get_attribute(.env=Train, keys=c("end"))) ,continue=c(F,F),Hertzelia_exit,Hertzelia_platform_to_north)
  
  
  TLV_platform_to_north<- trajectory("TLV_platform_to_north")%>%
    log_("hello, I'm going to TLV platform")%>%
    seize("platform_TLV",amount=1,continue=c(F),reject=TLV_bus) %>%
    timeout(time_out_platform(now(Train),3))%>%
    branch(option = function () malfunction_in_the_train(3, now(Train)), continue = c(T),waiting_for_repair)%>%
    release("platform_TLV")%>%
    batch(n=80,name="train_to_Hertzelia_from_TLV" , timeout = 5,rule = function() was_on_train( get_attribute(.env=Train, keys=c("start")), get_attribute(.env=Train, keys=c("direction")), get_attribute(.env=Train, keys=c("current_station"))))%>%
    join(Came_to_Hertzelia_north)
  
  
  Came_to_TLV_north<-trajectory("Came to TLV north")%>%
    timeout(Drive_time)%>%
    separate()%>%
    log_("im in TLV")%>%
    set_attribute(keys=c("current_station"),value=3)%>%
    set_prioritization(function()check_prioritization(get_attribute(.env=Train,keys=c("current_station")),get_attribute(.env=Train,keys=c("end"))))%>%
    branch(option = function() check_if_last_station(get_attribute(.env=Train, keys=c("current_station")),get_attribute(.env=Train, keys=c("end"))) ,continue=c(F,F),TLV_exit,TLV_platform_to_north)
  
  Lod_platform_to_north<- trajectory("Lod_platform_to_north")%>%
    log_("hello, I'm going to Lod platform")%>%
    seize("platform_Lod",amount=1,continue=c(F),reject=Lod_bus) %>%
    timeout(time_out_platform(now(Train),4))%>%
    branch(option = function () malfunction_in_the_train(4, now(Train)), continue = c(T),waiting_for_repair)%>%
    release("platform_Lod",1)%>%
    batch(n=80,name="train_to_TLV_from_Lod" , timeout = 5 ,rule = function() was_on_train( get_attribute(.env=Train, keys=c("start")), get_attribute(.env=Train, keys=c("direction")), get_attribute(.env=Train, keys=c("current_station"))))%>%
    join(Came_to_TLV_north)
  
  Came_to_Lod_north<-trajectory("Came to Lod north")%>%
    timeout(Drive_time)%>%
    separate()%>%
    log_("im in Lod")%>%
    set_attribute(keys=c("current_station"),value=4)%>%
    set_prioritization(function()check_prioritization(get_attribute(.env=Train,keys=c("current_station")),get_attribute(.env=Train,keys=c("end"))))%>%
    branch(option = function() check_if_last_station(get_attribute(.env=Train, keys=c("current_station")),get_attribute(.env=Train, keys=c("end"))) ,continue=c(F,F),Lod_exit,Lod_platform_to_north)
  
  ## Hedera ##
  Hedera_coffee <- trajectory("Hedera_Coffee")%>% ## Coffee trajectory
    log_("Hi I want to buy Coffee")%>%
    seize("coffee_Hedera",1)%>% 
    timeout(function () get_attribute(Train, "coffee"))%>%
    release("coffee_Hedera",1)
  
  Hedera_toilet <- trajectory("Hedera_toilet")%>% ### Toilet trajectory
    log_("Hi I want to use the toilet")%>%
    seize("toilet_Hedera",1)%>%
    timeout(function () get_attribute(Train, "toilet"))%>%
    release("toilet_Hedera",1)
  
  
  
  Hedera <- trajectory("Hedera")%>% ## Hedera main trajectory
    log_("hello security, check me! ")%>%
    seize("security_Hedera",1)%>% ## First - security check
    timeout(function () get_attribute(Train, "security"))%>%
    release("security_Hedera",1)%>%
    set_prioritization(c(0,0,F))%>% ## Set priority to 0 since after security there are no bypasses in the simulation (except in exit TLV)
    log_("hello, I want to buy a ticket")%>%
    seize("ticket_Hedera",1)%>% ## Second - buy tickets
    timeout(function () get_attribute(Train, "ticket"))%>%
    release("ticket_Hedera",1)%>%
    branch(option = function() {rdiscrete(1, c(0.2,0.5,0.3), c(0,1,2)) }, continue = c(T,T),Hedera_coffee,Hedera_toilet)%>% ## Decide if the entity will go to the coffee or toilets
    log_("hello, I'm going to the platform")%>%
    timeout(function () get_attribute(Train, "platform"))%>% ## Arriving to platform timeout
    seize("platform_Hedera",amount=1,continue=c(F),reject=Hedera_bus) %>% ## Try to seize platform and if not go to bus
    timeout(time_out_platform(now(Train),1))%>% ## Waiting in platform to train timeout
    branch(option = function () malfunction_in_the_train(1, now(Train)), continue = c(T),waiting_for_repair)%>%  ## Check for malfunction in the time the entity is in the platform
    release("platform_Hedera",1)%>% ## release the platform after going on the train \ bus
    batch(n=80,name=c("train_to_Hertzelia_from_Hedera") , timeout = 5)%>% ### sending the entity to Hertzelia since Hedera is the last station in the north line
    join(Came_to_Hertzelia_south)
  
  
  
  ## Herzelia ##
  Hertzelia_coffee <- trajectory("Hertzelia_Coffee")%>% ## Hertzeila coffee
    log_("Hi I want to buy Coffee")%>%
    seize("coffee_Hertzelia",1)%>%
    timeout(function() get_attribute(Train, "coffee"))%>%
    release("coffee_Hertzelia",1)
  
  Hertzelia_toilet <- trajectory("Hertzelia_toilet")%>% ### Herzeila Toilet
    log_("Hi I want to use the toilet")%>%
    seize("toilet_Hertzelia",1)%>%
    timeout(function() get_attribute(Train, "toilet"))%>%
    release("toilet_Hertzelia",1)
  
  
  Hertzelia <- trajectory("Hertzelia")%>% ## Hertzeila main trajectory (Big station)
    log_("hello security, check me! ")%>%
    seize("security_Hertzelia",1)%>% ## First-  security
    timeout(function () get_attribute(Train, "security"))%>%
    release("security_Hertzelia",1)%>%
    set_prioritization(c(0,0,F))%>% ## Set priority to 0 since after security there are no bypasses in the simulation (except in exit TLV)
    log_("hello, I want to buy a ticket")%>%
    simmer::select( resources = c("ticket_1_Hertzelia","ticket_2_Hertzelia"),  policy=c("shortest-queue"),id=0)%>% ## send the entity to one of the tickets line when the shortest queue is prioritiezed 
    seize_selected(amount = 1, id=0) %>% ## Seize the selected ticket resource
    timeout(function () get_attribute(Train, "ticket"))%>%
    release_selected(amount = 1, id=0)%>% ## Release the selected ticket resource
    branch(option = function() {rdiscrete(1, c(0.2,0.5,0.3), c(0,1,2)) }, continue = c(T,T),Hertzelia_coffee,Hertzelia_toilet)%>% ## Decide if the entity will go to coffee or toilet
    timeout(function () get_attribute(Train, "platform"))%>% ## Going to the platform timeout
    branch( option= function() get_attribute(.env=Train, keys=c("direction")),continue=c(F,F),Hertzelia_platform_to_north, Hertzelia_platform_to_south)## if direction =1 : continue in north line, if direction=2, continue in south line
  
  
  ## TLV ##
  TLV_coffee <- trajectory("TLV_Coffee")%>% ## TLV Coffee
    log_("Hi I want to buy Coffee")%>%
    seize("coffee_TLV",1)%>%
    timeout(function () get_attribute(Train, "coffee"))%>%
    release("coffee_TLV",1)
  
  TLV_toilet <- trajectory("TLV_toilet")%>% ## TLV Toilet
    log_("Hi I want to use the toilet")%>%
    seize("toilet_TLV",1)%>%
    timeout(function () get_attribute(Train, "toilet"))%>%
    release("toilet_TLV",1)
  
  TLV <- trajectory("TLV")%>%
    log_("hello security, check me! ")%>%
    seize("security_TLV",1)%>% ## First - Security
    timeout(function () get_attribute(Train, "ticket"))%>%
    release("security_TLV",1)%>%
    set_prioritization(c(0,0,F))%>% ## Set priority to 0 since after security there are no bypasses in the simulation (except in exit TLV)
    log_("hello, I want to buy a ticket")%>%
    simmer::select( resources = c("ticket_1_TLV","ticket_2_TLV"),  policy=c("shortest-queue"),id=1)%>% ## send the entity to one of the tickets line when the shortest queue is prioritized
    seize_selected(amount = 1, id=1) %>% ## Seize the selected ticket resource
    timeout(function () get_attribute(Train, "ticket"))%>%
    release_selected(amount = 1, id=1)%>%## Release the selected ticket resource
    branch(option = function() {rdiscrete(1, c(0.2,0.5,0.3), c(0,1,2)) }, continue = c(T,T),TLV_coffee, TLV_toilet) %>% ## Decide if the entity will go to coffee or toilet
    timeout(function () get_attribute(Train, "platform"))%>%
    branch( option= function()get_attribute(Train, "direction"),continue=c(T,T),TLV_platform_to_north ,TLV_platform_to_south ) ## if direction =1 : continue in north line, if direction=2, continue in south line
  
  
  
  
  ## Lod ##
  Lod_coffee <- trajectory("Lod_Coffee")%>% ## Lod Coffee
    log_("Hi I want to buy Coffee")%>%
    seize("coffee_Lod",1)%>%
    timeout(function () get_attribute(Train, "coffee"))%>%
    release("coffee_Lod",1)
  
  Lod_toilet <- trajectory("Lod_toilet")%>% ## Lod Toilet
    log_("Hi I want to use the toilet")%>%
    seize("toilet_Lod",1)%>%
    timeout(function () get_attribute(Train, "toilet"))%>%
    release("toilet_Lod",1)
  
  
  
  
  Lod <- trajectory("Lod")%>% ## Lod main trajectory
    log_("hello security, check me! ")%>%
    seize("security_Lod",1)%>% ## First - Security
    timeout(function ()get_attribute(Train, "security"))%>% 
    release("security_Lod",1)%>%
    set_prioritization(c(0,0,F))%>% ## Set priority to 0 since after security there are no bypasses in the simulation (except in exit TLV)
    log_("hello, I want to buy a ticket")%>%
    seize("ticket_Lod",1)%>% ## Second - Tickets
    timeout(function ()get_attribute(Train, "ticket"))%>%
    release("ticket_Lod",1)%>%
    branch(option = function() {rdiscrete(1, c(0.2,0.5,0.3), c(0,1,2)) }, continue = c(T,T),Lod_coffee,Lod_toilet)%>% ## Decide if the entity will go to coffee or toilet
    timeout(function ()get_attribute(Train, "platform"))%>%
    branch( option= function()get_attribute(Train, "direction"),continue=c(F,F),Lod_platform_to_north ,Lod_platform_to_south) ## if direction =1 : continue in north line, if direction=2, continue in south line
  
  
  ## Rehovot ##
  
  Rehovot_coffee <- trajectory("Rehovot_Coffee")%>% ## Coffee Rehovot
    log_("Hi I want to buy Coffee")%>%
    seize("coffee_Rehovot",1)%>%
    timeout(function() get_attribute(Train, "coffee"))%>% 
    release("coffee_Rehovot",1)
  
  Rehovot_toilet <- trajectory("Rehovot_toilet")%>% ## Toilet Rehovot
    log_("Hi I want to use the toilet")%>%
    seize("toilet_Rehovot",1)%>%
    timeout(function() get_attribute(Train, "toilet"))%>%
    release("toilet_Rehovot",1)
  
  
  
  Rehovot<- trajectory("Rehovot")%>% ## Main trajectory Rehovot
    log_("hello security, check me! ")%>%
    seize("security_Rehovot",1)%>% ## First - Security
    timeout(function() get_attribute(Train, "security"))%>%
    release("security_Rehovot",1)%>%
    set_prioritization(c(0,0,F))%>% ## Set priority to 0 since after security there are no bypasses in the simulation (except in exit TLV)
    log_("hello, I want to buy a ticket")%>%
    seize("ticket_Rehovot",1)%>% ## Second - Tickets
    timeout(function() get_attribute(Train, "ticket"))%>%
    release("ticket_Rehovot",1)%>%
    branch(option = function() {rdiscrete(1, c(0.2,0.5,0.3), c(0,1,2)) }, continue = c(T,T),Rehovot_coffee,Rehovot_toilet)%>% ## Decide if the entity will go to coffee or toilet
    log_("hello, I'm going to the platform")%>%
    timeout(function() get_attribute(Train, "platform"))%>% ## Going to platform timout
    seize("platform_Rehovot",amount=1,continue=c(F),reject=Rehovot_bus) %>% ## Try to seize the platform, if unsuccessful go to bus
    timeout(time_out_platform(now(Train),5))%>% ## Waiting in platform to train timeout
    branch(option = function () malfunction_in_the_train(5, now(Train)), continue = c(T),waiting_for_repair)%>%   ## malfunction in train check
    release("platform_Rehovot",1)%>%
    batch(n=80,name=c("train_to_lod_from_rehovot") , timeout = 5)%>% ## go on the train to north since Rehovot is the southest station
    join(Came_to_Lod_north)
  
  
  
  start_citizen<- trajectory("started")%>% ## Start trajectory for citizen
    log_("im main in the train station")%>%
    set_attribute(keys = c("type","start","direction","current_station","end","security","ticket","coffee","toilet","platform","exit"),value=function() typeInef(1))%>% ## set the attributes for each entity with the typeInef function
    branch(option =function() get_attribute(Train, "start"), continue = c(F,F,F,F,F) ,  Hedera, Hertzelia , TLV, Lod, Rehovot) ## Sent the entity to next trajectory according to "start" attribute
  
  start_soldier<- trajectory("started")%>% ## Start trajectory for soldier
    log_("im main in the train station")%>%
    set_attribute(keys = c("type","start","direction","current_station","end","security","ticket","coffee","toilet","platform","exit"),value=function() typeInef(2))%>% ## set the attributes for each entity with the typeInef function
    branch(option =function() get_attribute(Train, "start"), continue = c(F,F,F,F,F) , Hedera, Hertzelia , TLV, Lod, Rehovot)## Sent the entity to next trajectory according to "start" attribute
  
  ##----------------------------------------- 5.  All Generators, ALWAYS LAST. ------------------------------------------------
  
  
  Train%>%
    ## Citizen - gets  priority 0 (lower then soldiers) and preemtible =1 in order to get passed
    add_generator("citizen", start_citizen, distribution =to(activityTime, function () rexp(1,0.691058)), mon=2,priority=0,preemptible=1, restart=F)%>%
    ## Soldier - gets priority 1 (higher then citizen) and preemitble = 1 in order to bypass
    add_generator("soldier", start_soldier, distribution = to(activityTime, function () rexp(1,0.5132449)), mon=2,priority=1,preemptible=1, restart=F)
  
  
  ##----------------------------------------- 6.  reset, run, plots, outputs ------------------------------------------------
  
  reset(Train)%>%run(until=simulationTime)%>%
    wrap()
})


  ##----------------------------------------- 7.  SQL Queries  --------------------------------------------------------------




## Activity time table - gets all activity time including queue times per resource ##
Activity_time <- get_mon_arrivals(mm1envs, per_resource=TRUE)%>%
  mutate(queue_time = end_time - start_time)


### ----------- Avg time in station resources - Indicator 1  ----------- ###

### average time per customer in the following resources: security,tickets,coffee,toilet  - used for graphs##

AVG_time_for_customer_per_resource <-  sqldf("select replication, resource,AVG(queue_time) as AVG_Queue from Activity_time as a
             where(a.activity_time<>0 or a.queue_time<>0)  and resource NOT like 'exit%' and resource NOT like 'platform%'
             group by replication, resource 
             order by AVG_Queue DESC ")


### checking total average time in each station ###

AVG_time_in_Hertzelia <- sqldf("select replication, sum(AVG_Queue) as Hertzelia_time
                                from AVG_time_for_customer_per_resource
                                where resource like ('%Hertzelia%')
                                group by replication")

AVG_time_in_lod <- sqldf("select replication, sum(AVG_Queue) as lod_time
                          from AVG_time_for_customer_per_resource
                          where resource like ('%Lod%') 
                          group by replication")

AVG_time_in_TLV <- sqldf("select replication, sum(AVG_Queue) as TLV_time
                          from AVG_time_for_customer_per_resource
                          where resource like ('%TLV%')

                                                   group by replication")
AVG_time_in_Hedera <- sqldf("select replication, sum(AVG_Queue) as Hedera_time
                             from AVG_time_for_customer_per_resource
                             where resource like ('%Hedera%')
                             group by replication")

AVG_time_in_Rehovot <- sqldf("select replication, sum(AVG_Queue) as Rehovot_time
                              from AVG_time_for_customer_per_resource
                              where resource like ('%Rehovot%')
                              group by replication")


### Get all data from each station into one table ###
madad<- sqldf("select Hr.replication , Hr.Hertzelia_time,L.lod_time ,T.TLV_time,He.Hedera_time, R.Rehovot_time
              from AVG_time_in_Hertzelia as Hr join AVG_time_in_lod as L on  Hr.replication=L.replication
              join AVG_time_in_TLV as T  on  Hr.replication=T.replication 
              join AVG_time_in_Hedera He  on  Hr.replication=He.replication
              join AVG_time_in_Rehovot as R  on  Hr.replication=R.replication
              group by Hr.replication")

## Madad 1 : Average waiting time in all stations (per replication) ##

madad1<- sqldf("select Hr.replication,  (Hr.Hertzelia_time + L.lod_time +T.TLV_time + He.Hedera_time+ R.Rehovot_time)/5 as mean_time
              from AVG_time_in_Hertzelia as Hr join AVG_time_in_lod as L on  Hr.replication=L.replication
              join AVG_time_in_TLV as T  on  Hr.replication=T.replication 
              join AVG_time_in_Hedera He  on  Hr.replication=He.replication
              join AVG_time_in_Rehovot as R  on  Hr.replication=R.replication
              group by Hr.replication")



## Madad 1: getting the avarage and STDEV from all replications ##
madad1_SD_and_mean <-  sqldf("select AVG(mean_time) as Mean, STDEV(mean_time) as Standard_Deviation
                                     from madad1")


## Getting the data ##

Madad_simulation <- unlist(madad1_SD_and_mean[1,], use.names = FALSE)
madad_1_Mean <- Madad_simulation[1]
madad_1_STDEV <- Madad_simulation[2]



### ----------- Avg time in simulation - Indicator 2  ----------- ###


### Creating main tables ###
fullData<-get_mon_arrivals(mm1envs) 
arrivalData <- get_mon_arrivals(mm1envs, per_resource =TRUE)
resourceData <- get_mon_resources(mm1envs)
attributeData <- get_mon_attributes(mm1envs)



### Avarege time in simulation per replication ##
madad2 <- sqldf("select replication, avg(end_time-start_time) as AVG_Simulation_time  
            from fullData
            where finished=TRUE
           group by replication")


## The avarage and STDEV of all replications ##
madad2_SD_and_mean <-  sqldf("select AVG(AVG_Simulation_time) as Mean, STDEV(AVG_Simulation_time) as Standard_Deviation
                                     from madad2")


## Getting mean and stdev data ##
madad_2_sd_mean <- unlist(madad2_SD_and_mean[1,], use.names = FALSE)
madad2_Mean <- madad_2_sd_mean[1]
madad2_STDEV <- madad_2_sd_mean[2]



### ----------- Bus proportion Queries and result - Indicator 3  ----------- ###

fullData<-get_mon_arrivals(mm1envs) 
arrivalData <- get_mon_arrivals(mm1envs, per_resource =TRUE)
resourceData <- get_mon_resources(mm1envs)
attributeData <- get_mon_attributes(mm1envs)

## Count the number of entities who took the bus in each replication ##
Count_bus <- sqldf("select  a.replication, count(distinct( a.name)) as count_bus
            from attributeData as a
            join attributeData as b on a.name=b.name
            where a.key='took_the_bus' and b.key='start'
            group by a.replication")

## Count the total number of entities who took place in the simulation ##
Count_all <- sqldf("select replication, count(distinct( name)) as count
            from fullData
            group by replication")

## Creating the indicator - (no.of.people.who.took.the.bus/total.number.of.people.in.simulation) for each replication ##

madad3 <-sqldf("select a.replication, (cast(a.count_bus as real)/(select count  
                                                          from  Count_all as b 
                                                          join Count_bus as a  
                                                          on a.replication=b.replication group by a.replication )) as bus_Proportion
                       from Count_bus as a  join  Count_all as b on a.replication=b.replication
                      group by a.replication")

## Get the mean and STDEV ##

madad3_SD_and_mean <-  sqldf("select AVG(bus_Proportion) as Mean, STDEV(bus_proportion) as Standard_Deviation
                                     from madad3")


## Getting mean and stdev data ##
madad_3_Proportion <- unlist(madad3_SD_and_mean[1,], use.names = FALSE)
madad_3_mean <- madad_3_Proportion[1]
madad_3_stdev <- madad_3_Proportion[2]





