# 
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
mm3envs <- mclapply(1:36, function(i) {
  set.seed(i+456)
  
  
  #trimmed norm 
  trimmedNorm<-function(mu,sd){
    while(TRUE){
      sample<-rnorm(1,mu,sd)
      if (sample>0)
        return (sample)
    }
  }
  
  
  #attribute
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
  
  
  ## function who calculate the time that the train drive between tow station 
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
  
  ### function who checking if this the last station of the person who in the train 
  check_if_last_station<-function(current_station, end){
    if(current_station == end){
      return(1)
    }
    else{
      return(2)
    }
  }
  
  ### function who checking if  the person was on train 
  was_on_train<-function(start,direction,current_station){
    if(current_station == start){
      return(FALSE)}
    # if( direction == 1){return("TRUE")}
    else{ return(TRUE)}
  } 
  
  
  ## change the 
  exit_TLV<-function(type){
    if(type==1) { ##citizen
      return(c(0,0,F))
    }
    if(type==2){
      return(c(1,1,F))
    }
  }
  
  
  vector_of_cities <- c("1", "2", "3", "4" ,"5")
  i <- 1
  try <- c()
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
  
  
  ## A function that checks whether or not there is a malfunction in the platform
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
  
  ## function that returns the station name
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
  
  ##  function that changes prioritization for those who do not have their last stop entering the platform first
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
  coffe_big_schedule<-schedule(timetable = c(0,2*60,6*60), values = c(4,6,4), period = Inf)
  coffe_small_schedule<-schedule(timetable = c(0,2*60,6*60), values = c(4,5,4), period = Inf)
  coffe_small_schedule_R<-schedule(timetable = c(0,2*60,6*60), values = c(5,6,5), period = Inf)
  train_schedule<- c(90,135,180,225,270,315,360,405,480,525,570,615,660,705,750,595,840,885,930)
  malfunction_vec<-obstacles(60)
  
  ##----------------------------------------- 3.  Init Simulation and add all resources  ------------------------------------------------
  
  Train<- simmer("Train")%>%
    add_resource("ticket_1_Hertzelia",capacity=4,queue_size=Inf)%>%   ## ticket
    add_resource("ticket_2_Hertzelia",capacity=4,queue_size=Inf)%>%
    add_resource("ticket_1_TLV",capacity=4,queue_size=Inf)%>%
    add_resource("ticket_2_TLV",capacity=4,queue_size=Inf)%>%
    add_resource("ticket_Hedera",capacity=6,queue_size=Inf)%>%
    add_resource("ticket_Lod",capacity=6,queue_size=Inf)%>%
    add_resource("ticket_Rehovot",capacity=6,queue_size=Inf)%>%
    add_resource("toilet_Hertzelia",capacity=15,queue_size=Inf)%>%   ## toilet
    add_resource("toilet_TLV",capacity=15,queue_size=Inf)%>%
    add_resource("toilet_Hedera",capacity=10,queue_size=Inf)%>%
    add_resource("toilet_Lod",capacity=10,queue_size=Inf)%>%
    add_resource("toilet_Rehovot",capacity=10,queue_size=Inf)%>%
    add_resource("exit_Hertzelia",capacity=4,queue_size=Inf)%>%   ##exit
    add_resource("exit_TLV",capacity=3,queue_size=Inf,preemptive = T)%>%
    add_resource("exit_Hedera",capacity=3,queue_size=Inf)%>%
    add_resource("exit_Lod",capacity=2,queue_size=Inf)%>%
    add_resource("exit_Rehovot",capacity=2,queue_size=Inf)%>%
    add_resource("coffee_Hertzelia",capacity=coffe_big_schedule,queue_size=Inf)%>% ##coffee
    add_resource("coffee_TLV",capacity=coffe_big_schedule,queue_size=Inf,preemptive = F)%>%
    add_resource("coffee_Hedera",capacity=coffe_small_schedule,queue_size=Inf)%>%
    add_resource("coffee_Lod",capacity=coffe_small_schedule,queue_size=Inf)%>%
    add_resource("coffee_Rehovot",capacity=coffe_small_schedule_R,queue_size=Inf)%>%
    add_resource("security_Hertzelia",capacity=3,queue_size=Inf,preemptive = F)%>%   ## security
    add_resource("security_TLV",capacity=3,queue_size=Inf,preemptive = F)%>%
    add_resource("security_Hedera",capacity=3,queue_size=Inf,preemptive = F)%>%
    add_resource("security_Lod",capacity=3,queue_size=Inf,preemptive = F)%>%
    add_resource("security_Rehovot",capacity=3,queue_size=Inf,preemptive = F)%>%
    add_resource("platform_Hertzelia",capacity=100,queue_size=0,preemptive = T)%>%  ## platform 
    add_resource("platform_TLV",capacity=100,queue_size=0,preemptive = T)%>%
    add_resource("platform_Hedera",capacity=50,queue_size=0,preemptive = T)%>%
    add_resource("platform_Lod",capacity=50,queue_size=0,preemptive = T)%>%
    add_resource("platform_Rehovot",capacity=50,queue_size=0,preemptive = T)
  
  ##----------------------------------------- 4.  All trajectories, start from main trajectory and add sub-trajectories ABOVE IT it . ------------------------------------------------
  
  ########REPAIR WAITING ROOM##############
  waiting_for_repair<-trajectory("waiting_for_repair")%>% 
    set_attribute(keys=c("was_malfunction"),value=1)%>%
    log_("I'm waiting for the platform to be repaired")%>%
    timeout(function () rnorm(1,10,(50/60)))%>%
    log_("I'm going back to the platform!")
  
  
  
  ################ EXIT ###############
  
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
    set_prioritization(function()exit_TLV(get_attribute(Train,"type")))%>%
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
  
  ################BUs##############
  Hedera_bus<-trajectory("Hedera_bus")%>% 
    log_("i leaft to the bus ")%>%
    set_attribute(keys=c("took_the_bus"),value=1)%>%
    seize("exit_Hedera",1)%>%
    timeout(function () get_attribute(Train, "exit"))%>%
    release("exit_Hedera",1)%>%
    leave(prob=1)
  
  Hertzelia_bus<-trajectory("Hertzelia_bus")%>% 
    log_("i leaft to the bus ")%>%
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
    log_("i leaft to the bus ")%>%
    set_attribute(keys=c("took_the_bus"),value=1)%>%
    seize("exit_Lod",1)%>%
    timeout(function () get_attribute(Train, "exit"))%>%
    release("exit_Lod",1)%>%
    leave(prob=1)
  
  Rehovot_bus<-trajectory("Rehovot_bus")%>% 
    log_("i leaft to the bus ")%>%
    set_attribute(keys=c("took_the_bus"),value=1)%>%
    seize("exit_Rehovot",1)%>%
    timeout(function() get_attribute(Train, "exit"))%>%
    release("exit_Rehovot",1)%>%
    leave(prob=1)
  
  ##TRAIN TO SOUTH LINE###
  
  Lod_platform_to_south<- trajectory("Lod_platform_to_south")%>%
    log_("hello, I'm going to Lod platform")%>%
    seize("platform_Lod",amount=1,continue=c(F),reject=Lod_bus) %>%
    timeout(time_out_platform(now(Train),4))%>%
    branch(option = function () malfunction_in_the_train(4, now(Train)), continue = c(T),waiting_for_repair)%>%
    release("platform_Lod",1)%>%
    batch(n=80,name="train_to_Rehovot_from_Lod" , timeout = 5,rule = function() was_on_train( get_attribute(.env=Train, keys=c("start")), get_attribute(.env=Train, keys=c("direction")), get_attribute(.env=Train, keys=c("current_station"))))%>%
    timeout(Drive_time)%>%
    separate()%>%
    join(Rehovot_exit)
  
  Came_to_Lod_south<-trajectory("Came to Lod south")%>%
    timeout(Drive_time)%>%
    separate()%>%
    log_("im in lod")%>%
    set_attribute(keys=c("current_station"),value=4)%>%
    set_prioritization(function()check_prioritization(get_attribute(.env=Train,keys=c("current_station")),get_attribute(.env=Train,keys=c("end"))))%>%
    branch(option = function() check_if_last_station(get_attribute(.env=Train, keys=c("current_station")),get_attribute(.env=Train, keys=c("end"))) ,continue=c(F,F),Lod_exit,Lod_platform_to_south)
  
  
  TLV_platform_to_south<- trajectory("TLV_platform_to_south")%>%
    log_("hello, I'm going to TLV platform to south")%>%
    seize("platform_TLV",amount=1,continue=c(F),reject=TLV_bus) %>%
    timeout(time_out_platform(now(Train),3))%>%
    branch(option = function () malfunction_in_the_train(3, now(Train)), continue = c(T),waiting_for_repair)%>%
    release("platform_TLV",1)%>%
    batch(n=80,name="train_to_Lod_from_TLV" , timeout = 5,rule = function() was_on_train( get_attribute(.env=Train, keys=c("start")), get_attribute(.env=Train, keys=c("direction")), get_attribute(.env=Train, keys=c("current_station"))))%>%
    join(Came_to_Lod_south)
  
  Came_to_TLV_south<-trajectory("Came to TLV south")%>%
    timeout(Drive_time)%>%
    separate()%>%
    log_("im in TLV")%>%
    set_attribute(keys=c("current_station"),value=3)%>%
    set_prioritization(function()check_prioritization(get_attribute(.env=Train,keys=c("current_station")),get_attribute(.env=Train,keys=c("end"))))%>%
    branch(option = function() check_if_last_station(get_attribute(.env=Train, keys=c("current_station")),get_attribute(.env=Train, keys=c("end"))) ,continue=c(F,F),TLV_exit,TLV_platform_to_south)
  
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
  
  
  
  ##TRAIN TO NORTH LINE###
  
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
  
  ##########Hedera############
  Hedera_coffee <- trajectory("Hedera_Coffee")%>%
    log_("Hi I want to buy Coffee")%>%
    seize("coffee_Hedera",1)%>%
    timeout(function () get_attribute(Train, "coffee"))%>%
    release("coffee_Hedera",1)
  
  Hedera_toilet <- trajectory("Hedera_toilet")%>%
    log_("Hi I want to use the toilet")%>%
    seize("toilet_Hedera",1)%>%
    timeout(function () get_attribute(Train, "toilet"))%>%
    release("toilet_Hedera",1)
  
  
  
  Hedera <- trajectory("Hedera")%>%
    log_("hello security, check me! ")%>%
    #set_attribute(keys=c("current_station"),value=1)%>%
    seize("security_Hedera",1)%>%
    timeout(function () get_attribute(Train, "security"))%>%
    release("security_Hedera",1)%>%
    set_prioritization(c(0,0,F))%>%
    log_("hello, I want to buy a ticket")%>%
    seize("ticket_Hedera",1)%>%
    timeout(function () get_attribute(Train, "ticket"))%>%
    release("ticket_Hedera",1)%>%
    branch(option = function() {rdiscrete(1, c(0.2,0.5,0.3), c(0,1,2)) }, continue = c(T,T),Hedera_coffee,Hedera_toilet)%>%
    log_("hello, I'm going to the platform")%>%
    timeout(function () get_attribute(Train, "platform"))%>%
    seize("platform_Hedera",amount=1,continue=c(F),reject=Hedera_bus) %>%
    timeout(time_out_platform(now(Train),1))%>%
    branch(option = function () malfunction_in_the_train(1, now(Train)), continue = c(T),waiting_for_repair)%>%
    release("platform_Hedera",1)%>%
    batch(n=80,name=c("train_to_Hertzelia_from_Hedera") , timeout = 5)%>%
    join(Came_to_Hertzelia_south)
  
  
  
  ##########Herzelia############
  Hertzelia_coffee <- trajectory("Hertzelia_Coffee")%>%
    log_("Hi I want to buy Coffee")%>%
    seize("coffee_Hertzelia",1)%>%
    timeout(function() get_attribute(Train, "coffee"))%>%
    release("coffee_Hertzelia",1)
  
  Hertzelia_toilet <- trajectory("Hertzelia_toilet")%>%
    log_("Hi I want to use the toilet")%>%
    seize("toilet_Hertzelia",1)%>%
    timeout(function() get_attribute(Train, "toilet"))%>%
    release("toilet_Hertzelia",1)
  
  
  Hertzelia <- trajectory("Hertzelia")%>%
    log_("hello security, check me! ")%>%
    # set_attribute(keys=c("current_station"),value=2)%>%
    seize("security_Hertzelia",1)%>%
    timeout(function () get_attribute(Train, "security"))%>%
    release("security_Hertzelia",1)%>%
    set_prioritization(c(0,0,F))%>%
    log_("hello, I want to buy a ticket")%>%
    simmer::select( resources = c("ticket_1_Hertzelia","ticket_2_Hertzelia"),  policy=c("shortest-queue"),id=0)%>%
    seize_selected(amount = 1, id=0) %>%
    timeout(function () get_attribute(Train, "ticket"))%>%
    release_selected(amount = 1, id=0)%>%
    branch(option = function() {rdiscrete(1, c(0.2,0.5,0.3), c(0,1,2)) }, continue = c(T,T),Hertzelia_coffee,Hertzelia_toilet)%>%
    timeout(function () get_attribute(Train, "platform"))%>%
    branch( option= function() get_attribute(.env=Train, keys=c("direction")),continue=c(F,F),Hertzelia_platform_to_north, Hertzelia_platform_to_south)## chose traj to south or north
  
  
  ##########TLV############
  TLV_coffee <- trajectory("TLV_Coffee")%>%
    log_("Hi I want to buy Coffee")%>%
    seize("coffee_TLV",1)%>%
    timeout(function () get_attribute(Train, "coffee"))%>%
    release("coffee_TLV",1)
  
  TLV_toilet <- trajectory("TLV_toilet")%>%
    log_("Hi I want to use the toilet")%>%
    seize("toilet_TLV",1)%>%
    timeout(function () get_attribute(Train, "toilet"))%>%
    release("toilet_TLV",1)
  
  TLV <- trajectory("TLV")%>%
    log_("hello security, check me! ")%>%
    #set_attribute(keys=c("current_station"),value=3)%>%
    seize("security_TLV",1)%>%
    timeout(function () get_attribute(Train, "ticket"))%>%
    release("security_TLV",1)%>%
    set_prioritization(c(0,0,F))%>%
    log_("hello, I want to buy a ticket")%>%
    simmer::select( resources = c("ticket_1_TLV","ticket_2_TLV"),  policy=c("shortest-queue"),id=1)%>%
    seize_selected(amount = 1, id=1) %>%
    timeout(function () get_attribute(Train, "ticket"))%>%
    release_selected(amount = 1, id=1)%>%
    branch(option = function() {rdiscrete(1, c(0.2,0.5,0.3), c(0,1,2)) }, continue = c(T,T),TLV_coffee, TLV_toilet) %>%
    timeout(function () get_attribute(Train, "platform"))%>%
    branch( option= function()get_attribute(Train, "direction"),continue=c(T,T),TLV_platform_to_north ,TLV_platform_to_south ) ## chose traj to south or north
  
  
  
  
  ###########Lod############
  Lod_coffee <- trajectory("Lod_Coffee")%>%
    log_("Hi I want to buy Coffee")%>%
    seize("coffee_Lod",1)%>%
    timeout(function () get_attribute(Train, "coffee"))%>%
    release("coffee_Lod",1)
  
  Lod_toilet <- trajectory("Lod_toilet")%>%
    log_("Hi I want to use the toilet")%>%
    seize("toilet_Lod",1)%>%
    timeout(function () get_attribute(Train, "toilet"))%>%
    release("toilet_Lod",1)
  
  
  
  
  Lod <- trajectory("Lod")%>%
    log_("hello security, check me! ")%>%
    #set_attribute(keys=c("current_station"),value=4)%>%
    seize("security_Lod",1)%>%
    timeout(function ()get_attribute(Train, "security"))%>%
    release("security_Lod",1)%>%
    set_prioritization(c(0,0,F))%>%
    log_("hello, I want to buy a ticket")%>%
    seize("ticket_Lod",1)%>%
    timeout(function ()get_attribute(Train, "ticket"))%>%
    release("ticket_Lod",1)%>%
    branch(option = function() {rdiscrete(1, c(0.2,0.5,0.3), c(0,1,2)) }, continue = c(T,T),Lod_coffee,Lod_toilet)%>%
    timeout(function ()get_attribute(Train, "platform"))%>%
    branch( option= function()get_attribute(Train, "direction"),continue=c(F,F),Lod_platform_to_north ,Lod_platform_to_south) ## chose traj to south or north 
  
  
  ##########Rehovot############
  
  Rehovot_coffee <- trajectory("Rehovot_Coffee")%>%
    log_("Hi I want to buy Coffee")%>%
    seize("coffee_Rehovot",1)%>%
    timeout(function() get_attribute(Train, "coffee"))%>%
    release("coffee_Rehovot",1)
  
  Rehovot_toilet <- trajectory("Rehovot_toilet")%>%
    log_("Hi I want to use the toilet")%>%
    seize("toilet_Rehovot",1)%>%
    timeout(function() get_attribute(Train, "toilet"))%>%
    release("toilet_Rehovot",1)
  
  
  
  Rehovot<- trajectory("Rehovot")%>%
    log_("hello security, check me! ")%>%
    # set_attribute(keys=c("current_station"),value=5)%>%
    seize("security_Rehovot",1)%>%
    timeout(function() get_attribute(Train, "security"))%>%
    release("security_Rehovot",1)%>%
    set_prioritization(c(0,0,F))%>%
    log_("hello, I want to buy a ticket")%>%
    seize("ticket_Rehovot",1)%>%
    timeout(function() get_attribute(Train, "ticket"))%>%
    release("ticket_Rehovot",1)%>%
    branch(option = function() {rdiscrete(1, c(0.2,0.5,0.3), c(0,1,2)) }, continue = c(T,T),Rehovot_coffee,Rehovot_toilet)%>%
    log_("hello, I'm going to the platform")%>%
    timeout(function() get_attribute(Train, "platform"))%>%
    seize("platform_Rehovot",amount=1,continue=c(F),reject=Rehovot_bus) %>%
    timeout(time_out_platform(now(Train),5))%>%
    branch(option = function () malfunction_in_the_train(5, now(Train)), continue = c(T),waiting_for_repair)%>%   ## malfunction_in_the_train
    release("platform_Rehovot",1)%>%
    batch(n=80,name=c("train_to_lod_from_rehovot") , timeout = 5)%>%
    join(Came_to_Lod_north)
  
  
  
  start_citizen<- trajectory("started")%>%
    log_("im main in the train station")%>%
    set_attribute(keys = c("type","start","direction","current_station","end","security","ticket","coffee","toilet","platform","exit"),value=function() typeInef(1))%>%
    branch(option =function() get_attribute(Train, "start"), continue = c(F,F,F,F,F) ,  Hedera, Hertzelia , TLV, Lod, Rehovot)
  
  start_soldier<- trajectory("started")%>%
    log_("im main in the train station")%>%
    set_attribute(keys = c("type","start","direction","current_station","end","security","ticket","coffee","toilet","platform","exit"),value=function() typeInef(2))%>%
    branch(option =function() get_attribute(Train, "start"), continue = c(F,F,F,F,F) , Hedera, Hertzelia , TLV, Lod, Rehovot)
  
  ##----------------------------------------- 5.  All Generators, ALWAYS LAST. ------------------------------------------------
  
  
  Train%>%
    add_generator("citizen", start_citizen, distribution =to(activityTime, function () rexp(1,0.691058)), mon=2,priority=0,preemptible=1, restart=F)%>%
    add_generator("soldier", start_soldier, distribution = to(activityTime, function () rexp(1,0.5132449)), mon=2,priority=1,preemptible=1, restart=F)
  
  
  ##----------------------------------------- 6.  reset, run, plots, outputs ------------------------------------------------
  
  reset(Train)%>%run(until=simulationTime)%>%
    wrap()
})



################################### total time in services- Indicator 1  ###################################
Activity_time <- get_mon_arrivals(mm3envs, per_resource=TRUE)%>%
  mutate(queue_time = end_time - start_time)

########## MADAD 2 Queries - average time per customer in the following resources: security,tickets,coffee,toilet ############

AVG_time_for_customer_per_resource <-  sqldf("select replication, resource,AVG(queue_time) as AVG_Queue from Activity_time as a
             where(a.activity_time<>0 or a.queue_time<>0)  and resource NOT like 'exit%' and resource NOT like 'platform%'
             group by replication, resource 
             order by AVG_Queue DESC ")

###### Number of people visited Coffee #####

Total_customers_in_Coffee <-  sqldf("select replication, count(distinct(name)) as total_customers from Activity_time as a
             where resource  like 'coffee%'
              group by replication ")
####### Avarege time people are buying coffee ############

coffe_avg_time <-  sqldf("select replication, AVG(queue_time) as AVG_Queue from Activity_time as a
                          where(a.activity_time<>0 or a.queue_time<>0)  and  resource  like 'coffee%'
                         group by replication")
#coffe_avg_time1 <- unlist(coffe_avg_time[1,], use.names = FALSE)

########### checking total averege time in each station #################

AVG_time_in_Hertzelia <- sqldf("select replication, sum(AVG_Queue) as Hertzelia_time
                                from AVG_time_for_customer_per_resource
                                where resource like ('%Hertzelia%')
                                group by replication")
#AVG_time_in_Hertzelia1 <- unlist(AVG_time_in_Hertzelia[1,], use.names = FALSE)

AVG_time_in_lod <- sqldf("select replication, sum(AVG_Queue) as lod_time
                          from AVG_time_for_customer_per_resource
                          where resource like ('%Lod%') 
                          group by replication")
#AVG_time_in_lod1 <- unlist(AVG_time_in_lod[1,], use.names = FALSE)

AVG_time_in_TLV <- sqldf("select replication, sum(AVG_Queue) as TLV_time
                          from AVG_time_for_customer_per_resource
                          where resource like ('%TLV%')
                          group by replication")
#AVG_time_in_TLV1 <- unlist(AVG_time_in_TLV[1,], use.names = FALSE)

AVG_time_in_Hedera <- sqldf("select replication, sum(AVG_Queue) as Hedera_time
                             from AVG_time_for_customer_per_resource
                             where resource like ('%Hedera%')
                             group by replication")
#AVG_time_in_Hedera1 <- unlist(AVG_time_in_Hedera[1,], use.names = FALSE)

AVG_time_in_Rehovot <- sqldf("select replication, sum(AVG_Queue) as Rehovot_time
                              from AVG_time_for_customer_per_resource
                              where resource like ('%Rehovot%')
                              group by replication")
# AVG_time_in_Rehovot1 <- unlist(AVG_time_in_Rehovot[1,], use.names = FALSE)

###
madad<- sqldf("select Hr.replication , Hr.Hertzelia_time,L.lod_time ,T.TLV_time,He.Hedera_time, R.Rehovot_time
              from AVG_time_in_Hertzelia as Hr join AVG_time_in_lod as L on  Hr.replication=L.replication
              join AVG_time_in_TLV as T  on  Hr.replication=T.replication 
              join AVG_time_in_Hedera He  on  Hr.replication=He.replication
              join AVG_time_in_Rehovot as R  on  Hr.replication=R.replication
              group by Hr.replication")

madad1_a2<- sqldf("select Hr.replication,  (Hr.Hertzelia_time + L.lod_time +T.TLV_time + He.Hedera_time+ R.Rehovot_time)/5 as mean_time
              from AVG_time_in_Hertzelia as Hr join AVG_time_in_lod as L on  Hr.replication=L.replication
              join AVG_time_in_TLV as T  on  Hr.replication=T.replication 
              join AVG_time_in_Hedera He  on  Hr.replication=He.replication
              join AVG_time_in_Rehovot as R  on  Hr.replication=R.replication
              group by Hr.replication")


######Getting mean and stdev###########
madad1_SD_and_mean_a2 <-  sqldf("select AVG(mean_time) as Mean, STDEV(mean_time) as Standard_Deviation
                                     from madad1_a2")


Madad_simulation_a2 <- unlist(madad1_SD_and_mean_a2[1,], use.names = FALSE)
madad_1_Mean_a2 <- Madad_simulation_a2[1]
madad_1_STDEV_a2 <- Madad_simulation_a2[2]


################################### Avg time in simulation - Indicator 2 ###################################

fullData<-get_mon_arrivals(mm3envs) 
arrivalData <- get_mon_arrivals(mm3envs, per_resource =TRUE)
resourceData <- get_mon_resources(mm3envs)
attributeData <- get_mon_attributes(mm3envs)


###all people in the simualation##
madad2_a2 <- sqldf("select replication, avg(end_time-start_time) as AVG_Simulation_time  
            from fullData
            where finished=TRUE
           group by replication")


madad2_SD_and_mean_a2 <-  sqldf("select AVG(AVG_Simulation_time) as Mean, STDEV(AVG_Simulation_time) as Standard_Deviation
                                     from madad2_a2")


######Getting mean and stdev###########
madad_2_sd_mean_a2 <- unlist(madad2_SD_and_mean_a2[1,], use.names = FALSE)
madad2_Mean_a2 <- madad_2_sd_mean_a2[1]
madad2_STDEV_a2 <- madad_2_sd_mean_a2[2]





################################### Bus proportion Queries and result - Indicator 3   ###################################

fullData<-get_mon_arrivals(mm3envs) 
arrivalData <- get_mon_arrivals(mm3envs, per_resource =TRUE)
resourceData <- get_mon_resources(mm3envs)
attributeData <- get_mon_attributes(mm3envs)

## people who took the bus###
Count_bus <- sqldf("select  a.replication, count(distinct( a.name)) as count_bus
            from attributeData as a
            join attributeData as b on a.name=b.name
            where a.key='took_the_bus' and b.key='start'
            group by a.replication")


#Count_bus1 <- unlist(Count_bus[1,], use.names = FALSE)

###all people in the simualation##
Count_all <- sqldf("select replication, count(distinct( name)) as count
            from fullData
            group by replication")
# Count_all1 <- unlist(Count_all[1,], use.names = FALSE)

madad3_a2 <-sqldf("select a.replication, (cast(a.count_bus as real)/(select count  
                                                          from  Count_all as b 
                                                          join Count_bus as a  
                                                          on a.replication=b.replication group by a.replication )) as bus_Proportion
                       from Count_bus as a  join  Count_all as b on a.replication=b.replication
                      group by a.replication")

madad3_SD_and_mean_a2 <-  sqldf("select AVG(bus_Proportion) as Mean, STDEV(bus_proportion) as Standard_Deviation
                                     from madad3_a2")


######Getting mean and stdev###########
madad_3_Proportion_a2 <- unlist(madad3_SD_and_mean_a2[1,], use.names = FALSE)
madad_3_mean_a2 <- madad_3_Proportion_a2[1]
madad_3_stdev_a2 <- madad_3_Proportion_a2[2]

