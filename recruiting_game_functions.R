#this script is used to house functions for the recruiting app

#load packages
library(tidyverse)
library(data.table)
library(truncnorm)
library(randomNames)
library(maps)
library(codename)

create_coaches = function(ns = 400){
  #this function creates the database of coaches
  #the input is the number of coaches to create
  #the output is a database of coaches
  
  #create database
  coach_db =  data.frame(names = randomNames(ns, gender = 0, name.order = 'first.last'),
                         age = round(runif(ns, 30, 75), 0), 
                         offense = round(rtruncnorm(ns,mean=50, sd=15,a=1,b=100),0),
                         defense = round(rtruncnorm(ns,mean=50,sd=15,a=1,b=100),0),
                         recruit = round(rtruncnorm(ns,mean=50,sd=15,a=1,b=100),0),
                         scout = round(rtruncnorm(ns,mean=50,sd=15,a=1,b=100),0))
  
  #calculate overall 
  coach_db$overall = rowSums(coach_db[,c(3:6)]) / 4
  
  #output db from function
  return(coach_db)
}

create_players = function(ns = 1000, pos = 'PG', hmax = 77, hmin = 65, hmean = 74,
                          han = 70, pas = 70, reb = 30, ins = 30, mr = 55, tp = 55){
  #this function creates players and is nested within create player db
  #the inputs are means for individual player traits
  #the output is a player db
  
  #create player db
  player_db =  data.frame(names = randomNames(ns, gender = 0, name.order = 'first.last'),
                         age = round(runif(ns, 17, 23), 0),
                         height = round(rtruncnorm(ns,mean=hmean,sd=2,a=hmin,b=hmax),0),
                         position = pos,
                         athleticism = round(rtruncnorm(ns,mean=50,sd=15,a=1,b=100),0),
                         handles = round(rtruncnorm(ns,mean=han,sd=15,a=1,b=100),0),
                         passing = round(rtruncnorm(ns,mean=pas,sd=15,a=1,b=100),0),
                         defense = round(rtruncnorm(ns,mean=50,sd=15,a=1,b=100),0),
                         rebounding = round(rtruncnorm(ns,mean=reb,sd=15,a=1,b=100),0),
                         inside = round(rtruncnorm(ns,mean=ins,sd=15,a=1,b=100),0), 
                         midrange = round(rtruncnorm(ns,mean=mr,sd=15,a=1,b=100),0),
                         threepoint = round(rtruncnorm(ns,mean=tp,sd=15,a=1,b=100),0))
  
  #output player db from function
  return(player_db)
}

create_player_db = function(ns = 1000){
  #this function is used to create a database of players
  #the input is the number of players to create 
  #the output is a player database
  
  #create pgs
  pgs = create_players(ns = ns, pos = 'PG', hmax = 77, hmin = 65, hmean = 74,
                       han = 70, pas = 70, ins = 30, mr = 55, tp = 55)
  
  #create sgs
  sgs = create_players(ns = ns, pos = 'SG', hmax = 78, hmin = 67, hmean = 74,
                       han = 60, pas = 50, ins = 35, mr = 60, tp = 65)
  
  #create sfs
  sfs = create_players(ns = ns, pos = 'SF', hmax = 80, hmin = 75, hmean = 78,
                       han = 50, pas = 45, ins = 40, mr = 60, tp = 55)
  
  #create pfs
  pfs = create_players(ns = ns, pos = 'PF', hmax = 83, hmin = 77, hmean = 80,
                       han = 40, pas = 40, ins = 60, mr = 55, tp = 45)
  
  #create pfs
  cs = create_players(ns = ns, pos = 'C', hmax = 88, hmin = 78, hmean = 82,
                       han = 40, pas = 40, ins = 65, mr = 50, tp = 40)
  
  #combine dfs and output from function
  player_db = data.frame(rbind(pgs, sgs, sfs, pfs, cs))
  return(player_db)
}

create_schools = function(ns){
  #this function is used to create schools for the game
  #the input is the number of schools to create
  #the output is a db of schools
  
  #create locations for schools and trim and rename columns
  schools = subset(us.cities, name %in% sample(us.cities$name, ns))
  schools = schools[,c(1:3)]
  colnames(schools) = c('location', 'state', 'population')
  
  #sample animals list and add to schools df
  schools$mascot = sample(data.frame(animals)$value, ns)
  
  #create prestige
  schools$prestige = round(rtruncnorm(ns,mean=50,sd=15,a=1,b=100),0)
  
  #output from function
  return(schools)
}

