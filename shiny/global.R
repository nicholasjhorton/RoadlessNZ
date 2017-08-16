require(mosaic)
require(maptools)
require(dplyr)
library(shiny)
library(DT)
require(googlesheets)

# generate myroadless dataframe with lat/lon points in NZ
source('roadless-setup_NZ.R')

# function to display 1 row (`counter`) of myroadless on google map 
getLocation <- function(counter) {
  googleMap(myroadless[counter,"Latitude"], myroadless[counter,"Longitude"], 
            mark=TRUE, maptype="terrain", radius=1, browse=TRUE)
}

# calculate CI for a single student group's data
process = function(grp) {
  if (nrow(grp) > 0) {
    smgrp = subset(grp, OnLand==1)
    has.road = sum(smgrp$Within1Mile, na.rm=TRUE)
    res = binom.test(has.road, nrow(smgrp))
    return(c(est=res$estimate[[1]], ci=res$conf.int)) 
  } else {
    return(c(NA, NA, NA))
  }
}

# generate plot comparing all CIs to classwide CI
generateGroupCIs <- function(results_list) {

  # remove zero-length data frames
  results_list <- results_list[sapply(results_list, nrow) > 0]
  
  # initialize list to hold estimates and CIs for groups
  ci.df <- data.frame(est=NA, ci1=NA, ci2=NA)
  i <- 1
  for(df in results_list) {
    ci.df[i,] <- process(df)
    i <- i + 1
  }

  # calculate est and CI for all together
  results <- bind_rows(results_list)
  valall = process(results)
  ci.df[i, ] <- valall

  return(ci.df)
}




