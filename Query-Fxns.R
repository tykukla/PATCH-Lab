# ----------------------------------------- # 
# PATCH LAB database query functions        #
#                                           #
# Functions to query database by space,     #
# time, sample, and reference constraints   #
# ---                                       #
# T Kukla (Colorado State University, 2022) #
#                                           # 
# Date created: January 07, 2022            #
# Last modified: January 22, 2022           #
# ----------------------------------------- # 
library(data.table)
library(mapdata)
library(ggplot2)

## USAGE NOTES:
## [1] read in the data as a data.table
## [2] use plot.result=TRUE to show map of data when a query fxn is called


# Un-comment and fill in path to read in data
# df <- as.data.table(read.csv("PATH/FILENAME.csv"))

## Function 1: GeographicQuery
## select data by latitude and longitude bin (bounds are inclusive)
## -- dataframe: isotope data in form of data.table
## -- minlat, maxlat: minimum and maximum latitudinal bounds (minlat must be < maxlat)
## -- minlon, maxlon: minimum and maximum longitudinal bounds (-180 to 180)
## -- plot.result: FALSE to only return data; TRUE to plot the result of query
GeographicQuery <- function(dataframe, minlat, maxlat, minlon, maxlon, plot.result = FALSE){
  df.latTrim <- dataframe[Latitude <= maxlat & Latitude >= minlat]   # cut by latitude
  df.trim <- df.latTrim[Longitude <= maxlon & Longitude >= minlon]   # cut by longitude
  
  # plot if asked
  if(plot.result == TRUE){
    # read in map
    globePoly <- map_data('world')
    # rectangle of bounding box
    this.bb <- as.data.table(base::matrix(data=c(minlat, maxlat, minlon, maxlon), nrow=1, ncol=4))
    colnames(this.bb) <- c("ymin", "ymax", "xmin", "xmax")
    
    # __________ Plot layers
    ggm <- ggplot() +
      coord_fixed(1.3) +
      geom_polygon(data=globePoly, aes(x=long, y=lat, group=group), color='#404040', fill="#bababa") + 
      # add all isotope data
      geom_point(data=dataframe, aes(x=Longitude, y=Latitude), shape=21, fill='white', color='black', size=1.5) +
      # add trimmed isotope data
      geom_point(data=df.trim, aes(x=Longitude, y=Latitude), shape=21, fill='yellow', color='black', size=2) +
      # add rectangle
      geom_rect(data=this.bb, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill=NA, color='darkred', size=1) +
      scale_fill_viridis_c(guide="none") +
      # scale_x_continuous(expand = c(0, 0)) +
      # scale_y_continuous(expand = c(0, 0)) +
      # the default, ratio = 1 in coord_fixed ensures that one unit on the x-axis is the same length as one unit on the y-axis
      # remove the background and default gridlines
      theme_void()
    
    print(ggm)  # print to displays a plot from a fxn: https://ggplot2.tidyverse.org/reference/print.ggplot.html 
    
  }
  
  # return the trimmed result
  return(df.trim)
}



## Function 2: AgeQuery
## select data by age (bounds are inclusive)
## -- dataframe: isotope data in form of data.table
## -- old_Ma: [millions of years ago] old end-member of age bin (if empty, return up to the oldest data)
## -- young_Ma: [millions of years ago] young end-member of age bin (if empty, return up to the youngest data)
## -- plot.result: FALSE to only return data; TRUE to plot the result of query
AgeQuery <- function(dataframe, old_Ma = 1e6, young_Ma = 0, plot.result = FALSE){
  df.trim <- dataframe[Age_Ma >= young_Ma & Age_Ma <= old_Ma]    # remove data too old or too young
  df.inverse <- dataframe[Age_Ma < young_Ma & Age_Ma > old_Ma]   # to plot the data that aren't selected
  
  # plot if asked
  if(plot.result == TRUE){
    # rectangle of bounding box
    this.bb <- as.data.table(base::matrix(data=c(old_Ma, young_Ma), nrow=1, ncol=2))
    colnames(this.bb) <- c("xmax", "xmin")
    if(old_Ma > max(dataframe$Age_Ma, na.rm=TRUE)){this.bb$xmax <- max(dataframe$Age_Ma, na.rm=TRUE)+3}
    
    # __________ Plot layers
    ggm <- ggplot() +
      # add all isotope data
      geom_jitter(data=dataframe, aes(x=Age_Ma, y=0), shape=21, fill='white', color='black', size=1.5, height=0.7, width=0) +
      # add trimmed isotope data
      geom_jitter(data=df.trim, aes(x=Age_Ma, y=0), shape=21, fill='yellow', color='black', size=2.5, height=0.7, width=0) +
      # add rectangle
      geom_rect(data=this.bb, aes(xmin=xmin, xmax=xmax, ymin=-1, ymax=1), fill=NA, color='darkred', size=1) +
      scale_fill_viridis_c(guide="none") +
      scale_x_reverse() +
      scale_y_continuous(limits = c(-1, 1)) +
      theme_few()
    
    suppressWarnings(print(ggm))  # print to displays a plot from a fxn: https://ggplot2.tidyverse.org/reference/print.ggplot.html 
    
  }
  
  # return the trimmed result
  return(df.trim)
}




## Function 3: SampleQuery
## select data by sample type or material (bounds are inclusive)
## -- dataframe: isotope data in form of data.table
## -- Sample_type_basic.vector: vector of strings of Sample_type_basic entries
## -- Sample_type.vector: vector of strings of Sample_type entries
## -- Sample_material.vector: vector of strings of Sample_material entries
SampleQuery <- function(dataframe, Sample_type_basic.vector = c(""), Sample_type.vector = c(""), Sample_material.vector = c("")){
  # return error if any of the vectors are not of type "character"
  if(!is.character(Sample_type_basic.vector)){
    stop("Wrong entry type. 'Sample_type_basic.vector' should be of type character")
  } else if(!is.character(Sample_type.vector)){
    stop("Wrong entry type. 'Sample_type.vector' should be of type character")
  } else if(!is.character(Sample_type.vector)){
    stop("Wrong entry type. 'Sample_material.vector' should be of type character")
  } else{ # otherwise push forward
    # if any vector is empty, don't trim by that vector
    if(Sample_type_basic.vector[1] == ""){
      df.STBtrim <- dataframe
    } else{ df.STBtrim <- dataframe[Sample_type_basic %in% Sample_type_basic.vector] }
    
    if(Sample_type.vector[1] == ""){
      df.STtrim <- df.STBtrim
    } else{ df.STtrim <- df.STBtrim[Sample_type %in% Sample_type.vector] }
    
    if(Sample_material.vector[1] == ""){
      df.SMtrim <- df.STtrim
    } else{ df.SMtrim <- df.STtrim[Sample_material %in% Sample_material.vector] }
  }
  
  # return the result
  return(df.SMtrim)
}

