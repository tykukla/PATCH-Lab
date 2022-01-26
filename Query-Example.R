# ----------------------------------------- # 
# PATCH LAB database query Example          #
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


## Usage notes: This script will not run "out of the box"!
## Set a path to the working dir, query functions, and data (Examples below)
path.workdir <- "main/sub"          # path to the working directory (does not have to be directory of data or functions)
path.data <- "main/sub/dat"         # path where the dataframe is 
path.queryFxn <- "main/sub/scripts" # path to "Query-Fxns.R" script
filename.data <- "ISOTOPES.csv"     # name of the isotope dataset


# Set working directory to location of dataframe
setwd(path.workdir)   # This needs to be changed

# Source the query functions
source_me <- paste(path.queryFxn, "Query-Fxns.R", sep='/')
source(source_me)

# Read in the data
df.filename <- paste(path.data, filename.data, sep='/')
df <- as.data.table(read.csv(df.filename))


## : EXAMPLE 1 -- Query by lat/lon (lon is -180 to 180)
lat.min <- 30 ; lat.max <- 47        # latitude bounds (lat.min must be < lat.max)
lon.min <- -122 ; lon.max <- -103    # longitude bounds (lon.min must be < lon.max)

df.Ex1 <- GeographicQuery(dataframe = df, minlat = lat.min, maxlat = lat.max,
                          minlon = lon.min, maxlon = lon.max, plot.result = TRUE)


## : EXAMPLE 2 -- Query by age
lat.min <- 30 ; lat.max <- 47        # latitude bounds (lat.min must be < lat.max)
lon.min <- -122 ; lon.max <- -103    # longitude bounds (lon.min must be < lon.max)

df.Ex2 <- AgeQuery(dataframe = df, old_Ma = 34, young_Ma = 7, plot.result = TRUE)



## : EXAMPLE 3 -- Query by sample type
# no plotting example here because constraints are qualitative
main_sampletype <- c("inorganic carbonate (paleosol + fluvial", "phyllosilicate")
sub_sampletype <- c("Paleosol Carbonate", "Paleosol", "Altered ash")
samplematerial <- c("smectite", "calcite")

df.Ex3 <- SampleQuery(dataframe = df, Sample_type_basic.vector = main_sampletype, 
                      Sample_type.vector = sub_sampletype, Sample_material.vector = samplematerial)



