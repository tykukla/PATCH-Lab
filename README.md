# PATCH-Lab
Code and data associated with The PATCH Lab web portal

## Introduction
This readme steps through what is contained in each script and what is needed to run them.

### DataEntry_DiagnosticsFXNs.R
This is the set of diagnostic functions that are used in the diagnostic tool in The PATCH Lab data upload portal. You can read in a .csv of downloaded data and one of the data you hope to submit to test for compatibility offline. The code is not well-commented, so it is probably easiest to utilize the function from the online portal. This file is mostly so you can check out what's behind the diagnostic test. 

### Fractionation_Equations.R
One common application of the isotope data may be to produce isotope ratios of the fluid that a given proxy material formed in contact with. This is challenging when a wide range of proxy types, each with a different fractionation equation, are used. This script defines a bunch of fractionation equations. You can add a column to hold the names of these equations in your .csv file. Then use the MASTER function of this script to quickly calculate fluid isotopic compositions by applying the correct equation to each row. 

Note that some of the equations may be out of date. If you have experience with R, I think it will be pretty straightforward to update the existing equations and add new ones. However, please reach out to me if you'd like any help. (Also, please let me know if I can fix or add any equations to this script). 

### Query-Example.R
This script goes with the Query-Fxns script below. It is an example of how you can use the query functions to filter data by sample type, age, and geography. 

### Query-Fxns.R
These functions should be sourced into the Query-Example.R script. They filter the data and, for quantitative filers, display a plot that shows the filtered versus non-filtered data to quickly check that it worked correctly. 

### Miscellaneous
I spent a good couple of years of searching on and off trying to find an easy way to plot a Robinson projection map in ggplot. Finally found one that seems to work well, in case you're interested too: https://gist.github.com/valentinitnelav/065af8eba2d9455d9407e5d3890f6f86.
