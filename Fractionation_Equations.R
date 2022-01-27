# ***************************************************** # 
# FRACTIONATION EQUATIONS ----------------------------- #
# for dealing with compilation data                     #
#                                                       #
# T. Kukla (Colo State University, 2022)                #
#                                                       #
# Date Created: Jan 15, 2019                            #
# Last Modified: Jan 22, 2021                           #
# ***************************************************** #


## NOTE: Some of these fractionation equations may be outdated! Please check for the latest or
## standard use equations in the literature (this list is certainly not comprehensive).

#... Use these terms to call a given fractionation equation (terms can be added in an extra column of database...
#    ... then master fractionation function can be used to quickly calculate isotopic compositions of water.
precipCalc_terms <- c('Kim_Oneil', 'SG_Smectite_O', 'SG_Kaolinite_O', 'SG_Smectite_H', 'SG_Kaolinite_H', 
                      'SG_Smectite_OH', 'SG_Kaolinite_OH', 'BF95_Model', 'Friedman_Glass', 'water',
                      'GK86D99_Arag', 'KIM07', 'LN73_Phos', 'KE76_Chert', 'CN03_nAlk', 'KD07')

## FUNCTION to calculate meteoric water no matter what fractionation equation we ise 
#... [1] Tmp == temperature in degrees K 
#... [2] dat == isotopic composition of interest
#... [3] precipCalc_fxn == the term used in the compilation for what equation to employ
#... [4] isoFlag == 'd18O' or 'dD' depending on what you'd like to calculate

MASTER_FRACTIONATION <- function(Tmp, dat, precipCalc_fxn, isoFlag = 'd18O'){
  pCalc <- c('Kim_Oneil', 'SG_Smectite_O', 'SG_Kaolinite_O', 'SG_Smectite_H', 'SG_Kaolinite_H', 
             'SG_Smectite_OH', 'SG_Kaolinite_OH', 'BF95_Model', 'Friedman_Glass', 'water',
             'GK86D99_Arag', 'KIM07', 'LN73_Phos', 'KE76_Chert', 'CN03_nAlk', 'KD07')
  
  if(precipCalc_fxn==pCalc[1]){
    KimOneil(Tmp=Tmp, dat=dat)
  } else if(precipCalc_fxn==pCalc[2] | precipCalc_fxn==pCalc[6] & isoFlag=='d18O'){
    SG_Smect_O(Tmp=Tmp, dat=dat)
  } else if(precipCalc_fxn==pCalc[3] | precipCalc_fxn==pCalc[7] & isoFlag=='d18O'){
    SG_Kaol_O(Tmp=Tmp, dat=dat)
  } else if(precipCalc_fxn==pCalc[4] | precipCalc_fxn==pCalc[5] | precipCalc_fxn==pCalc[6] | precipCalc_fxn==pCalc[7]  & isoFlag=='dD'){
    SG_Kaol_H(Tmp=Tmp, dat=dat)
  } else if(precipCalc_fxn==pCalc[8]){
    print('BF95 model not yet built')
    return(NA)
  } else if(precipCalc_fxn==pCalc[9]){
    Friedman_GlassH(dat=dat)
  } else if(precipCalc_fxn==pCalc[10]){
    print("Error -- will not override the given water value")
    return(NA)
  } else if(precipCalc_fxn==pCalc[11]){
    GK86D99_Arag(Tmp=Tmp, dat=dat)
  } else if(precipCalc_fxn==pCalc[12]){
    KIM07_Arag(Tmp=Tmp, dat=dat)
  } else if(precipCalc_fxn==pCalc[13]){
    LN73_Phos(Tmp=Tmp, dat=dat)
  } else if(precipCalc_fxn==pCalc[14]){
    KE76_chert(Tmp=Tmp, dat=dat)
  } else if(precipCalc_fxn==pCalc[15]){
    CN03_nalk(dat=dat)
  } else if(precipCalc_fxn==pCalc[16]){
    KD07_Arag(dat=dat)
  } else{print("------ precip calculator not found ------")}
  
  
}


#... individual ones
## Kim and O'Neil, 1997; GCA
KimOneil <- function(Tmp=(273.15 + 25), dat){
  dat - ((18.03 * (10^3)/Tmp) - 32.42)
}
## Sheppard and Gilg, 1996; Clay Minerals
SG_Smect_O <- function(Tmp=(273.15 + 25), dat){
  dat - (2.55 * (10^6) * (Tmp^-2)) - 4.05
}
## Sheppard and Gilg, 1996; Clay Minerals
SG_Kaol_O <- function(Tmp=(273.15 + 25), dat){
  dat - (2.76 * (10^6) * (Tmp^-2)) -6.75
}
## Sheppard and Gilg, 1996; Clay Minerals
SG_Kaol_H <- function(Tmp=(273.15 + 25), dat){
  dat - (-2.2 * (10^6) * (Tmp^-2)) - 7.7
}
## Friedman et al., 1993; Clim Change in Cont. Records
Friedman_GlassH <- function(dat){
  dat - 1000 * log(0.9668)
}
## Grossman & Ku, 1986; Chem. Geol || Dettman et al., 1999; GCA
GK86D99_Arag <- function(Tmp=273.15+21, dat){    # use an assumed avg temp of fm of 21 degrees C for bivalves after Dettman and Lohmann 2000
  dat - (2.559 * 10^6 * (T^-2)) + 0.715
}
## Kohn and Dettman, 2007; Reviews in mineralogy and geochemistry
KD07_Arag <- function(dat){  # This includes going from VSMOW to VPDB as the equation is written for VPDB in Kohn and Dettman 2007 (Fig 5)
  (((dat - 30.91) / 1.03091) + 0.98) / 0.89
}
## Kim et al., 2007; GCA
KIM07_Arag <- function(Tmp, dat){   # from Kim et al., 2007 for water - aragonite
  dat - ((17.88 + (10^3) / (Tmp)) - 31.14)
}
## Longinelli and Nuti, 1973; EPSL
LN73_Phos <- function(Tmp, dat){    # Phosphate water fractionation relationship of Longinelli and Nuti 1973 (copied from Fricke and Wing 2004 AJS page 616)
  (111.4 + (4.3 * dat) - Tmp) / 4.3
}
## Knauth and Epstein, 1976; GCA
KE76_chert <- function(Tmp, dat){   # from Knauth and Epstein 1976
  dat - (3.09 * (10^6) * (Tmp^-2) - 3.29)
}
## Chikaraishi and Naraoka, 2003 (Phytochemistry)
CN03_nalk <- function(dat){ # from Chikaraishi and Naraoka, 2003 (Phytochemistry) for C3 Angiosperms (uncertainty not included) this paper also has epsilon values for other plant groups
  dat - -117
}



