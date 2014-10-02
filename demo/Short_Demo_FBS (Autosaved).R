rm(list=ls())
gc()


remove.packages("conSTable") 
detach("package:conSTable")


##### Installing the package "conSTable"  ########
# The first step before installing the package consists to download and require the package "devtools"

#install.packages("devtools")

library(devtools)

install_github("conSTable",username="mrpozzi",ref="master")
#install_github("conSTable",username="marcogarieri",ref="master")

# The we can load the package

library(conSTable)


## Set directory with data
# setwd("Documents/conSTable/data/")

# file <- "ContTab.csv"
file <- "commodityContTab.csv"
file_g <- "groupContTab.csv"
file0 <- "Comm.SZ.csv"
filef <- "FeedConstraint.csv"
FBS <- readFBS(file,file0,filef)
FBS_g <- readFBS_group(file_g,file0,filef) 


# The command shows the data for Congo in 2008 
FBS_g[["1"]][["2008"]]


balanceOne <- balanceFBS(FBS_g)
balanceOne("1",2008,oset=c(30,30,40,50,50,10000),prop=NULL, nIter = 10,objF = function(tab){-colSums(tab)[1]},verbose=TRUE,checks="Import")
balanceOne("Congo",2008,oset=c(30,30,40,50,50,10000),prop=NULL, nIter = 10,objF = function(tab){-colSums(tab)[1]},verbose=TRUE)
balanceCountry(FBS,"Congo",oset=c(30,30,40,50,50,10000),prop=NULL, nIter = 10,verbose=TRUE)
balanceAll(FBS,oset=c(30,30,40,50,50,10000),ncores=1)
