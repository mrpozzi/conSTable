rm(list=ls())
gc()


#remove.packages("conSTable") 
#detach("package:conSTable")


##### Installing the package "conSTable"  ########
# The first step before installing the package consists to download and require tha package "devtools"

#install.packages("devtools")
library(devtools)

install_github("conSTable",username="mrpozzi",ref="master")

# The we can load the package

library(conSTable)


## Set directory with data
# setwd("Documents/conSTable/data/")

# file <- "ContTab.csv"
file <- "ContTabClean.csv"
file0 <- "struct_zero.csv"
FBS <- readFBS(file,file0)


# The command shows the data for Congo in 2008 
FBS[["Congo"]][[as.character(2008)]]


balanceOne <- balanceFBS(FBS)
balanceOne("Congo",2008,oset=c(30,30,40,50,50,10000),prop=NULL, nIter = 10,objFun = function(tab){-colSums(tab)[1]},verbose=TRUE)
balanceCountry(FBS,"Congo",oset=c(30,30,40,50,50,10000),prop=NULL, nIter = 10,verbose=TRUE)
balanceAll(FBS,oset=c(30,30,40,50,50,10000))
