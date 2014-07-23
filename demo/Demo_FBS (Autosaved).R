rm(list=ls())
gc()


##### Installing the package "conSTable"  ########

# The first step before installing the package consists to download and require tha package "devtools"

#install.packages("devtools")
library(devtools)

# Now we are read to install the package "conSTable" from the private github  repository of "mrpozzi"

install_github("conSTable",username="mrpozzi",ref="master")

# Please, before you install a new version of the package be sure to remove the previous version of the same package 

#remove.packages("conSTable") 
#detach("package:conSTable")


# The we can load the package

library(conSTable)


# we create a list, called FBS,  containing our data
# with NA we indicate the "structural zeros"



# file <- "ContTab.csv"
file <- "ContTabClean.csv"
file0 <- "struct_zero.csv"
FBS <- readFBS(file,file0)


# The command
FBS[["Congo"]][[as.character(2008)]]

# shows the data for Congo in 2008 


fillFBS <- function(FBS){
	
	function(Country, year,...){
		
		fbs <- FBS[[Country]][[as.character(year)]]
		
		## mu_Tab is the matrix of expected value for a country in time t
		## e.g.
		## Country <- "Congo"
		## year <- 2008 
		## mu_Tab
		mu_Tab <- fbs$data #do.call(rbind,lapply(names(fbs$data),function(name)fbs$data[[name]]))

	    
	    ## assignment of row and column names for the table mu_tab
	    # dimnames(mu_Tab) <- list(names(fbs$data),c("Food","Feed","OtherUse","Seed","Losses","StVar"))
	    
		## row_Tot indicates the vector of row totals given by user (FAO experts) that mach
		## to the totals shown in column I ("Domestic Supply") of the excell sheet.
		## In other words, these are the values (fixed and true) represent the constraints of our rows
		row_Tot <- fbs$row_Tot
		

		## At this point we need to define the bounds: lower and upper. 
		## The function has two options that can alternatively be used to obtain the bounds:
		## 
		## prop: takes a value between 0 and 1. Can be a scalar or a vector of size equal to the number of columns of mu_Tab.
		## 
		##       - prop=0.1  means that the bounds are constructed by adding and subtracting to each cell of mu_Tab the 10% of the 
		##                   expected value: mu_Tab±10%   
		## 
		##       - prop=c(0.1,0.1,0.2,0.1,0.2,0.5) means that the bounds for each column of mu_Tab are obtained in this way:
		##              ^ for the fisrt column, by adding and subtracting to each cell of the column the 10% of the corresponding expected value in mu_Tab;
		##              ^ for the second column, by adding and subtracting to each cell of the column the 10% of the corresponding expected value in mu_Tab;
		##              ^ for the third column, by adding and subtracting to each cell of the column the 20% of the corresponding expected value in mu_Tab;
		##              ^ for the fourth column, by adding and subtracting to each cell of the column the 10% of the corresponding expected value in mu_Tab;
		##              ^ for the fifth column, by adding and subtracting to each cell of the column the 20% of the corresponding expected value in mu_Tab;
		##              ^ for the sixth column, by adding and subtracting to each cell of the column the 50% of the corresponding expected value in mu_Tab;
		##
		## shift: takes values > 0. Can be a scalar or a vector of size equal to the number of columns of mu_Tab. 
		## 
		##       - shift=100  means that the bounds are constructed by adding and subtracting to each cell of mu_Tab the value 100   
		## 
		##       - shift=c(100,50,200,100,20,500) means that the bounds for each column of mu_Tab are obtained in this way:
		##              ^ for the fisrt column, by adding and subtracting to each cell of the column the value 10
		##              ^ for the second column, by adding and subtracting to each cell of the column the value 50
		##              ^ for the third column, by adding and subtracting to each cell of the column the value 200
		##              ^ for the fourth column, by adding and subtracting to each cell of the column the value 100
		##              ^ for the fifth column, by adding and subtracting to each cell of the column the value 20
		##              ^ for the sixth column, by adding and subtracting to each cell of the column the value 50



		
		## nIter: indicates the number of iteration, and then the number of "balanced" tables that we want to generate 

		## Finally, we can define a criterion by which we choose our best "balanced" table. 
		## For instance, with the option 
		## objFun = function(tab){-colSums(tab)[1]} 
		## we are asking our algorithm to provide as output the table for which is minimun the total of column 1 ("Food") 
		## In contrast, with the command
		## objFun = function(tab){colSums(tab)[1]} 
		## we are asking our algorithm to provide as output the table for which is maximum the total of column 1 ("Food") 
		
		tab <- conSTable(muTab=mu_Tab, rowTot=row_Tot, prop=NULL, shift=cbind(fbs$sd,t(c(20,30,30,40,50,50,10000)%*%t(rep(1,nrow(mu_Tab))))),nIter = 10,objFun = function(tab){-colSums(tab)[1]},verbose=TRUE,...)
       tab

		
		}
	}


fillFBS <- fillFBS(FBS)
fillFBS("Congo",2008)



balanceOne <- balanceFBS(FBS)
balanceOne("Congo",2008,oset=c(30,30,40,50,50,1000),prop=NULL, nIter = 10,objFun = function(tab){-colSums(tab)[1]},verbose=TRUE)
balanceCountry(FBS,"Congo",oset=c(30,30,40,50,50,10000),prop=NULL, nIter = 10,verbose=TRUE)

