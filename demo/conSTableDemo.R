rm(list=ls())
gc()

library("conSTable")

data("Italy2006FBsheet")

# List of loaded data regarding a FBS of Italy in 2006
ls()

## muTab: table with mean for each cell
## bounds: array with lower and upper bounds of the distribution for each cell
## n0: total of the rows (for each commodity)
## lowCol, uppCol: bounds for the totals of the colums
## Scenario: real values needed to check the results in case the objective function is the Root Mean Square Error

# Dimension of the table:
dim(muTab)
# Rows' names: all the different commodities (Cereals, Wheat, Rice, etc.)
rownames(muTab)
# Columns' names: all the different type of quantities (Feed, Seed, Waste, etc.)
colnames(muTab)

## Saving the range of the columns' total in a data.frame
#controlCol1 <- rbind(lowCol,uppCol)

## Running the main function sampleTables with default objective function to define the best table
# Objective function: minimum of the first column's total (Feed)
#system.time(tab <- sampleTables(n0,muTab,bounds,controlCol=controlCol1,verbose=FALSE))

system.time(tab <- conSTable(muTab, n0, prop=0.5,verbose=FALSE))

## Result of the function, with the optimal table, the number of the iterations and the results from the objective function (value of Feed)
tab

## Check multiplicity of the results of the iterations (100 default value)
unlist(lapply(tab@tables,function(x)attr(x,"mult")))
## As a results, each table sampled is present just once

## Resample the table, fixing the 3rd, 56th and 101st rows from the previous best table
#system.time(tab2 <- sampleTables(n0,muTab,bounds,controlCol=controlCol1,fixed=c(3,56,101),fixedRows=tab@bestTab[c(3,56,101),],verbose=FALSE))
system.time(tab2 <- conSTable(muTab, n0, prop=0.5,fixed=c(3,56,101),fixedRows=tab@bestTab[c(3,56,101),],verbose=FALSE))

## Control of the fixed rows
tab@bestTab[c(3,56,101),]
tab2@bestTab[c(3,56,101),]

## Run the main function, with a different objective function
# Objective function: minimize difference between sampled values and the real values stored in Scenario, measure with the Root Mean Square Error (MSE)
#system.time(tab3 <- sampleTables(n0,muTab,bounds,controlCol=controlCol1,objFun=rmseObj(Scenario),verbose=FALSE))
system.time(tab3 <- conSTable(muTab, n0, prop=0.5,objFun=rmseObj(Scenario),verbose=FALSE))
## Results
tab3

m0 <- colSums(muTab)
system.time(tab4 <- conSTable(muTab, m0, prop=0.5,verbose=FALSE,transpose=TRUE))
tab4


