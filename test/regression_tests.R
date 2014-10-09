library(conSTable)


## Set directory with data

# file <- "ContTab.csv"
file <- "../data/commodityContTab.csv"
file_g <- "../data/groupContTab.csv"
file0 <- "../data/Comm.SZ.csv"
filef <- "../data/FeedConstraint.csv"
FBS <- readFBS(file,file0,filef)
FBS_g <- readFBS_group(file_g,file0,filef) 


balanceOne <- balanceFBS(FBS)#,feedShift=20)

a <- balanceOne("9",2012,oset=c(0,0,0,0,0,10000),prop=NULL, nIter = 100,verbose=FALSE,checks="none",feedShift=30)
ss <- a@bestTab


if(all(abs(apply(ss,1,function(x){ sum(x[!names(x)%in%c("Production","Stock")])-x["Production"]-x["Stock"]}))<sqrt(.Machine$double.eps))) {
	cat("Passed\n")
	} else {
	cat("Not Passed\n")	
	print(head(abs(apply(ss,1,function(x){ sum(x[!names(x)%in%c("Production","Stock")])-x["Production"]-x["Stock"]}))[apply(ss,1,function(x){ sum(x[!names(x)%in%c("Production","Stock")])-x["Production"]-x["Stock"]})>sqrt(.Machine$double.eps)]))
	}