## Here we are close, but that's not exactly what it's needed

balanceFBS <- function(FBS){
	function(Country, year,oset,objF=NULL,feedShift=20,stockShift=20,writeTable=FALSE,...){
				
		if(!is.character(year)) year <- as.character(year)
		if(is.character(Country)&&is.na(suppressWarnings(as.numeric(Country)))){Country <- attr(FBS,"countryMap")[Country]
			} else if(is.numeric(Country)){
				Country <- as.character(Country)
				}
		
		fbs <- FBS[[Country]][[year]]
		mu_Tab <- fbs$data
		row_Tot <- fbs$row_Tot
		feed <- fbs$feed
		if(is.null(objF)) objF <- function(tab){-colSums(tab)["Food"]}
		objFeed <- function(feed, objF){
			if(length(feed)>0){
				bounds <- feed * (1+feedShift/100*c(-1,1))
				} else {
					bounds <- c(-Inf,Inf)
					}
			function(tab){
				if(colSums(tab)["Feed"]<bounds[1]||colSums(tab)["Feed"]>bounds[2]){
					return(-Inf)
					} else {
						return(objF(tab))
					}
				}
			}
		tab <- conSTable(muTab=mu_Tab, rowTot=row_Tot, shift=cbind(fbs$sd,t(oset%*%t(rep(1,nrow(mu_Tab))))),objFun=objFeed(feed, objF),stkSft=stockShift,...)
       	if(!is.null(tab)) attr(tab,"Production") <- row_Tot
       	
       	if(writeTable){
       		who <- names(attr(FBS,"countryMap")[match(Country,attr(FBS,"countryMap"))])
       		when <- year
       		tbl <- x@bestTab
       		tbl[,colnames(tbl)!="Stock"] <- abs(tbl[,colnames(tbl)!="Stock"])
	write.table(tbl,file=paste(who,when,".csv",sep=""),sep=",",col.names=NA,row.names=T)
       	}
	return(invisible(tab))
		
		}
	}


#### STOCKSHIFT, WE HAVE TO TALK ABOUT IT, FOR balanceOne 


balanceCountry <- function(FBS,Country,oset,feedShift=20,stockShift=20,...){
	balanceFBS <- balanceFBS(FBS,feedShift)
	
	if(is.character(Country)&&is.na(suppressWarnings(as.numeric(Country)))){Country <- attr(FBS,"countryMap")[Country]
			} else if(is.numeric(Country)){
				Country <- as.character(Country)
				}
	
	objectiveFun <- function(tab2=NULL){
		if(!is.null(tab2)){
			oldTot <- sum(tab2[,"Food"])
			} 
			funK <- function(tab){
				n0 <- attr(tab,"Production")
				totFood <- -sum(tab[,"Food"])
				if(!is.null(tab2)){
					cond <- (abs(totFood + oldTot) >= 150)
					}else {
						cond <- FALSE
						}
				if(any(tab[,"Stock"] > stockShift/100 * (n0 + tab[,"Imports"] - tab[,"Exports"])) || any(tab[,"Exports"] > n0 + tab[,"Imports"]) || cond) {
					return(-Inf)
					}
				if(totFood > 3000){
					return(min(tab[,"Food"])) 
					} else {
						return(totFood)
						}
					}
					attr(funK,"objName") <- "tot. Food"
					funK
				}

	res <- list()
	yearOld <- "NULL"
	for(year in sort(names(FBS[[Country]]))){
		cat("Balancing year ",year," (Country ",names(attr(FBS,"countryMap"))[attr(FBS,"countryMap")==Country],")\n")
		tab <- balanceFBS(Country,year,oset,objF = objectiveFun(res[[yearOld]]$bestTab),...)
		if(is.null(tab)) warning(paste("Failed to match condition for year",year,"Country",names(attr(FBS,"countryMap"))[attr(FBS,"countryMap")==Country],sep=" "))
		res[[year]] <- tab
		yearOld <- year
		}
	return(invisible(res))
	}

#	Balancing year  2011  (Country  Kazakhstan )

balanceAll <- function(FBS,oset,ncores=1L,feedShift=20,stockShift=20,...){
	require("parallel")
	return(invisible(mclapply(names(FBS), function(Country){
			cat("Balancing Country",names(attr(FBS,"countryMap"))[attr(FBS,"countryMap")==Country],"\n")
		 balanceCountry(FBS,Country,oset,feedShift,stockShift,...)
		 }, mc.cores=ncores)))
	
	}
	
