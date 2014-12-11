## Here we are close, but that's not exactly what it's needed

balanceFBS <- function(FBS,sanityCheck=FALSE,maxErr=10){
	function(Country, year,oset=NULL,objF=NULL,feedShift=20,stockShift=20,writeTable=FALSE,...){
				
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
		
		if(sanityCheck) {
			fe <- sum(fbs$data[,3],na.rm=T)
			if(fe<fbs$feed[1] || fe>fbs$feed[2]) {
				warning(paste("Feed falls out of boundaries: ",fe," [",fbs$feed[1],", ",fbs$feed[2],"]"))
				}
			}
		
		objFeed <- function(feed, objF){
			bounds <- feed
			if(length(feed)==1){
				bounds <- feed * (1+feedShift/100*c(-1,1))
				} else if(length(feed)!=2) {
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

		if(is.null(oset)) {
			oset <- colSums(mu_Tab[,-c(1:2)],na.rm=T)*20/100
			oset["Feed"] <- sum(fbs$data[,3],na.rm=T)*min(1-feed[1],feed[2]-1)
			oset <- abs(oset)
		}
		
		tab <- conSTable(muTab=mu_Tab, rowTot=row_Tot, shift=cbind(fbs$sd,t(oset%*%t(rep(1,nrow(mu_Tab))))),objFun=objFeed(feed, objF),stkSft=stockShift,...)
       	if(!is.null(tab)) attr(tab,"Production") <- row_Tot
       	
       	count <- 0
       	if(sanityCheck) {
       		while(count < maxErr) {
       			if(!is.null(tab)) {
       				fe <- sum(tab@bestTab[,"Feed"])
       				if(fe<fbs$feed[1] || fe>fbs$feed[2]) {
       					warning(paste("Feed falls out of boundaries: ",fe," [",fbs$feed[1],", ",fbs$feed[2],"]. Shrinking bounds."))
       					oset["Feed"] <- oset["Feed"]/2
       					} else break
       				} else count <- count + 1
       			tab <- conSTable(muTab=mu_Tab, rowTot=row_Tot, shift=cbind(fbs$sd,t(oset%*%t(rep(1,nrow(mu_Tab))))),objFun=objFeed(feed, objF),stkSft=stockShift,...)
       			if(!is.null(tab)) attr(tab,"Production") <- row_Tot
       			}
       		}
       	
       	if(writeTable&!is.null(tab)){
       		who <- names(attr(FBS,"countryMap")[match(Country,attr(FBS,"countryMap"))])
       		when <- year
       		tbl <- tab@bestTab
       		tbl[,colnames(tbl)!="Stock"] <- abs(tbl[,colnames(tbl)!="Stock"])
	write.table(tbl,file=paste(who,when,".csv",sep=""),sep=",",col.names=NA,row.names=T)
       	}
	return(invisible(tab))
		
		}
	}


#### STOCKSHIFT, WE HAVE TO TALK ABOUT IT, FOR balanceOne 


balanceCountry <- function(FBS,Country,oset,feedShift=20,stockShift=20,sanityCheck=FALSE,...){
	balanceFBS <- balanceFBS(FBS,sanityCheck=sanityCheck)
	
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
					## Here I change from + to - oldTot, since we want that the difference between years has to be less than 150
					cond <- (abs(totFood - oldTot) >= 150)
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
		tab <- balanceFBS(Country,year,oset,objF = objectiveFun(res[[yearOld]]$bestTab),feedShift=feedShift,...)
		if(is.null(tab)) warning(paste("Failed to match condition for year",year,"Country",names(attr(FBS,"countryMap"))[attr(FBS,"countryMap")==Country],sep=" "))
		res[[year]] <- tab
		yearOld <- year
		}
	return(invisible(res))
	}

#	Balancing year  2011  (Country  Kazakhstan )

balanceAll <- function(FBS,oset,ncores=1L,feedShift=20,stockShift=20,sanityCheck=FALSE,...){
	require("parallel")
	return(invisible(mclapply(names(FBS), function(Country){
			cat("Balancing Country",names(attr(FBS,"countryMap"))[attr(FBS,"countryMap")==Country],"\n")
		 balanceCountry(FBS,Country,oset,feedShift,stockShift,sanityCheck=sanityCheck,...)
		 }, mc.cores=ncores)))
	
	}
	

balanceAll <- function(FBS,oset,ncores=1L,feedShift=20,stockShift=20,sanityCheck=FALSE,...){
	require("parallel")
	return(invisible(mclapply(names(FBS), function(Country){
			cat("Balancing Country",names(attr(FBS,"countryMap"))[attr(FBS,"countryMap")==Country],"\n")
		 balanceCountry(FBS,Country,oset,feedShift,stockShift,sanityCheck=sanityCheck,...)
		 }, mc.cores=ncores)))
	
	}
	
