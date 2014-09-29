## Here we are close, but that's not exactly what it's needed

balanceFBS <- function(FBS){
	function(Country, year,oset,objF=NULL,...){
				
		if(!is.character(year)) year <- as.character(year)
		if(is.character(Country)) Country <- attr(FBS,"countryMap")[Country]
		
		fbs <- FBS[[Country]][[year]]
		mu_Tab <- fbs$data
		row_Tot <- fbs$row_Tot
		feed <- fbs$feed
		if(is.null(objF)) objF <- function(tab){-colSums(tab)["Food"]}
		objFeed <- function(feed, objF){
			bounds <- feed * c(0.8,1.2)
			function(tab){
				if(colSums(tab)["Feed"]<bounds[1]||colSums(tab)["Feed"]>bounds[2]){
					return(-Inf)
					} else {
						return(objF(tab))
					}
				}
			}
		tab <- conSTable(muTab=mu_Tab, rowTot=row_Tot, shift=cbind(fbs$sd,t(oset%*%t(rep(1,nrow(mu_Tab))))),objFun=objFeed(feed, objF),...)
       	if(!is.null(tab)) attr(tab,"Production") <- row_Tot
       	tab

		
		}
	}


balanceCountry <- function(FBS,Country,oset,...){
	balanceFBS <- balanceFBS(FBS)
	
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
				if(any(tab[,"Stock"] > 0.2 * (n0 + tab[,"Imports"] - tab[,"Exports"])) || any(tab[,"Exports"] > n0 + tab[,"Imports"]) || cond) {
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
		cat("Balancing year ",year," (Country ",Country,")")
		tab <- balanceFBS(Country,year,oset,objF = objectiveFun(res[[yearOld]]$bestTab),...)
		if(is.null(tab)) warning(paste("Failed to match condition for year",year,"Country",Country,sep=" "))
		res[[year]] <- tab
		yearOld <- year
		}
	res
	}

	
balanceAll <- function(FBS,oset,ncores=1L,...){
	require("parallel")
	mclapply(names(FBS), function(Country){
		cat("Balancing Country",Country)
		 balanceCountry(FBS,Country,oset,...)
		 }, mc.cores=ncores)
	}
	
