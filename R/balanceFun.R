## Here we are close, but that's not exactly what it's needed

balanceFBS <- function(FBS){
	
	function(Country, year,oset,...){
		
		fbs <- FBS[[Country]][[as.character(year)]]
		mu_Tab <- fbs$data
		row_Tot <- fbs$row_Tot
		

		tab <- conSTable(muTab=mu_Tab, rowTot=row_Tot, shift=cbind(fbs$sd,t(oset%*%t(rep(1,nrow(mu_Tab))))), ...)
       	tab

		
		}
	}


balanceCountry <- function(FBS,Country,oset,...){
	balanceFBS <- balanceFBS(FBS)
	
	objectiveFun <- function(tab2=NULL){
		if(!is.null(tab2)){
			oldTot <- sum(tab2[,"Food"])
			} 
			function(tab){
				totFood <- -sum(tab[,"Food"])
				if(!is.null(tab2)){
					cond <- (abs(totFood + oldTot) >= 150)
					}else {
						cond <- FALSE
						}
				if(any(tab[,"dStock"] > 0.2 * (n0 + tab[,"Imports.primary"] - tab[,"Exports.primary"])) || any(tab[,"Exports.primary"] > n0 + tab[,"Imports.primary"]) || cond)) {
					return(-Inf)
					}
				if(totFood > 3000){
					return(min(tab[,"Food"])) 
					} else {
						return(totFood)
						}
					}
				}
				
	res <- list()
	yearOld <- "NULL"
	for(year in sort(names(FBS[[Country]]))){
		cat("Balancing year",year,"(Country",Country,")")
		tab <- balanceFBS(Country,year,oset,objFun = objectiveFun(res[[yearOld]]),...)
		oldObj <- tab@objective
		res[[year]] <- tab
		yearOld <- year
		}
	res
	}

	
balanceAll <- function(FBS,oset,ncores=2L,...){
	require("parallel")
	#mclapply(names(FBS), function(Country) balanceCountry(FBS,Country,oset,...), mc.cores=ncores)
	lapply(names(FBS), function(Country){
		cat("Balancing Country",Country)
		 balanceCountry(FBS,Country,oset,...)
		 })
	}
	
