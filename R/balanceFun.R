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
	objectiveFun <- function(tab){
		totFood <- -sum(tab[,"Food"])
		if(any(bestTab[,"dStock"] > 0.2 * (n0 + bestTab[,"Imports.primary"] - bestTab[,"Exports.primary"]))) {
			return(-Inf)
			}
		if(totFood > 3000){
			return(min(tab[,"Food"])+max(apply(tab[,c("Feed","Bio","dStock","Loss")],1,max)))
			} else {
				return(totFood)
				}
		}
	res <- list()
	oldObj <- 0.0
	for(year in sort(names(FBS[[Country]]))){
		tab <- balanceFBS(Country,year,oset,objFun = objectiveFun,...)
		oldObj <- tab@objective
		res[[year]] <- tab
		
		objectiveFun <- function(tab2){
			function(tab){
				totFood <- -sum(tab[,"Food"]) + sum(tab2[,"Food"])
				if(any(bestTab[,"dStock"] > 0.2 * (n0 + bestTab[,"Imports.primary"] - bestTab[,"Exports.primary"]))) {
					return(-Inf)
					}
				if(totFood > 3000){
					return(min(tab[,"Food"])+max(apply(tab[,c("Feed","Bio","dStock","Loss")],1,max)))
					} else {
						return(totFood)
						}
					}
				}
		objectiveFun <- objectiveFun(tab@bestTab)
		
		}
	res
	}

	
balanceAll <- function(FBS,oset,ncores=2L,...){
	require("parallel")
	mclapply(names(FBS), function(Country) balanceCountry(FBS,Country,oset,...), mc.cores=ncores)
	}
	
