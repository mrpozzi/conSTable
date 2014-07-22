balanceFBS <- function(FBS){
	
	function(Country, year,oset,...){
		
		fbs <- FBS[[Country]][[as.character(year)]]
		mu_Tab <- fbs$data
		row_Tot <- fbs$row_Tot
		

		tab <- conSTable(muTab=mu_Tab, rowTot=row_Tot, shift=cbind(fbs$sd,t(oset%*%t(rep(1,nrow(mu_Tab))))), ...)
       tab

		
		}
	}


balanceCountry <- function(FBS,Country,oset,objFun = function(tab){-colSums(tab)[1]},...){
	balanceFBS <- balanceFBS(FBS)
	res <- list()
	oldObj <- 0.0
	for(year in names(FBS[[Country]])){
		tab <- balanceFBS(Country,2008,oset,objFun = function(tab){-abs(objFun(tab)-oldObj)},...)
		oldObj <- tab@objective
		res[[year]] <- tab
		}
	res
	}

	
balanceAll <- function(FBS,oset,ncores=2L,...){
	require("parallel")
	mclapply(names(FBS), function(Country) balanceCountry(FBS,Country,oset,...), mc.cores=ncores)
	}
	
