# CHANGE FROM Imports.primary & Exports.primary to Imports.total & Exports.total
readFBS <- function(file,file0=NULL,filef,whichCols=c("Imports.total","Exports.total","Feed.use","Seed.use","Losses","Industrial.use","Food.use","Stock.change"),fixed="Production",sdCols=c("Imports.sd","Exports.sd"),whichRowsNot=c("GRAND TOTAL")){
	
	Sys.setlocale(locale="C")
	rawData <- scan(file, what="", sep="\n",quote="\"")
	header <- rawData[1]; rawData <- rawData[-1]
	header  <- unlist(strsplit(header, ","))
	rawData <- strsplit(rawData, ",")
	rawData <- rawData[sapply(rawData,length)!=1]
	
	colN <- unlist(lapply(strsplit(whichCols,".",fixed=TRUE),"[[",1))
	
	structZero <- NULL
	if(!is.null(file0)){
		structZero <- read.csv(file0, row.names=2)[,-1]!="num"
	}
	feedConstraints <- NULL
	if(!is.null(filef)){
		feedConstraints <- read.csv(filef)
		# Removing duplicated rows
		feedConstraints  <- feedConstraints[!duplicated(feedConstraints),]
	}
	countries <- sapply(rawData, `[`,2)
	names(countries) <- sapply(rawData, `[`,1)
	countryData <- tapply(rawData, countries,function(country){
		years <- sapply(country, `[`,5)
		yearData <- tapply(country,years,function(year){
			nm <- sapply(year,`[`,4)
			fbs <- t(sapply(year,function(y) as.numeric(y[-(1:5)])))
			rownames(fbs) <- nm
			colnames(fbs) <- header[-(1:5)]
			## I am making this comment because we have to read correctly imports and exports from the original data
			## PROBABLY THIS WOULD LEAD TO SOME BUGS
			#fbs[, whichCols[1:2]][fbs[,sdCols]==0] <- NA
			#browser()
			fbs[, whichCols[1]] <- -fbs[, whichCols[1]]	
			#browser()
			if(!is.null(structZero)){
				structZero <- structZero[rownames(structZero)%in%rownames(fbs),]
				structZero <- structZero[,colnames(structZero)%in%colnames(fbs[rownames(structZero), whichCols[-(1:2)]])]
				fbs[rownames(structZero),colnames(structZero)][structZero] <- NA
				}
			codeYear <- t(sapply(year,`[`,c(2,5)))
			codeYear  <- codeYear[!duplicated(codeYear,margin=2),]
			## Should we remove GRAND TOTAL?
			## Since we made the control on the total of the columns, then we need to remove GRAND TOTAL
			feed <- feedConstraints[feedConstraints[,1]==(codeYear[1])&feedConstraints[,2]==codeYear[2],3]
			if(length(feed)>1) {
				if(length(unique(feed))>1){
					warning(paste("Constraints on Feed for",year[[1]][1],year[[1]][5],"have multiple values (choosing first).",sep=" "))
					}
				feedConstraints <- unique(feedConstraints)[1]
			}
			
			ind <- duplicated(rownames(fbs))
			fbs <- fbs[!ind,]
			
			data <- fbs[!rownames(fbs) %in% whichRowsNot, whichCols]
			colnames(data) <- colN
			
			list(data=data,row_Tot=fbs[!rownames(fbs) %in% whichRowsNot,fixed],sd=fbs[!rownames(fbs) %in% whichRowsNot,sdCols],feed=feed)
			})
		
		yearData
		
		})

	attr(countryData,"countryMap") <- countries[!duplicated(countries)] #Fun Fact: unique removes names.
	countryData
	
	}
	
	
	
	
readFBS_group <- function(file,file0=NULL,filef,whichCols=c("Imports.primary","Exports.primary","Feed.use","Seed.use","Losses","Industrial.use","Food.use","Stock.change"),fixed="Production",sdCols=c("Imports.sd","Exports.sd"),whichRowsNot=c("GRAND TOTAL")){
	
	Sys.setlocale(locale="C")
	rawData <- scan(file, what="", sep="\n",quote="\"")
	header <- rawData[1]; rawData <- rawData[-1]
	header  <- unlist(strsplit(header, ","))
	rawData <- strsplit(rawData, ",")
	rawData <- rawData[sapply(rawData,length)!=1]
	
	colN <- unlist(lapply(strsplit(whichCols,".",fixed=TRUE),"[[",1))
	
	structZero <- NULL
	if(!is.null(file0)){
		structZero <- read.csv(file0, row.names=2)[,-2]!="num"
	}
	feedConstraints <- NULL
	if(!is.null(filef)){
		feedConstraints <- read.csv(filef)
		# Removing duplicated rows
		feedConstraints  <- feedConstraints[!duplicated(feedConstraints),]
	}
	countries <- sapply(rawData, `[`,2)
	names(countries) <- sapply(rawData, `[`,1)
	countryData <- tapply(rawData, countries,function(country){
		years <- sapply(country, `[`,4)
		yearData <- tapply(country,years,function(year){
			nm <- sapply(year,`[`,3)
			fbs <- t(sapply(year,function(y) as.numeric(y[-(1:4)])))
			rownames(fbs) <- nm
			colnames(fbs) <- header[-(1:4)]
			fbs[, whichCols[1:2]][fbs[,sdCols]==0] <- NA
			fbs[, whichCols[1]] <- -fbs[, whichCols[1]]	
			if(!is.null(structZero)){
				structZero <- structZero[rownames(structZero)%in%rownames(fbs),]
				structZero <- structZero[,colnames(structZero)%in%colnames(fbs[rownames(structZero), whichCols[-(1:2)]])]
				fbs[rownames(structZero),colnames(structZero)][structZero] <- NA
				}
			codeYear <- t(sapply(year,`[`,c(2,4)))
			codeYear  <- codeYear[!duplicated(codeYear,margin=2),]
			## Should we remove GRAND TOTAL?
			## Since we made the control on the total of the columns, then we need to remove GRAND TOTAL
			#browser()
			feed <- feedConstraints[feedConstraints[,1]==(codeYear[1])&feedConstraints[,2]==codeYear[2],3]
			if(length(feed)>1) {
				if(length(unique(feed))>1){
					warning(paste("Constraints on Feed for",year[[1]][1],year[[1]][5],"have multiple values (choosing first).",sep=" "))
					}
				feedConstraints <- unique(feedConstraints)[1]
			}
			data <- fbs[!rownames(fbs) %in% whichRowsNot, whichCols]
			colnames(data) <- colN
			list(data=data,row_Tot=fbs[!rownames(fbs) %in% whichRowsNot,fixed],sd=fbs[!rownames(fbs) %in% whichRowsNot,sdCols],feed=feed)
			})
		
		yearData
		
		})

	attr(countryData,"countryMap") <- countries[!duplicated(countries)] #Fun Fact: unique removes names.
	countryData
	
	}