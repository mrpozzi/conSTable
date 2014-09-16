readFBS <- function(file,file0=NULL,whichCols=c("Imports.primary","Exports.primary","Feed.use","Seed.use","Losses","Industrial.use","Food.use","Stock.change"),fixed="Production",sdCols=c("Imports.sd","Exports.sd"),whichRowsNot=c("GRAND TOTAL")){
	
	rawData <- scan(file, what="", sep="\n",quote="\"")
	header <- rawData[1]; rawData <- rawData[-1]
	header  <- unlist(strsplit(header, ","))
	rawData <- strsplit(rawData, ",")
	rawData <- rawData[sapply(rawData,length)!=1]
	
	structZero <- NULL
	if(!is.null(file0)){
		structZero <- read.csv(file0, row.names=2)[,-1]!="num"
	}
	feedConstraints <- NULL
	if(!is.null(filef)){
		feedConstraints <- read.csv(filef)
	}
	
	countries <- sapply(rawData, `[`,2)
	countryData <- tapply(rawData, countries,function(country){
		years <- sapply(country, `[`,6)
		yearData <- tapply(country,years,function(year){
			nm <- sapply(year,`[`,5)
			fbs <- t(sapply(year,function(y) as.numeric(y[-(1:6)])))
			rownames(fbs) <- nm
			colnames(fbs) <- header[-(1:5)]
			fbs[, whichCols[1:2]][fbs[,sdCols]==0] <- NA
			fbs[, whichCols[1]] <- -fbs[, whichCols[1]]	
			# if(!is.null(structZero)){
				# fbs[, whichCols[-(1:2)]][fbs[, whichCols[-(1:2)]]==0 & structZero[rownames(fbs),]] <- NA
				# }
			codeYear <- apply(sapply(year,`[`,c(3,6)),1,unique)
			## Should we remove GRAND TOTAL?
			## Since we made the control on the total of the columns, then we need to remove GRAND TOTAL
			list(data=fbs[!rownames(fbs) %in% whichRowsNot, whichCols],row_Tot=fbs[!rownames(fbs) %in% whichRowsNot,fixed],sd=fbs[!rownames(fbs) %in% whichRowsNot,sdCols],feed=feedConstraints[feedConstraints[,1]== codeYear[1]&feedConstraints[,2]==codeYear[2],3])
			})
		
		yearData
		
		})
		
	countryData
	
	}

