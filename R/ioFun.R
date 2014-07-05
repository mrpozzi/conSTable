readFBS <- function(file){
	rawData <- scan(file, what="", sep="\n",quote="\"")
	header <- rawData[1]; rawData <- rawData[-1]
	header  <- unlist(strsplit(header, ","))
	rawData <- strsplit(rawData, ",")
	rawData <- rawData[sapply(rawData,length)!=1]
	
	countries <- sapply(rawData, `[`,2)
	countryData <- tapply(rawData, countries,function(country){
		
		years <- sapply(country, `[`,4)
		yearData <- tapply(country,years,function(year){
			nm <- sapply(year,`[`,3)
			fbs <- t(sapply(year,function(y) as.numeric(y[-(1:4)])))
			rownames(fbs) <- nm
			colnames(fbs) <- header[-(1:4)]
			fbs
			})
		
		yearData
		
		})
		
	countryData
	
	}



# file <- "ContTab.csv"
# file <- "ContTabClean.csv"
 # FBSdata <- readFBS(file)
