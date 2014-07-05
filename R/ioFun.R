readFBS <- function(file){
	rawData <- scan(file, what="", sep="\n",quote="\"")
	rawData <- strsplit(rawData, ",")
	
	countries <- sapply(rawData, `[`,2)
	
	countryData <- tapply(rawData, countries,function(country){
		
		years <- sapply(country, `[`,4)
		yearData <- tapply(country,years,function(year){
			nm <- sapply(year,`[`,3)
			stuff <- sapply(year,function(y)as.numeric(y)[5:length(y)])
			colnames(stuff) <- nm
			stuff
			})
		yearData
		})
	
	countryData
	
	}


# file <- "ContTab (2).csv"
# FBSdata <- readFBS(file)
