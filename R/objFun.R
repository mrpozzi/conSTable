rmseObj <-
function(obsTable){
	funK <- function(tab){
		sqrt((mean((tab-obsTable)^2)))
		}
	attr(funK,"objName") <- "RMSE"
	funK
	}

rrmseObj <-
function(obsTable){
	funK <- function(tab){
		sqrt((mean(((tab-obsTable)/(obsTable+2*sqrt(.Machine$double.eps)))^2)))
		}
	attr(funK,"objName") <- "RRMSE"
	funK
	}
