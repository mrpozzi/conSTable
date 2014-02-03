rmseObj <-
function(obsTable){
	function(tab){
		sqrt((mean((tab-obsTable)^2)))
		}
	}

rrmseObj <-
function(obsTable){
	function(tab){
		sqrt((mean(((tab-obsTable)/(obsTable+2*sqrt(.Machine$double.eps)))^2)))
		}
	}
