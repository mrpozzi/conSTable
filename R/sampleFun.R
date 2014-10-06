conSTable <-
function(muTab, rowTot, prop=NULL, shift=0, controlCol, nIter=100, N=10000,sdev=5,verbose=TRUE,objFun=function(tab){-colSums(tab)[1]},fixedRows=NULL,fixed=c(),transpose=FALSE,communicate=warnings,checks=c("all","Import","Stock","none"),stkSft=NULL,...){#,keepArgs=FALSE
	
	checks <- match.arg(checks)
	
	if(transpose){
		muTab <- t(muTab)
		}
	
	# Need to remove NA in colSums
	colTot <- colSums(muTab, na.rm=T)
	colMu <- colMeans(muTab, na.rm=T)
	

  # NB: questo è il caso in cui l'user imposta sia prop diverso da NULL che shift > 0
  if(!is.null(prop) & !all(shift==0)) stop("Either prop between 0 and 1 and shift 0 or prop NULL and shift > 0")
  
	if(!is.null(prop)) {
		if(any(prop > 1)) stop("prop must be a number/vector with value/values between 0 and 1")
		} else if(all(shift==0)) stop("When prop is NULL shift must be > 0")
	
	if(!(is.null(prop) & all(shift==0))){
		
		bounds <- array(NA,c(dim(muTab),2))
		dimnames(bounds) <- c(dimnames(muTab),list(c("Lower","Upper")))
		
		if(!is.null(prop)){
			if(missing(controlCol)) controlCol <- rbind(colTot*(1-prop*sign(colTot)),colTot*(1+prop*sign(colTot)))
			
			if(!is.null(dim(prop))|(length(prop)==1)){
				# prop is a by cell proportion
				# prop is a number
				shift <- prop*sign(muTab)*muTab
				} else {
					# prop is a by column proportion
					shift <- as.matrix(do.call(cbind,lapply(1:length(prop),function(j) prop[j]*sign(muTab[,j])*muTab[,j] )))
					}
				
				} else {
					#browser()
					if(missing(controlCol)) {
						if(is.null(dim(shift))){
							controlCol <- rbind(colTot-shift,colTot+shift)
							} else {
								colShift <- apply(shift,2,max)
								controlCol <- rbind(colTot-nrow(muTab)*colShift,colTot+nrow(muTab)*colShift)
								}
						}
					
					if(is.null(dim(shift))&(length(shift)!=1)){
						# by column shift
						shift <- as.matrix(do.call(rbind,lapply(1:nrow(muTab),function(i) shift)))
						}
					}
			
			bounds[,,"Lower"] <- as.matrix(muTab - shift)
			bounds[,,"Upper"] <- as.matrix(muTab + shift)
			
			# We need to set the bounds to zero, and then muTab
			bounds[,,"Lower"][is.na(muTab)] <- bounds[,,"Upper"][is.na(muTab)] <- 0
			muTab[is.na(muTab)] <- 0
		
		 }
	
	.sampleTables(rowTot,muTab,bounds,controlCol,nIter=nIter,N=N,sdev=sdev,verbose=verbose,transpose=transpose,fixedRows=fixedRows,fixed=fixed,objFun=objFun,checks=checks,stkSft=stkSft,...)#

	}




.sampleTables <-
function(n0,muTab, bounds,controlCol=NULL,controlRow=NULL,nIter=100,N=10000,sdev=5,verbose=TRUE,objFun=function(tab){-colSums(tab)[1]}, checks="all",fixed=c(),fixedRows=NULL,transpose=FALSE,keepArgs=FALSE,communicate=warnings,stkSft=20,...){

	call <- match.call()
	
	if(keepArgs){
		argz <- lapply(2:length(call),function(i)eval(call[[i]]))
		names(argz) <- names(call)[2:length(names(call))]
		}else argz <- list()
		
	### zero rows	
	## This approach create problems
	#indZero <- unlist(lapply(1:nrow(muTab),function(i)all(muTab[i,]==0)&all(bounds[i,,1]==0)&all(bounds[i,,2]==0)))|(n0==0)
	indZero <- unlist(lapply(1:nrow(muTab),function(i)all(muTab[i,]==0)&all(bounds[i,,1]==0)&all(bounds[i,,2]==0)))&(n0==0)
	names(indZero) <- rownames(muTab)
	if(is.null(names(indZero)) || length(unique(names(indZero)))!=length(indZero)){
		names(indZero) <- 1:length(indZero)
		}
	leaveOut <- -which(indZero)
	if(any(indZero)){
		if(!is.null(fixedRows)){
			fixed <- which((1:nrow(muTab))[leaveOut]%in%fixed)
			}
		muTab <- muTab[leaveOut,]
		n0 <- n0[leaveOut]
		if(!is.null(controlRow)) controlRow <- controlRow[leaveOut,]
		bounds <- bounds[leaveOut,,]
		}

	nr<-nrow(muTab)
	nc<-ncol(muTab)
	if(length(sdev==1)) sdev <- rep(sdev,nc)
	
	okTab <- list()
	
	if(is.null(controlRow)) controlRow <- do.call(rbind,lapply(1:nr,function(i){
		if(all(c(muTab[i,nc],bounds[i,nc,])==0)) nc <- which.max(apply(bounds[i,,],1,function(x)diff(range(x))))
		range(bounds[i,nc,])
		}))
		
	# NB: questo é SOLO se non diamo control Col come input, quindi credo vada bene una normale (è il caso nel quale non abbiamo idea dei boundaries)
	if(is.null(controlCol)) {
		sdTab <- abs((bounds[,,2]-bounds[,,1])/2)
		controlCol <- do.call(cbind,lapply(1:nc,function(j)range(rnorm(N,colSums(muTab),sqrt(colSums(sdTab^2))))))
		}
	#browser()
	iter <- t <- 1L
	uniqueT <- 1
	avuoto <- 0L
	
	while(t <= nIter){
		
		if(verbose) print(t)
		sim <- do.call(rbind,lapply(1:nr,function(i){
			
			if(i%in%fixed) return(fixedRows[fixed==i,])
			
			rrow <- rep(NA,nc)
			names(rrow) <- colnames(muTab)
			repeat{
				nc<-ncol(muTab)
				### VARSTOCK structural 0
				if(all(c(muTab[i,nc],bounds[i,nc,])==0)){
					nc <- max(which(muTab[i,-nc]!=0))
					rrow[(1:length(rrow))>nc]<-0
					
					maxTol <- which.max(apply(bounds[i,,],1,function(x)diff(range(x))))
					
					rrow[-c(maxTol,(nc+1):length(rrow))] <- rtnorm(nc-1, mean=unlist(muTab[i,-c(maxTol,(nc+1):length(rrow))]), sd=sdev[(nc+1):length(rrow)], lower=bounds[i,-c(maxTol,(nc+1):length(rrow)),1],upper=bounds[i,-c(maxTol,(nc+1):length(rrow)),2])

					rrow[maxTol] <- (n0[i]-sum(rrow[-c(maxTol,nc+1:length(rrow))]))
					nc<-maxTol
					}else{ 
						###VARSTOCK not structural 0
						#browser()
						#if(length(rrow[-nc])!=nc-1)browser()
						rrow[-nc] <- rtnorm(nc-1, mean=unlist(muTab[i,-nc]), sd=sdev[-nc], lower=bounds[i,-nc,1],upper=bounds[i,-nc,2])
						rrow[nc] <- -n0[i]+sum(rrow[-nc])
						
						}
				if(rrow[nc]>=min(controlRow[i,]) & rrow[nc]<=max(controlRow[i,])) break
				avuoto <- avuoto + 1L
				if(avuoto > 1000L) warning("Running in Circles!!!")
				}
				if(verbose)cat("*")
			return(rrow)
			
			}))
			
		if(verbose)cat("\n")
		# browser()
		totCol <- colSums(sim)
		cond <- sapply(1:nc,function(j)(totCol[j]>=controlCol[1,j] & totCol[j]<=controlCol[2,j]))

		if(all(cond)){
			avuoto <- 0L
			if(t==1){
				okTab[[uniqueT]] <- sim
				attr(okTab[[uniqueT]],"mult") <- 0
				}
				
			dejavu <- FALSE
			
			for(k in 1:uniqueT){
				if(all(sim==okTab[[k]])){
					attr(okTab[[k]],"mult") <- attr(okTab[[k]],"mult") + 1
					break
					}
				}
			
			
			if(!dejavu & t<nIter){
				uniqueT <- uniqueT+1
				okTab[[uniqueT]] <- sim
				attr(okTab[[uniqueT]],"mult") <- 1
				}
			
			t <- t+1L
			} else {
				avuoto <- avuoto + 1L
				if(avuoto > 1000L) warning("Running in Circles!!!")
			}
		iter <- iter + 1L
        }
      #browser()
      okTab <- okTab[1:uniqueT]
      bestTab <- okTab[[which.min(unlist(lapply(okTab,objFun)))]]
      row.names(bestTab) <- names(indZero[!indZero])
      bestTab <- data.frame(bestTab)[names(indZero),]
      
      ## Add name when indZero (otherwise NA) #Marco
      row.names(bestTab) <- names(indZero)
      bestTab[indZero,] <- 0
      
      ## Let's give back the correct sign for imports, otherwise the next checks do not make sense
      bestTab[, "Imports"] <- -bestTab[, "Imports"]
      
      ## Add colon names
      colnames(bestTab) <- colnames(muTab)
      if(transpose){
      	bestTab <- t(bestTab)
      	okTab <- lapply(okTab,function(tab)t(tab))
      	}

      if(any(c("all","Import")%in%checks) & any(bestTab[!indZero,"Exports"] > n0 + bestTab[!indZero,"Imports"])){
      	#communicate("Exports exceed Production + Imports")
      	warning("Exports exceed Production + Imports")
      	}
      
      if(!is.null(stkSft)){
      	#browser()
		      if(any(c("all","Stock")%in%checks) & any(bestTab[!indZero,"Stock"] > stkSft/100 * (n0 + bestTab[!indZero,"Imports"] - bestTab[!indZero,"Exports"]))){
      	#communicate("Stock cannot exceed more than 20% of Domestic Supply")
      	warning(paste("Stock cannot exceed more than ",stkSft,"% of Domestic Supply",sep=""))
      	}      	
      }	

      
      if(is.infinite(abs(objFun(bestTab)))) {
      	#communicate("Conditions Violated")
      	warning("Conditions Violated")
      	if(!"none"%in%checks) return(NULL)
      }
      return(new("conTa",bestTab=as.matrix(bestTab),tables=okTab,iters=iter,objective=abs(objFun(bestTab)),call=call,args=argz))
      
      }


#################################
### OOP: CONstrained TAble object
#new("conTa",bestTab=matrix(),tables=list(),iters=0L,objective=0,call=new("call"),args=list())
setClass("conTa",representation=representation(bestTab="matrix",
                                              tables="list",
                                              iters="integer",
                                              objective="numeric",
                                              call="call",
                                              args="list"))

setMethod("show",signature("conTa"),function(object){
	cat("Call:\n")
	print(object@call)
	cat("\nOptimal Table: ")
	print(object@bestTab)
	cat("Number of Iterations: ")
	cat(object@iters,"\n")
	cat("Objective Function: ",attr(object@objective,"objName"))
	cat(object@objective,"\n")
	})


setMethod("print",signature("conTa"),function(x,file=NULL,...){
	cat("Call:\n")
	print(x@call)
	cat("\nOptimal Table: ")
	print(x@bestTab)
	cat("Number of Iterations: ")
	cat(x@iters,"\n")
	cat("Objective Function: ",attr(x@objective,"objName"))
	cat(x@objective,"\n")
	if(!is.null(file)) write.csv(x@bestTab,file=file,...)
	})

### subsetting method
setMethod("[[", signature = "conTa", definition = function (x, i,j,..., drop = TRUE) {
	if (!missing(j)) {stop("Wrong number of dimensions.")}
	if (!missing(i)) {
		return(switch(class(i),"character" = attributes(x)[[i]],
		                       "integer" = attributes(x)[[i]],
		                       "numeric" = attributes(x)[[i]],
		                       "logical" = attributes(x)[c(i,rep(FALSE,length(attributes(x))-length(i)))],
		                        stop("Subsetting object needs to be either a character, a numeric/integer or a logical.")
		                        ))
		}else{return(NULL)}
  
   })

setMethod("$", signature = "conTa", definition = function(x, name) {
	 x[[name]]
	 })
