conSTable <-
function(muTab, rowTot, prop, nIter=100, N=10000,sdev=5,verbose=TRUE,objFun=function(tab){-colSums(tab)[1]},fixedRows=NULL,fixed=c(),transpose=FALSE,...){#,keepArgs=FALSE
	
	if(transpose){
		muTab <- t(muTab)
		}
	
	bounds <- array(NA,c(dim(muTab),2))
	dimnames(bounds) <- c(dimnames(muTab),list(c("Lower","Upper")))
	if(!is.null(dim(prop))){
		# prop is a by cell proportion
		bounds[,,"Lower"] <- as.matrix((1-prop*sign(muTab))*muTab)
		bounds[,,"Upper"] <- as.matrix((1+prop*sign(muTab))*muTab)
		} else if(length(prop)==1){
			# prop is a number
			bounds[,,"Lower"] <- as.matrix((1-prop*sign(muTab))*muTab)
			bounds[,,"Upper"] <- as.matrix((1+prop*sign(muTab))*muTab)
			} else {
				# prop is a by column proportion
				bounds[,,"Lower"] <- as.matrix(do.call(cbind,lapply(1:length(prop),function(j)(1-prop[j]*sign(muTab[,j]))*muTab[,j] )))
				bounds[,,"Upper"] <- as.matrix(do.call(cbind,lapply(1:length(prop),function(j)(1+prop[j]*sign(muTab[,j]))*muTab[,j] )))
				}
	
	colTot <- colSums(muTab)
	controlCol <- rbind(colTot*(1-prop*sign(colTot)),colTot*(1+prop*sign(colTot)))
	.sampleTables(rowTot,muTab,bounds,controlCol,verbose=verbose,transpose=transpose,fixedRows=fixedRows, fixed=fixed,...)#
	
	}




.sampleTables <-
function(n0,muTab, bounds,controlCol=NULL,controlRow=NULL,nIter=100,N=10000,sdev=5,verbose=TRUE,objFun=function(tab){-colSums(tab)[1]},fixed=c(),fixedRows=NULL,transpose=FALSE,keepArgs=FALSE,...){

	call <- match.call()
	
	if(keepArgs){
		argz <- lapply(2:length(call),function(i)eval(call[[i]]))
		names(argz) <- names(call)[2:length(names(call))]
		}else argz <- list()
	
	### zero rows	
	indZero <- apply(muTab,1,function(x)all(x==0))|(n0==0)
	leaveOut <- -which(indZero)
	if(any(indZero)){
		if(!is.null(fixedRows)){
			fixed <- which((1:nrow(muTab))[leaveOut]%in%fixed)
			}
		muTab <- muTab[leaveOut,]
		n0 <- n0[leaveOut]
		if(!is.null(controlRow))controlRow <- controlRow[leaveOut,]
		bounds <- bounds[leaveOut,,]
		}

	nr<-nrow(muTab)
	nc<-ncol(muTab)
	
	okTab <- list()
	
	if(is.null(controlRow)) controlRow <- do.call(rbind,lapply(1:nr,function(i){
		if(muTab[i,nc]==0) nc <- which.max(apply(bounds[i,,],1,function(x)diff(range(x))))
		range(bounds[i,nc,])
		}))
		
	# NB: questo é SOLO se non diamo control Col come input, quindi credo vada bene una normale (é il caso nel quale non abbiamo idea dei boundaries)
	if(is.null(controlCol)) {
		sdTab <- abs((bounds[,,2]-bounds[,,1])/2)
		controlCol <- do.call(cbind,lapply(1:nc,function(j)range(round(rnorm(N,colSums(muTab),sqrt(colSums(sdTab^2)))))))
		}
	
	iter <- t <- 1L
	uniqueT <- 1
	
	while(t <= nIter){
		
		sim <- do.call(rbind,lapply(1:nr,function(i){
			
			if(i%in%fixed) return(fixedRows[fixed==i,])
			
			rrow <- rep(NA,nc)
			repeat{
				
				nc<-ncol(muTab)
				
				### VARSTOCK structural 0
				if(muTab[i,nc]==0){
					nc <- max(which(muTab[i,-nc]!=0))
					rrow[(1:length(rrow))>nc]<-0
					
					maxTol <- which.max(apply(bounds[i,,],1,function(x)diff(range(x))))
					
					rrow[-c(maxTol,(nc+1):length(rrow))] <- round(rtnorm(nc-1, mean=unlist(muTab[i,-c(maxTol,(nc+1):length(rrow))]), sd=sdev, lower=bounds[i,-c(maxTol,(nc+1):length(rrow)),1],upper=bounds[i,-c(maxTol,(nc+1):length(rrow)),2]),0)

					rrow[maxTol] <- (n0[i]-sum(rrow[-c(maxTol,nc+1:length(rrow))]))
					nc<-maxTol
					}else{ 
						###VARSTOCK not structural 0
						
						#if(length(rrow[-nc])!=nc-1)browser()
						rrow[-nc] <- round(rtnorm(nc-1, mean=unlist(muTab[i,-nc]), sd=sdev, lower=bounds[i,-nc,1],upper=bounds[i,-nc,2]),0)
						rrow[nc] <- -(n0[i]-sum(rrow[-nc]))
						
						}
				if(rrow[nc]>=min(controlRow[i,]) & rrow[nc]<=max(controlRow[i,]))break
				
				}
				if(verbose)cat("*")
			return(rrow)
			
			}))
			#browser()
		if(verbose)cat("\n")
		#browser()
		totCol <- colSums(sim)
		cond <- sapply(1:nc,function(j)(totCol[j]>=controlCol[1,j] & totCol[j]<=controlCol[2,j]))
		
		if(all(cond)){
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
			if(verbose)print(t)
			}
		iter <- iter + 1L
        }

      okTab <- okTab[1:uniqueT]
      bestTab <- okTab[[which.min(unlist(lapply(okTab,objFun)))]]
      row.names(bestTab) <- names(indZero[!indZero])
      bestTab <- data.frame(bestTab)[names(indZero),]
      
      if(transpose){
      	bestTab <- t(bestTab)
      	okTab <- lapply(okTab,function(tab)t(tab))
      	}
      	return(new("conTa",bestTab=as.matrix(bestTab),tables=okTab,iters=iter,objective=objFun(bestTab),call=call,args=argz))
      
      }


#################################
### OOP: CONstrained TAble object

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
	cat("Objective Function: ")
	cat(object@objective,"\n")
	})
