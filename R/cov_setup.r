
cov_setup <- function(PredVars,covs.local=FALSE){
			
			if(covs.local){
				dat=variableNamesTypeLocal
			}else{
				dat=variableNamesType
			}
			
        coverage_setup=list()  ; nms=c()
        k=1
        
        for(var in PredVars){
            rw_id = which(dat$VARIABLE.NAME==var)
            coverage_setup[[k]]=list(as.character(dat$netCDF.VARIABLE[rw_id]),as.character(dat$VARIABLE.TYPE[rw_id]))
            nms=c(nms,as.character(dat$THREDDS.URL[rw_id]))
        
            k=k+1
        
        }
        names(coverage_setup)=nms
        return(coverage_setup)
    
    }

	
