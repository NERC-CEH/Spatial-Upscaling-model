
construct_form <- function(nm_var,var_data_type){

  ###need to know which variables are factors

      ##if the variable has an associated unit, it is a continous numberic variable, else it is a factor
      factor_vars <- nm_var[var_data_type=="class"]
      smooth_vars <- nm_var[var_data_type=="cont"]


      factor_form <- paste(paste("as.factor(",factor_vars,")",sep=""),collapse="+")
      smooth_form <- paste(paste("s(",smooth_vars,",k=6)",sep=""),collapse="+")
      #if(is.null(mult_year)){int=1}else{int=mult_year}


      ##paste togetehr the factor variable terms and the numeric smooth terms into a model formula to submit to the model
      ##add additional term to capture purely large scale spatial dependence
      if(length(factor_vars)>0){
         form <- paste("response~1",factor_form,"te(EASTING,NORTHING)",sep="+")
         if(length(smooth_vars)>0){
            form <- paste(form,smooth_form,sep="+")
         }
      }else{
         if(length(smooth_vars)>0){
             form <- paste("response~1",smooth_form,"te(EASTING,NORTHING)",sep="+")
         }else{
            form <- paste("response~1","te(EASTING,NORTHING)",sep="+")
         }
      }

	  	  return(list(form=form,factor_vars=factor_vars,smooth_vars=smooth_vars))
	  
	  
	}
	
########