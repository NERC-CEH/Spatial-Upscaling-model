
make_preds <- function(spat_res,east,north,tmp.array,mod,newdat,factor_vars,smooth_vars,nm_var){


		if(class(mod)=="try-error"){
		
		
        out_mat_pred = matrix(NA,byrow=TRUE,ncol=1300,nrow=700)
        out_mat_var = matrix(NA,byrow=TRUE,ncol=1300,nrow=700)
		
			resuse=NULL
		
		}else{

  ###convert all covariates onto common scale by choosing the finest resolution available.
        resuse <- which.min(spat_res)
        std_res <- spat_res[resuse]
        scale_res <- spat_res/std_res

        ##for those not on the common scale, repeat values to recreate corresponding scale. eg repetat 5km scale 25 times to match 1km scale
        for(k in which(scale_res!=1)){

            mat=matrix(1:(length(east[[k]])*length(north[[k]])),ncol=length(north[[k]]),nrow=length(east[[k]]),byrow=TRUE)
            z=c()

            for(j in 1:length(mat[,1])){

                   x=rep(mat[j,],rep(scale_res[k],length(mat[j,])))
                   y=rep(x,scale_res[k])
                   z=c(z,y)
            }

            tmp.array[[k]] = matrix(unmatrix(tmp.array[[k]],byrow=TRUE)[z],byrow=TRUE,ncol=1300,nrow=700)
        }


          #### predict model using the full covariate set at the common resolution
          pred_points <- expand.grid(north[[resuse]],east[[resuse]])
          newd=data.frame(easting=pred_points[,2],northing=pred_points[,1])
          newd$EASTING=newd$easting ; newd$NORTHING=newd$northing
          for(cov_i in 1:length(tmp.array)){
            newd <- cbind(newd,unmatrix(tmp.array[[cov_i]],byrow=TRUE))
          }

          names(newd)[-c(1:4)]=nm_var

          #to ensure all fatcor levels are represented, find all rows that were excluded in the model
          cls <- match(c(factor_vars,smooth_vars),names(newdat))
          rem_na_rows <- which(apply(newdat[,cls],1,function(X){any(is.na(X))}))


          ##set any values outside the covariate space to NA. Particularly important for factors

          for(fv in factor_vars){
            c1=which(names(newd)==fv) ; c2=which(names(newdat)==fv)
            if(length(rem_na_rows)>0){
               idx=which(!is.element(newd[,c1],unique(newdat[-rem_na_rows,c2])))
            }else{
               idx=which(!is.element(newd[,c1],unique(newdat[,c2]))) 
            }
            newd[idx,c1]=NA
            newd[,c1]=as.factor(as.character(newd[,c1]))
          }


          ##use the model the predict the coverage data. ensure model predict the standard error as well - this may be replaced by bootstrap in future
          modpred=predict(mod$gam,newdata=newd,se.fit=TRUE,type="response")

          pred_se_tab=data.frame(preds=modpred$fit,sefit=modpred$se.fit)

          if(exists("idx")) {
            pred_se_tab[idx,]=NA
          }

          names(pred_se_tab)=c("Predicted","Standard_Error")

          out.table=cbind(newd,pred_se_tab)

          ##store the predicted mean and variance as matrices - easy for compliance with Netcdf format
          out_mat_pred = matrix(out.table$Predicted,byrow=TRUE,ncol=1300,nrow=700)
          out_mat_var = matrix(out.table$Standard_Error,byrow=TRUE,ncol=1300,nrow=700)

		  }
		  
		  return(list(out_mat_pred=out_mat_pred,out_mat_var=out_mat_var,resuse=resuse))
		  
}		  
		  
	