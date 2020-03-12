	
mod_summary <- function(mod,nm_var,mod_t,model_variable){
        
		if(class(mod)!="try-error"){
		## Ste info of model fit for the netCDF file. 
        ## specifically, AIC, RMSE, Rsquared, model formula and summary table for coefficients and p values.
        if(!is.element("lme",names(mod))){aic_val <- AIC(mod$gam)}else{aic_val <- AIC(mod$lme)}
        
			rmse <- sqrt(mean((mod$gam$y-mod$gam$fitted.values)^2))
        r2 <- (summary(mod$gam))$r.sq
        mod_formula <- paste(model_variable," ~ Intercept + ",paste(nm_var,collapse=" + "),sep="")
        mod_sum <- summary(mod$gam)
		}else{
		
		rmse <- "NA Error"
        r2 <- "NA Error"
        mod_formula <- "NA Error"
        aic_val <- "NA Error"
		mod_sum <- "NA Error"
		}
		
		return(list(rmse=rmse,r2=r2,mod_formula=mod_formula,mod_sum=mod_sum,aic_val=aic_val))

}		
		