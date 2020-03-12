


EcoMAPS_R <- function(
PredVars,
time_slices,
csv_file,
map_image_file,
fit_image_file,
temp_netcdf_file = "temp",
output_netcdf_file = "output.nc",
mult_year, 
rand_grp,
data_type,
model_variable,
saveplots=TRUE,
write_2_netcdf=TRUE
){
	
	#load R libraries that are needed
   require(nlme)
   require(mgcv)
   require(ncdf4)
   require(gdata)
   require(fields) 
	
	
	print("Loading data and covariate information")
	dat = read.csv(csv_file)
    
	coverage_setup <- cov_setup(PredVars)
	  
	covariate_data <- cov_data_def(coverage_setup)
	
	print("Defining family of response")  
	data_fam_resp <- def_dist_fam(dat=dat,rand_grp=rand_grp,mult_year=mult_year,model_variable=model_variable,data_type="Cont")

	print("Extracting covariates and defining model formula") 
	cov_data_full <- get_cov_dat(covariate_data=covariate_data,temp_netcdf_file=temp_netcdf_file,time_slices=time_slices,newdat=data_fam_resp$newdat)
	  
	form <- construct_form(nm_var=cov_data_full$nm_var,var_data_type=cov_data_full$var_data_type)
	  
	print("Running model")   
	mod <- run_mod(newdat=data_fam_resp$newdat,data_fam=data_fam_resp$data_fam,rand_grp=rand_grp,
				mult_year=mult_year,form=form$form)	
	  
	print("Making predictions from model")   
	prd_ests <- make_preds(spat_res=cov_data_full$spat_res,east=cov_data_full$east,north=cov_data_full$north,
				tmp.array=cov_data_full$tmp.array,mod=mod,newdat=data_fam_resp$newdat,
				factor_vars=form$factor_vars,smooth_vars=form$factor_vars,nm_var=cov_data_full$nm_var)

	print("plotting estimated maps and goodness of fit plots") 
	plot_map(out_mat_pred=prd_ests$out_mat_pred,out_mat_var=prd_ests$out_mat_var,east=cov_data_full$east,
				north=cov_data_full$north,resuse=prd_ests$resuse,saveplots=saveplots,map_image_file=map_image_file,mod=mod,model_variable="loi")
	  
	plot_fit(mod=mod,fit_image_file=fit_image_file,saveplots=saveplots)
	  
	print("Saving model summary")   
	mod_sum <- mod_summary(mod=mod,nm_var=cov_data_full$nm_var,model_variable="loi")
	  
	  
	if(write_2_netcdf){  
	print("Writing file to netcdf") 
	writeNCDF(covariate_data=covariate_data,nm_var=cov_data_full$nm_var,resuse=prd_ests$resuse,east=cov_data_full$east,north=cov_data_full$north,
				model_variable="loi",file=output_netcdf_file,out_mat_pred=prd_ests$out_mat_pred,out_mat_var=prd_ests$out_mat_var,
				mod_formula=mod_sum$mod_formula,rmse=mod_sum$rmse,r2=mod_sum$r2)
	}
	
	return(mod_sum)
	
}				
