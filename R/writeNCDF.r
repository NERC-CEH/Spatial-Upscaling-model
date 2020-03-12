
writeNCDF <- function(covariate_data,nm_var,resuse,north,east,model_variable,file,
				out_mat_pred,out_mat_var,mod_formula,rmse,r2,covs.local){

		## open up one of the covariate files with same spatial resolution as derived map to use as a template
    v=unlist(covariate_data)
    fl=v[which(names(v)[1:which(v==nm_var[resuse])]=="linkfile")]
    
		if(covs.local){
			cov_dat <- nc_open(fl)
		}else{
		download.file(url=fl,destfile="tempres.nc", mode="wb")
		## open up the connection to the file to use as a template
		cov_dat <- nc_open("tempres.nc")
		}
		
		output_netcdf_file = file
    ###### define all the variable to go into the netcdf file

    ## define the spatial coordinates of the derived map
    y <- ncdim_def("y", "m", north[[resuse]], unlim=FALSE, create_dimvar=TRUE, longname="northing - OSGB36 grid reference")
    x <- ncdim_def("x", "m", east[[resuse]],  unlim=FALSE, create_dimvar=TRUE, longname="easting - OSGB36 grid reference")

    ## define the mean value prodcued by the model
    pred <- ncvar_def(name=model_variable, units="Some Units", dim=list(x, y), missval=(-999), longname=paste(model_variable,"Modelled Estimate",sep=""))
    ##define the variance term on the modelled variable
    mod_var <- ncvar_def(name="VAR", units="Standard Error", dim=list(x, y), missval=(-999), longname=paste(model_variable," Estimate Variance",sep=""))
    ##define the data projection so the netcdf file can be read by GIS easily
    prj <- ncvar_def(name="transverse_mercator", units="", dim=list(), longname="coordinate_reference_system", prec="integer")

    ##### create the new netcdf file to put the derived map in
    ncnew <- nc_create(output_netcdf_file, list(pred, mod_var, prj))

    #put the mean and variance data into the defined variables in the netcdf
    ncvar_put(ncnew, pred, unmatrix(out_mat_pred))
    ncvar_put(ncnew, mod_var, unmatrix(out_mat_var))

    ##provide meta data for the spatial coordinates used in the derived map
    ncatt_put(ncnew, "y", "standard_name", "projection_y_coordinate")
    ncatt_put(ncnew, "y", "point_spacing", "even")
    ncatt_put(ncnew, "x", "standard_name", "projection_x_coordinate")
    ncatt_put(ncnew, "x", "point_spacing", "even")

    ## provide meta data for the modelled value in the derived map
    ncatt_put(ncnew, model_variable, "coordinates", "x y")
    ncatt_put(ncnew, model_variable, "grid_mapping", "transverse_mercator")
    ncatt_put(ncnew, model_variable, "valid_min", min(unmatrix(out_mat_pred), na.rm = TRUE), prec="float")
    ncatt_put(ncnew, model_variable, "valid_max", max(unmatrix(out_mat_pred), na.rm = TRUE), prec="float")
    ncatt_put(ncnew, model_variable, "missing_value", -999, prec="float")
    ncatt_put(ncnew, model_variable, "ancillary_variables", "VAR")

    ### provide meta data for the variance of the variable in the derived map
    ncatt_put(ncnew, "VAR", "coordinates", "x y")
    ncatt_put(ncnew, "VAR", "grid_mapping", "transverse_mercator")
    ncatt_put(ncnew, "VAR", "valid_min", min(unmatrix(out_mat_var), na.rm = TRUE), prec="float")
    ncatt_put(ncnew, "VAR", "valid_max", max(unmatrix(out_mat_var), na.rm = TRUE), prec="float")
    ncatt_put(ncnew, "VAR", "missing_value", -999, prec="float")

    ## provide meta data for the sptai lprojection used in the derived map
    ncatt_put(ncnew,"transverse_mercator", "grid_mapping_name" ,"transverse_mercator")
    ncatt_put(ncnew,"transverse_mercator", "semi_major_axis", 6377563.396,prec="double")
    ncatt_put(ncnew,"transverse_mercator", "semi_minor_axis" ,6356256.910,prec="double")
    ncatt_put(ncnew,"transverse_mercator", "inverse_flattening" ,299.3249646,prec="double")
    ncatt_put(ncnew,"transverse_mercator", "latitude_of_projection_origin" ,49.0,prec="double")
    ncatt_put(ncnew,"transverse_mercator", "longitude_of_projection_origin" ,-2.0,prec="double")
    ncatt_put(ncnew,"transverse_mercator", "false_easting" ,400000.0,prec="double")
    ncatt_put(ncnew,"transverse_mercator", "false_northing" ,-100000.0,prec="double")
    ncatt_put(ncnew,"transverse_mercator", "scale_factor_at_projection_origin",0.9996012717,prec="double")

    ## provide other global header information for the netcdf file. 
    ncatt_put(ncnew, 0, "title", paste("A map of ",model_variable,sep=""))
    ncatt_put(ncnew, 0, "institution", "Centre for Ecology & Hydrology (CEH) Lancaster")
    ncatt_put(ncnew, 0, "source", "Centre for Ecology & Hydrology (CEH) Lancaster")
    ncatt_put(ncnew, 0, "reference", "EcoMAPS v1.01")
    ncatt_put(ncnew, 0, "description", paste("Predicted map of ",model_variable," using EcoMAPS v1.01",sep=""))
    ncatt_put(ncnew, 0, "grid_mapping", "transverse_mercator")
    ncatt_put(ncnew, 0, "history", "history")
    ncatt_put(ncnew, 0, "summary", paste("Predicted map of ",model_variable,sep=""))
    ncatt_put(ncnew, 0, "keywords", paste("Mapping",model_variable,sep=" , "))
    ncatt_put(ncnew, 0, "date_created", as.character(Sys.Date()), prec="text")
    ncatt_put(ncnew, 0, "date_modified", as.character(Sys.Date()), prec="text")
    ncatt_put(ncnew, 0, "date_issued", as.character(Sys.Date()), prec="text")

    ncatt_put(ncnew, 0, "creator_name", "Not Specified")
    ncatt_put(ncnew, 0, "creator_url", "http://www.ceh.ac.uk//")
    ncatt_put(ncnew, 0, "creator_email", "Not Specified")

    ncatt_put(ncnew, 0, "geospatial_lon_min", ncatt_get(cov_dat, 0, "geospatial_lon_min")$value, prec="double")
    ncatt_put(ncnew, 0, "geospatial_lat_min", ncatt_get(cov_dat, 0, "geospatial_lat_min")$value, prec="double")
    ncatt_put(ncnew, 0, "geospatial_lon_max", ncatt_get(cov_dat, 0, "geospatial_lon_max")$value, prec="double")
    ncatt_put(ncnew, 0, "geospatial_lat_max", ncatt_get(cov_dat, 0, "geospatial_lat_max")$value, prec="double")
    ncatt_put(ncnew, 0, "licence", ncatt_get(cov_dat,0,"licence")$value)
    ncatt_put(ncnew, 0, "publisher_name", "Centre for Ecology & Hydrology")
    ncatt_put(ncnew, 0, "publisher_url", "http://www.ceh.ac.uk")
    ncatt_put(ncnew, 0, "publisher_email", "enquiries@ceh.ac.uk")
    ncatt_put(ncnew, 0, "Conventions", ncatt_get(cov_dat, 0, "Conventions")$value)
    ncatt_put(ncnew, 0, "comment", "comment")
    ncatt_put(ncnew, 0, "model_formula", mod_formula)
    ncatt_put(ncnew, 0, "root_mean_square_error", rmse)
    ncatt_put(ncnew, 0, "r_squared", r2)

    ##finish and close the created netcdf file
    #nc_close(temp_netcdf_file)
    nc_close(ncnew)
	
}

