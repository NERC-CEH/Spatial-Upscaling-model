
get_cov_dat <- function(covariate_data,temp_netcdf_file,time_slices,newdat,covs.local){

    ##set up empty vectors for storage
    n_north = n_east= spat_res = nm_var = dunits = var_data_type = c()
    tmp.array = north = east = list()

    cn <- 1
    
    for(i in 1:length(covariate_data)){

        
		if(covs.local){
			cov_dat <- nc_open(covariate_data[[i]]$linkfile)
		}else{
			
			#download the files temporarily to enable easy reading in - R currently does not support OpenDAP, this "downloading" may change in the future
        download.file(url=covariate_data[[i]]$linkfile, destfile=paste(temp_netcdf_file,cn,sep=""), mode="wb")

        #open the connection to the current netcdf file
        cov_dat <- nc_open(paste(temp_netcdf_file,cn,sep=""))
		
		}

			

        ## loop over the variables to be extracted from the current netcdf file
        for(vn in 1:length(covariate_data[[i]]$vars)){

            # define the name of the variable and store as character string vector
            nm_var[cn] <- toString(covariate_data[[i]]$vars[vn])

            #extract the vertical spatial coordinates, which could be listed as y, northing, Northing or NORTHING in the netcdf file
            north[[cn]] <- try(ncvar_get(cov_dat, "y"));bv=1
            if(class(north[[cn]])=="try-error"){north[[cn]] <- try(ncvar_get(cov_dat, "northing"));bv=2}
            if(class(north[[cn]])=="try-error"){north[[cn]] <- try(ncvar_get(cov_dat, "Northing"));bv=3}
            if(class(north[[cn]])=="try-error"){north[[cn]] <- try(ncvar_get(cov_dat, "NORTHING"));bv=4}
            if(class(north[[cn]])=="try-error"){north[[cn]] <- try(ncvar_get(cov_dat, "y"));bv=5}
            #store the dimension of this vertical axis
            n_north[cn] <- dim(north[[cn]])
                       
            #extract the horizontal spatial coordinates, which could be listed as x, easting, Easting or EASTING in the netcdf file
            east[[cn]] <- try(ncvar_get(cov_dat, "x"))
            if(class(east[[cn]])=="try-error"){east[[cn]] <- try(ncvar_get(cov_dat, "easting"))}
            if(class(east[[cn]])=="try-error"){east[[cn]] <- try(ncvar_get(cov_dat, "Easting"))}
            if(class(east[[cn]])=="try-error"){east[[cn]] <- try(ncvar_get(cov_dat, "EASTING"))}
            if(class(east[[cn]])=="try-error"){east[[cn]] <- try(ncvar_get(cov_dat, "x"))}
            
            #store the dimension of this horizontal axis
            n_east[cn] <- dim(east[[cn]])

            ## store the resolution of the current variable, defined as the unit differnece between consecutive spatial points
            spat_res[cn] <- abs(mean(diff(north[[cn]])))

            # get the data and attributes
            tmp.array[[cn]] <- ncvar_get(cov_dat,toString(nm_var[cn]))

            # Check the time slices array to see if there's
            # a corrresponding entry, extract the slice at the designated index if so
            if(length(time_slices) > 0 && !is.na(time_slices[[nm_var[cn]]])){

                tmp.array[[cn]] <- tmp.array[[cn]][,,(time_slices[[nm_var[cn]]])]
            }

            ###retrieve meta-data from netcdf files
            ## extract the units of the current variable 
            dunits[cn] <- ncatt_get(cov_dat, nm_var[cn], "units")$hasatt
            var_data_type[cn] <- covariate_data[[i]]$data_type[vn]
            ## extract the value used to fill in missing data for the current variable
            fillvalue <- ncatt_get(cov_dat, nm_var[cn], "_FillValue")

            #If the fill value is not NA (ie -999) then replace with NA (both in the csv and the coverage matrix)so that it is not included in building the model
            if(!is.na(fillvalue$value)){
              var_id_nd <- which(names(newdat)==nm_var[cn])
              newdat[(newdat[,var_id_nd]==fillvalue$value & !is.na((newdat[,var_id_nd]))),var_id_nd]=NA
            }
            # replace fillvalues with NAs
            tmp.array[[cn]][tmp.array[[cn]]==fillvalue$value] <- NA

            cn <- cn +1
        }

        # done with the netCDF file, so close it
        #close.ncdf(cov_dat)
        nc_close(cov_dat)
    }

	
return(list(n_north = n_north, n_east = n_east, spat_res = spat_res, nm_var = nm_var, dunits = dunits,
			var_data_type = var_data_type, tmp.array = tmp.array, north = north, east = east))

}	

#