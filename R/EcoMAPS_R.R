EcoMAPS_R <- function(

PredVars,
time_slices,                                              ### defines which time slice to use for each input data set. eg precip runs 1970-2012 time_slice 1 = 1970
progress_rep_file = "progress.txt",
user_name = "Test Harness",                               ### details of the user name to pass to the resulting netcdf header
csv_file,													       ### name of the point based csv file from the spatial concatenation in python
output_netcdf_file = "output.nc",                         ### name of the final netcdf file that is written out
mult_year = NULL,                                         ### variable in the source data set that defines the temporal aspect of the data 
rand_grp = NULL,                                          ### variable in the source data set that defines any grouping structure present in the data that needs to be accounted for
data_type = "Cont",                                       ### type of variable that is to be modelleds - continuous, binary or count
model_variable,																	### name of variable (as it appears in csv file) that is to be modelled
saveplots=TRUE,     													### logical. should plots be saved                                      
map_image_file = "map_output.png",                        ### name to give the png of the map image produced
fit_image_file = "fit_output.png"                        ### name to give the png of the model summary plots produced
){

    
    #load R libraries that are needed
    require(nlme)
    require(mgcv)
    #require(ncdf)
    require(ncdf4)
    require(gdata)
    require(fields) 

	#specify a temporary name to store netcdf files to
    temp_netcdf_file = "temp.nc" 

    
    progress_fn("Initiating R session and loading libraries",progress_rep_file)

    #set contrasts to match those used in SAS as CS has always used this approach
    options(contrasts = c(factor = "contr.SAS",ordered = "contr.poly"))


    
    cov_setup <- function(PredVars){
			
			#data(variableNamesType)
        dat=variableNamesType
		
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


    coverage_setup=cov_setup(PredVars)

    covariate_data = list()

    # PJ: Constructing the named list dynamically based on what
    # has been passed in from the Python
    for(i in 1:length(names(coverage_setup))) {

        covariate_data[[i]] = list()
        url = names(coverage_setup)[i]

        covariate_data[[i]]$linkfile = url
        covariate_data[[i]]$vars = c(coverage_setup[[url]][[1]])
        covariate_data[[i]]$data_type = c(coverage_setup[[url]][[2]])
        
    }

    #read in concatenated point data (simon's python script does this)
    dat=read.csv(csv_file)
    
    progress_fn("Read CSV.",progress_rep_file)
 

    ## define the family of distributions to choose from
    poss_fam <- c("gaussian","Gamma","quasibinomial","quasipoisson")

    #create new dataset so that we do not over-write the input one
    newdat=dat

    ## specify the response variable to include in the model
    newdat$response <- newdat[,which(names(newdat)==model_variable)]
    ## specify the random grouping facotr to include in the model, if it exists
    if(!is.null(rand_grp)){newdat$rnd_group <- newdat[,which(names(newdat)==rand_grp)]}
    ## specify the temporal component variable to include in the model, if it exists
    if(!is.null(mult_year)){newdat$tm_var <- newdat[,which(names(newdat)==mult_year)]}

    progress_fn("Checking distribution of Response Variable",progress_rep_file)

    #test normality of response if it is continuos. if normality test fails, ensure that a Gamma error distribution is used.
    if(data_type=="Cont" & length(newdat$response)<4999){
        
        ##use shapiro  wilks test to assess normality
        norm.test.res <- shapiro.test(newdat$response)

        ##if null hypothesis of normality rejected, then specify data type to be continuous, but not gaaussian
        if(norm.test.res$p.value<0.05){
         
            data_type="ContNonG"
        
        }
        
     }

    #select the distribution family to use given the type of data being modelled
    data_fam <- switch(data_type,"Cont"=poss_fam[1],"ContNonG"=poss_fam[2],"Count"=poss_fam[4],"Binary"=poss_fam[3])

    progress_fn("Loading data into R",progress_rep_file)

    ##set up empty vectors for storage
    n_north = n_east= spat_res = nm_var = dunits = var_data_type = c()
    tmp.array = north = east = list()

    cn <- 1
    
    for(i in 1:length(covariate_data)){

        #download the files temporarily to enable easy reading in - R currently does not support OpenDAP, this "downloading" may change in the future
        download.file(url=covariate_data[[i]]$linkfile, destfile=paste(temp_netcdf_file,cn,sep=""), mode="wb")

        #open the connection to the current netcdf file
        cov_dat <- nc_open(paste(temp_netcdf_file,cn,sep=""))

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

    progress_fn("Constructing Model formula",progress_rep_file)



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


    progress_fn("Running Model",progress_rep_file)

      ## find the correct grouping variable to use to define individual observations within a group within a year. 
      ##subset the data to the first time observation and the first group
      x=newdat[newdat$rnd_group==newdat$rnd_group[1] & newdat$tm_var==newdat$tm_var[1],]
      ##fins all columns in the data frame that have unique values for all enties in the subset data
      v=which(apply(x,2,function(X){length(unique(X))})==dim(x)[1])
      ##find which of these varaibles are a factor and choose the first
      tm_id <- which(sapply(x[,v],class)=="factor")[1]
      ##this is the name of the variable to define groups/temporal slices in the mdoel
      tmp_nm = names(v)[tm_id]
      ##define a new column called temp_rand with this grouping factor in
      newdat$temp_rand = newdat[,which(names(newdat)==tmp_nm)]
      
      #by default specify the type of model used as a mixed effects gam
      mod_t = "mix_gam"
      
      #based on whether there is a random factor and whether autocrellation is needed, fit the correct model
      if(!is.null(rand_grp) & is.null(mult_year)){
        progress_fn("Applying GAMM, grouped data",progress_rep_file)
        mod=try(gamm(as.formula(form),random=list(rnd_group=~1),data=newdat,family=data_fam,niterPQL=5))
      }else{

        if(is.null(rand_grp) & !is.null(mult_year)){
          progress_fn("Applying GAMM, multi-year",progress_rep_file)
          mod=try(gamm(as.formula(form),correlation=corAR1(form=~tm_var|temp_rand),data=newdat,family=data_fam,niterPQL=5))
        }else{
          if(!is.null(rand_grp) & !is.null(mult_year)){
            progress_fn("Applying GAMM, multi-year and grouped",progress_rep_file)
            mod=try(gamm(as.formula(form),random=list(temp_rand=~1),correlation=corAR1(form=~tm_var|temp_rand),data=newdat,family=data_fam,niterPQL=5))
            if(class(mod)=="try-error"){# | (summary(mod$gam)$r.sq)<0){
              mod=try(gamm(as.formula(form),random=list(rnd_group=~1),data=newdat,family=data_fam,niterPQL=5))
            }
          }
        }
      }
      ## store the r squared of the model (providing it ran ok)
      if(class(mod)!="try-error" ){md_rsq <- (summary(mod$gam)$r.sq)}else{md_rsq <- (-9999)}

      ##if model didnt run, then simplify and use a standard gam
      if(class(mod)=="try-error" | md_rsq<0){
        mod=list() ; mod$gam=try(gam(as.formula(form),data=newdat,family=data_fam))
        #store the model type as normal gam
        mod_t="norm_gam"
        if(class(mod$gam)=="try-error"){class(mod)="try-error"}
     }

    if(class(mod)!="try-error"){

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

         ##write model results to the assocaited values

          progress_fn("Predicting model over full spatial grid",progress_rep_file)


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


        progress_fn("Writing map image",progress_rep_file)
        
        ## produce graphic of the modelled output and save as a png file 
        windows(height=720,width=700)
        
          #split the plotting windoe into two - one for mean, one for variance
          par(mfrow=c(1,2))
          
          qtsm=round(quantile(c(out_mat_pred),c(0.1,0.9),na.rm=TRUE))
          brk=c(min(out_mat_pred,na.rm=TRUE),seq(qtsm[1],qtsm[2],length=9),max(out_mat_pred,na.rm=TRUE))
          #brk=c(seq(0,50,by=10),100)
          #cols=c("lightgoldenrod1","lightgoldenrod3","burlywood3","darkgoldenrod","lightsalmon4","darkorange4")
          cols=tim.colors(10)
          #produce image of the mean
          par(mai=c(0,0,0.4,0));image(east[[resuse]],rev(north[[resuse]]),(out_mat_pred[,1300:1]),asp=1,main="",col=cols,xaxt="n",breaks=brk,xlab="",yaxt="n",ylab="")
           
           ##add legend to the plot based on breaks
           lb=c()
           labs=as.character((round(brk,2))[-length(brk)])
           for(i in 1:length(labs)){
                 lb[i]=paste(as.character(labs[i])," - ",round(brk[i+1],2))
           }
           legend("topright",legend=as.character(lb),col=cols,pch=15,pt.cex=2.4,bty="n",title=model_variable,cex=0.75)


          #produce image of variance
          #find the quantiles of the variance in order to select appropriate breaks in legend 
          qts=round(quantile(c(out_mat_var),c(0.1,0.9),na.rm=TRUE))
          brk=c(seq(qts[1],qts[2],length=10),100)
          #specify colours as greyscale
          cols=rev(grey(seq(0.1,0.9,len=10)))
          
          #proudce image of variance
          par(mai=c(0,0,0.4,0));image(east[[resuse]],rev(north[[resuse]]),(out_mat_var[,1300:1]),asp=1,main="",col=cols,xaxt="n",breaks=brk,xlab="",yaxt="n",ylab="")
          
          #add legend to plot based on breaks and colours. 
          lb=c()
          labs=as.character(round(brk,2)[-length(brk)])
          for(i in 1:length(labs)){
                lb[i]=paste(as.character(labs[i])," - ",round(brk[i+1],2))
          }
          legend("topright",legend=as.character(lb),col=cols,pch=15,pt.cex=2.4,bty="n",title=paste("Variance of ",model_variable,sep=""),cex=0.75)

        #write out the graphics window
        if(saveplots){
			dev.copy(png,map_image_file,height=720,width=700)
			dev.off()
			}

        progress_fn("Writing fit image",progress_rep_file)
       
        ## produce a goodness of fit plots
        windows(height=320,width=700)
          par(mfrow=c(1,2))
          
          #scatter plot of observed versus fitted values
          plot(mod$gam$y,mod$gam$fitted.values,xlab="Observed Values",ylab="Fitted Values")
          abline(a=0,b=1,col="red",lwd=2)
          #historgram of model residuals
          hist(mod$gam$y-mod$gam$fitted.values,main="",xlab="Model Residuals")

        if(saveplots){
			dev.copy(png,fit_image_file,height=320,width=700)
			dev.off()
			}

        
        ## Ste info of model fit for the netCDF file. 
        ## specifically, AIC, RMSE, Rsquared, model formula and summary table for coefficients and p values.
        if(mod_t=="norm_gam"){aic_val <- AIC(mod$gam)}else{aic_val <- AIC(mod$lme)}
        rmse <- sqrt(mean((mod$gam$y-mod$gam$fitted.values)^2))
        r2 <- (summary(mod$gam))$r.sq
        mod_formula <- paste(model_variable," ~ Intercept + ",paste(nm_var,collapse=" + "),sep="")
        mod_sum <- summary(mod$gam)


    }else{

        #if model did not converge / produced an error, then return empty plots and NA for summary stats

        rmse <- "NA Error"
        r2 <- "NA Error"
        mod_formula <- "NA Error"
        aic_val <- "NA Error"

        png(height=720,width=700,file=map_image_file)
          par(mfrow=c(1,2))
          par(mai=c(0,0,0.4,0))
          plot(c(0,1),c(0,1),type="n",main="Model Error",xaxt="n",yaxt="n",xlab="",ylab="")
          text(expand.grid(seq(0,1,len=5),seq(0,1,len=10)),"error",cex=0.7,col="grey")
          par(mai=c(0,0,0.4,0))
          plot(c(0,1),c(0,1),type="n",main="Model Error",xaxt="n",yaxt="n",xlab="",ylab="")
          text(expand.grid(seq(0,1,len=5),seq(0,1,len=10)),"error",cex=0.7,col="grey")
        dev.off()

        png(height=320,width=700,file=fit_image_file)
          par(mfrow=c(1,2))
          plot(0,type="n",xlab="",ylab="",main="Model Error")
          plot(0,type="n",xlab="",ylab="",main="Model Error")

        dev.off()

        out_mat_pred = matrix(NA,byrow=TRUE,ncol=1300,nrow=700)
        out_mat_var = matrix(NA,byrow=TRUE,ncol=1300,nrow=700)


    }

    ##  Predicted mean and variance will be written out as a single netCDF variables, with variance specified as an acillary variable
    ##  (as per the CF 1.6 Convention; see:  http://cf-pcmdi.llnl.gov/documents/cf-conventions/1.6/cf-conventions.html#ancillary-data).

    progress_fn("Writing output to file",progress_rep_file)

    ## open up one of the covariate files with same spatial resolution as derived map to use as a template
    v=unlist(covariate_data)
    fl=v[which(names(v)[1:which(v==nm_var[resuse])]=="linkfile")]
    download.file(url=fl,destfile="tempres.nc", mode="wb")

    ## open up the connection to the file to use as a template
    cov_dat <- nc_open("tempres.nc")

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
    ncatt_put(ncnew, 0, "keywords", paste("Mapping",paste(as.character(unlist(coverage_setup)),collapse=" , "),model_variable,sep=" , "))
    ncatt_put(ncnew, 0, "date_created", as.character(Sys.Date()), prec="text")
    ncatt_put(ncnew, 0, "date_modified", as.character(Sys.Date()), prec="text")
    ncatt_put(ncnew, 0, "date_issued", as.character(Sys.Date()), prec="text")

    ncatt_put(ncnew, 0, "creator_name", user_name)
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
    ncatt_put(ncnew, 0, "AIC", aic_val)
    ncatt_put(ncnew, 0, "Model_Terms", paste(c(row.names(mod_sum$pTerms.table),row.names(mod_sum$s.table)),collapse=","))
    ncatt_put(ncnew, 0, "Model_pVals", paste(round(c((mod_sum$pTerms.table[,3]),(mod_sum$s.table[,4])),4),collapse=","))
    
    ##finish and close the created netcdf file
    #nc_close(temp_netcdf_file)
    nc_close(ncnew)
}

#########################
#########################



