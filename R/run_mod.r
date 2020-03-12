run_mod <- function(newdat,data_fam,rand_grp,mult_year,form){	
	
      ## find the correct grouping variable to use to define individual observations within a group within a year. 
      ##subset the data to the first time observation and the first group
      
	  if(!is.null(mult_year)){
		  
		  if(!is.null(rand_grp)){
		  x=newdat[newdat$rnd_group==newdat$rnd_group[1] & newdat$tm_var==newdat$tm_var[1],]
		  }else{
		  x=newdat[newdat$tm_var==newdat$tm_var[1],]
		  }##fins all columns in the data frame that have unique values for all enties in the subset data
		  
		  v=which(apply(x,2,function(X){length(unique(X))})==dim(x)[1])
		  ##find which of these varaibles are a factor and choose the first
		  tm_id <- which(sapply(x[,v],class)=="factor")[1]
		  ##this is the name of the variable to define groups/temporal slices in the mdoel
		  tmp_nm = names(v)[tm_id]
		  ##define a new column called temp_rand with this grouping factor in
		  newdat$temp_rand = newdat[,which(names(newdat)==tmp_nm)]
     
	 }
      #by default specify the type of model used as a mixed effects gam
      mod_t = "mix_gam"
      
      #based on whether there is a random factor and whether autocrellation is needed, fit the correct model
      if(!is.null(rand_grp) & is.null(mult_year)){
        #progress_fn("Applying GAMM, grouped data",progress_rep_file)
        mod=try(gamm(as.formula(form),random=list(rnd_group=~1),data=newdat,family=data_fam,niterPQL=5))
      }else{

        if(is.null(rand_grp) & !is.null(mult_year)){
          #progress_fn("Applying GAMM, multi-year",progress_rep_file)
          mod=try(gamm(as.formula(form),correlation=corAR1(form=~tm_var|temp_rand),data=newdat,family=data_fam,niterPQL=5))
        }else{
          if(!is.null(rand_grp) & !is.null(mult_year)){
            #progress_fn("Applying GAMM, multi-year and grouped",progress_rep_file)
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
	 
	 return(mod)
}	 
