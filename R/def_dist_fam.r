
def_dist_fam <- function(dat,rand_grp,mult_year,model_variable,data_type){

	## define the family of distributions to choose from
    poss_fam <- c("gaussian","Gamma","quasibinomial","quasipoisson")

    #create new dataset so that we do not over-write the input one
    newdat=dat

    # ## specify the response variable to include in the model
     newdat$response <- newdat[,which(names(newdat)==model_variable)]
    # ## specify the random grouping facotr to include in the model, if it exists
     if(!is.null(rand_grp)){newdat$rnd_group <- newdat[,which(names(newdat)==rand_grp)]}
    # ## specify the temporal component variable to include in the model, if it exists
     if(!is.null(mult_year)){newdat$tm_var <- newdat[,which(names(newdat)==mult_year)]}

    # #progress_fn("Checking distribution of Response Variable",progress_rep_file)

    # #test normality of response if it is continuos. if normality test fails, ensure that a Gamma error distribution is used.
     if(data_type=="Cont" & length(newdat$response)<4999){
        
        # ##use shapiro  wilks test to assess normality
        norm.test.res <- shapiro.test(newdat$response)

        # ##if null hypothesis of normality rejected, then specify data type to be continuous, but not gaaussian
         if(norm.test.res$p.value<0.05){
         
             data_type="ContNonG"
        
         }
        
      }

    #select the distribution family to use given the type of data being modelled
    data_fam <- switch(data_type,"Cont"=poss_fam[1],"ContNonG"=poss_fam[2],"Count"=poss_fam[4],"Binary"=poss_fam[3])

	return(list(data_fam=data_fam,newdat=newdat))
}


