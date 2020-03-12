
cov_data_def <- function(coverage_setup){ 
 
		covariate_data = list()

		
		for(i in 1:length(names(coverage_setup))) {

        covariate_data[[i]] = list()
        url = names(coverage_setup)[i]

        covariate_data[[i]]$linkfile = url
        covariate_data[[i]]$vars = c(coverage_setup[[url]][[1]])
        covariate_data[[i]]$data_type = c(coverage_setup[[url]][[2]])
        
		}

	return(covariate_data)
	
}

#