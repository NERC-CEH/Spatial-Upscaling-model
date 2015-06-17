\name{EcoMAPS_R}
\alias{EcoMAPS_R}


%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Main EcoMAPS function to produce GB maps from point data
}
\description{
Uses spatial statistical model to upscale point data to GB coverage mapped data sets using detailed covariate information}
\usage{
EcoMAPS_R(coverage_setup,time_slices,progress_rep_file = "progress.txt",user_name = "Test Harness",                              
   email_address = "test@test.com",csv_file = "InputRDataFile_altitudeplusLCM2007.csv",map_image_file = "map_output.png",                  fit_image_file = "fit_output.png",temp_netcdf_file = "temp.nc",output_netcdf_file = "output.nc",mult_year = NULL,                       rand_grp = NULL,data_type = "Cont",model_variable)                                            

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{coverage_setup}{
List of covariate files and the names of the variables within them to be used in the model 
}
  \item{time_slices}{
Defines which time slice to use for each input data set. eg precip runs 1970-2012 time_slice 1 = 1970
}
  \item{progress_rep_file}{
Name of the file where the progress updates will be sent to
}
  \item{user_name}{
Details of the user name to pass to the resulting netcdf header
}
  \item{email_address}{
Email address of user to use as contact info in netcdf header
}
  \item{csv_file}{
Name of the point based csv file from the spatial concatenation in python
}
  \item{map_image_file}{
Name to give the png of the map image produced
}
  \item{fit_image_file}{
Name to give the png of the model summary plots produced
}
  \item{temp_netcdf_file}{
Name of netcdf files that are stored temporarily during modelling 
}
  \item{output_netcdf_file}{
Name of the final netcdf file that is written out
}
  \item{mult_year}{
Variable in the source data set that defines the temporal aspect of the data 
}
  \item{rand_grp}{
Variable in the source data set that defines any grouping structure present in the data that needs to be accounted for
}
  \item{data_type}{
Type of variable that is to be modelled - continuous, binary or count
}
  \item{model_variable}{
Name of variable (as it appears in csv file) that is to be modelled
}
}
\details{
Currently uses a GAMM to do the modelling with the spatial corellation accounted for by the inclusion of a tensor product msmooth interaction between the spatial axes. Variation os taken straight from the model currently, but this will hopefuilly be replaced by bootstrapping in the near future. 
}
\value{
Returns a figure containing the mean and varaicne maps, a figure with goodness of fit plots and a NetCDF file containing the estimated gridded coverage over GB.
}

\author{
Peter Henrys
}

\examples{

##wont work

#EcoMAPS_R(
#coverage_setup = list("http://thredds-prod.nerc-lancaster.ac.uk/thredds/fileServer/altitude1000mdetail/BlockMeanGrid_1000.nc" = list("altitude")),
#time_slices=NULL,
#progress_rep_file = "progress.txt",
#user_name = "Test Harness" ,
#email_address = "test@test.com",
#csv_file = "InputRDataFile_altitudeplusLCM2007.csv",
#map_image_file = "map_output.png",
#fit_image_file = "fit_output.png",
#temp_netcdf_file = "temp.nc",
#output_netcdf_file = "output.nc",
#mult_year <- NULL , 
#rand_grp <- "SERIES_NUM" ,
#data_type <- "Cont" ,
#model_variable="loi"
#)


}