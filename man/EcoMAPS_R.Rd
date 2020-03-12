\name{EcoMAPS_R}
\alias{EcoMAPS_R}


%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Main EcoMAPS wrapper function to produce GB maps from point data
}
\description{
Uses spatial statistical model to upscale point data to GB coverage mapped data sets using detailed covariate information}
\usage{
EcoMAPS_R(
PredVars=c("elevation1km","LCM2007_1kmDomTar","airTemperature","precipitation"),
time_slices=NULL,
csv_file = "CS_LOI_Point_PlusCovs.csv",
map_image_file = "map_plot.png",
fit_image_file = "fit_plot.png",
temp_netcdf_file = "temp",
output_netcdf_file = "output.nc",
mult_year <- NULL, 
rand_grp <- "SERIES_NUM",
data_type <- "Cont",
model_variable="loi",
saveplots=TRUE,
write_2_netcdf=TRUE
)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{PredVars}{
Names of covariates to be used in the model 
}
  \item{time_slices}{
Defines which time slice to use for each input data set. eg precip runs 1970-2012 time_slice 1 = 1970
}
  \item{csv_file}{
Name of the point based csv file from the spatial concatenation in python
}
  \item{output_netcdf_file}{
Name of the final netcdf file that is written out
}
  \item{temp_netcdf_file}{
Name of the temporary file to write intermittent files to
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
  \item{saveplots}{
Logical, specifying whether plots should be saved to file
}
  \item{map_image_file}{
Name to give the png of the map image produced
}
  \item{fit_image_file}{
Name to give the png of the model summary plots produced
}
  \item{write_2_netcdf}{
Logical. should a netcdf file of the predicted map be written out to file specified by output_netcdf_file
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
#PredVars=c("elevation1km","LCM2007_1kmDomTar","airTemperature","precipitation"),
#time_slices=NULL,
#progress_rep_file = "progress.txt",
#user_name = "Test Harness" ,
#csv_file = "OutVars_EbergN_X.csv",
#output_netcdf_file = "EbergN.nc",
#mult_year <- NULL , 
#rand_grp <- "EASTING_km" ,
#data_type <- "Cont" ,
#model_variable="EbergN",
#saveplots=TRUE,
#map_image_file = "map_ebergN.png",
#fit_image_file = "fit_ebergN.png"
#)





}