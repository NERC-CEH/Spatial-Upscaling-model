\name{progress_fn}
\alias{progress_fn}


%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Report progress of EcoMAPS applications and write to text file
}
\description{
Report progress of EcoMAPS applications and write to text file}
\usage{
progress_fn(message,progress_rep_file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{message}{
Character string of progress statement to write to the text file
}
  \item{progress_rep_file}{
Name of the file where the progress updates will be sent to
}
}
\details{
simple function to write out messages for progress reporting of the main EcoMAPS function
}
\value{
Saves a file to progress_rep_file containing the line as specified by message 
}

\author{
Phil Jenkins, Peter Henrys
}

\examples{

##Extract model terms
progress_fn("EcoMAPS run Initialised", progress_report.txt)

}