% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/reliability.R
\name{sed}
\alias{sed}
\title{Standard error of difference}
\usage{
sed(sem1, sem2, type = "sem", sigma = NULL)
}
\arguments{
\item{sem1}{Either the SEM or reliablity of the 1st test}

\item{sem2}{Either the SEM or reliability of the 2nd test}

\item{type}{Either sem, if specifying SEMs or r, if specifying reliability estimates}

\item{sigma}{The standard deviation of the test scores if reliablity estimates were provided}
}
\description{
Calculate the standard error of difference given the SEM of two tests or the reliablity of two tests and the standard deviation of the total test scores
}

