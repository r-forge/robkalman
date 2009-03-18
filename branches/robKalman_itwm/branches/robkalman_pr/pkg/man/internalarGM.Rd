\name{internalarGM}
\alias{internalarGM}
\alias{.ARmodel}
\alias{.invCp}
\alias{.Weights}
\alias{.startval}
\alias{.BH}
\alias{.BB}
\alias{.weights}
\alias{.IWLS}

\title{Internal functions of package robKalman --- psi functions}

\description{
These functions are used internally by package robKalman for the ACM filter --- \eqn{\psi}-functions
}

\usage{
.ARmodel(x, p)
.invCp(p, s, Phi)
.Weights(p, Z, invCp, type, psi2, c)
.startval(y, Z, tol)
.BH(k=1.345) 
.BB(c=4.685)
.weights(r, s, u, v, psi1, ...)
.IWLS(y, Z, phi.ini, s.ini, u, v, psi1, niter, tol, ...)
}

\arguments{
  \item{x}{univarite time series (vector)}
  \item{p}{order of AR(p) process}
  \item{s}{vector of \code{sx} and innovation scale estimates for AR(p-1) models of order \eqn{1} to \eqn{(p-1)}}
  \item{Phi}{\eqn{(p-1)\times(p-1)}{(p-1)x(p-1)} matrix of AR(p-1) model parameters}
  \item{Z}{AR(p) model matrix} 
  \item{invCp}{matrix from function \code{.invCp} to compute metric}
  \item{type}{type of GM-estimates --- character: currently:  "Mallows" or "Schweppe"}
  \item{psi1, psi2}{type of \eqn{\psi} function; current possibilities: "Huber", "Tukey", "Hampel", "Ident"}
  \item{c, k}{tuning constants}
  \item{y}{response vector of AR(p) model}
  \item{tol}{tolerance level} 
  \item{r}{residuals}
  \item{s}{innovations scale parameter}
  \item{u, v}{weights}
  \item{...}{additional arguments (tuning constants) for influence functions}
  \item{phi.ini}{initial AR(p) model parameters}
  \item{s.ini}{initial innovations scale parameter}
  \item{niter}{maximal number of iterations}
}

\details{
to be filled
}


\value{
\code{.ARmodel(x, p)} returns design matrix \code{Z} and response vector \code{y} of an AR(p) model (as list with corresponding elements).\cr
\code{.invCp(p, s, Phi)} computes the inverse \eqn{p \times p}{p x p} covariance matrix.\cr
\code{.Weights} computes weights for  Mallows- or Schweppe-type GM-estimates; returns a list with elements \code{u}, \code{v}.\cr
\code{.startval} computes appropriate starting values; returns a list with elements \code{phi}, \code{s}.\cr
\code{.BH} computes appropriate constant to obtain a consistent estimate for \eqn{\sigma} when using Huber's \eqn{\psi} function.\cr
\code{.BB} computes appropriate constant to obtain a consistent estimate for \eqn{\sigma} when using Tukey's \eqn{\psi} function.\cr
\code{.weights} computes appropriate weights for reweighting --- returns a vector.\cr
\code{.IWLS}  iteratively reweighted least squares algorithm; returns a list with elements 
 \code{phi}, \code{s},  \code{w}, \code{B}, \code{niter}.\cr
}

\author{
Bernhard Spangl \email{bernhard.spangl@boku.ac.at},\cr 
}


\seealso{
\code{\link{internalpsi}}
}


\keyword{internal}
