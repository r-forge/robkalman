\name{arGM}
\alias{arGM}

\title{GM-estimates for AR parameters}

\description{
This function realizes Martin's GM-estimates for AR parameters.
}

\usage{
arGM(x, order=1, chr=1.5, iterh=maxiter, cbr=5.0, iterb=maxiter, 
     psi2="Tukey", c=4.0, type="Mallows", k=1.5, maxiter=100, 
     tol=1e-08, equal.LS=FALSE, ...) 
}

\arguments{
\item{x}{univarite time series (vector)}
\item{order}{order of AR(p) process}
\item{chr}{tuning constant for Huber's \eqn{\psi} function}
\item{iterh}{number of iterations for IWLS-alogrithm using  Huber's \eqn{\psi} function}
\item{cbr}{tuning constant for Tukey's \eqn{\psi} function}
\item{iterb}{number of iterations for IWLS-alogrithm using Tukey's \eqn{\psi} function}
\item{psi2}{influence function to determine the 'size of \eqn{z_i}',  either "Ident", "Huber" or "Tukey"}
\item{c}{tuning constant for psi2}
\item{type}{type of GM-estimates, either "Mallows" or "Schweppe"}
\item{k}{tuning constant for centering}
\item{maxiter}{maximal number of iteration}
\item{tol}{tolerance level}
\item{equal.LS}{logical, for testing purpose only}
\item{...}{further parameters to be passed to the functions \code{HuberM} or \code{hubers}}  
}

\details{
to be filled}
           
\value{a list with elements
\item{ar}{parameter estimate}
\item{sinnov}{scale estimate for the innovations from AR(p) fits of orders 1 through p}
\item{Cx}{an estimate of the \eqn{p \times p}{p x p} autocovariance matrix}
\item{mu}{ location estimate of \code{x}}
\item{sx}{Huber scale estimate}
\item{u,v}{weights for  Mallows- or Schweppe-type GM-estimates}
\item{w}{weights from IWLS algorithm}
\item{BH}{consistency constant for \eqn{\sigma} when using Huber's \eqn{\psi} function}
\item{BB}{consistency constant for \eqn{\sigma} when using Tukey's \eqn{\psi} function}
\item{niterh}{number of iterations for IWLS-alogrithm using  Huber's \eqn{\psi} function}
\item{niterb}{number of iterations for IWLS-alogrithm using  Tukey's \eqn{\psi} function}
\item{niter.testing}{for testing purposes only}   
}

\author{
Bernhard Spangl \email{bernhard.spangl@boku.ac.at},\cr 
  }


\seealso{
\code{\link{internalarGM}}, \code{\link{internalpsi}}
}

\examples{
}

\references{
Martin, R.D. and Zeh, J.E. (1978): Generalized M-estimates for  Autoregression Including Small-sample Efficiency Robustness \cr
Martin, R.D. (1980): Robust Estimation of Autoregressive Models.  \cr
Martin, R.D. and Thomson, D.J. (1982): Robust-resistent Spectrum Estimation.  \cr
Stockinger, N. and Dutter, R. (1987): Robust Time Series Analysis: A Survey.  \cr
}

\keyword{robust}
\keyword{ts}
