\name{ACMfilt}
\alias{ACMfilt}

\title{ACM filter - S-Plus type arguments}

\description{
This function realizes the ACM filter with a S-Plus type signature.
}

\usage{
ACMfilt(x, gm, s0=0, psi="Hampel", a=2.5, b=a, c=5.0, flag="weights", lagsmo=TRUE)
}

\arguments{
\item{x}{univarite time series (vector)}
\item{gm}{list as produced by function \code{arGM}, with elements
           \code{ar} containing the AR(p) coefficient estimates, \code{sinnov} containing 
           innovation scale estiamtes from AR(p) fits of orders 1 through p;
           \code{Cx} containing an estimate of the p by p autocovariance matrix, and       
           \code{mu}, the estimated mean of \code{x}} 
  \item{s0}{scale of nominal Gaussian component of additive noise}
  \item{psi}{influence function to be used (default: Hampel's \eqn{\psi} function, which is the only one available at the moment)}
  \item{a,b,c}{tuning constants for Hampel's \eqn{\psi}-function}
  \item{flag}{character, if "weights", use \eqn{\psi(t)/t} to calculate the weights; if "deriv", use \eqn{\psi'(t)}}
  \item{lagsmo}{logical, if \code{TRUE}, lag p-1 smoothing is performed; else filtering from the top of \eqn{\hat X_t} is performed}
}

\details{
to be filled}


\value{a list with elements 
\item{filt.ck}{the classical Kalman filter}
\item{filt}{the ACM filter}
\item{st}{time-dependent scale parameter estimate}
\item{r}{AR-residuals}
}    


\author{
Bernhard Spangl \email{bernhard.spangl@boku.ac.at},\cr 
  }


\seealso{
\code{\link{internalarGM}}, \code{\link{internalpsi}}, \code{\link{internalACM}},
\code{\link{arGM}}, \code{\link{recFilter}}
}

\examples{
require(robKalman)

##  AO model:
set.seed(361)
Eps <- as.ts(rnorm(100))
ar2 <- arima.sim(list(ar = c(1, -0.9)), 100, innov = Eps)
Binom <- rbinom(100, 1, 0.1)
Noise <- rnorm(100,sd = 10)
y <- ar2 + as.ts(Binom*Noise)

y.arGM <- arGM(y, 3)
y.ACMfilt <- ACMfilt(y, y.arGM)

plot(y)
lines(y.ACMfilt$filt, col=2)
lines(ar2,col="green")

}

\references{
Martin, R.D. and Zeh, J.E. (1978): Generalized M-estimates for  Autoregression Including Small-sample Efficiency Robustness \cr 
Martin, R.D. (1980): Robust Estimation of Autoregressive Models.\cr 
Martin, R.D. and Thomson, D.J. (1982): Robust-resistent Spectrum Estimation. \cr 
Stockinger, N. and Dutter, R. (1987): Robust Time Series Analysis: A Survey.\cr 
}

\keyword{robust}
\keyword{ts}
