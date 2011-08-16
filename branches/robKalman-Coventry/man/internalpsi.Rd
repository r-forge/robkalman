\name{internalpsi}
\alias{internalpsi}
\alias{psiLS}
\alias{psiHuber}
\alias{psiTukey}
\alias{psiHampel}
\alias{.psi}

\title{Internal functions of package robKalman --- psi functions}

\description{
These functions are used internally by package robKalman for the ACM filter --- psi functions
}

\usage{
psiLS(t, rho=FALSE)
psiHuber(t, k=1.345, rho=FALSE) 
psiTukey(t, c=4.685, rho=FALSE)
psiHampel(t, a=2, b=4, c=8, flag="psi")
.psi(type)
}

\arguments{
  \item{t}{numerical vector to be modified}
  \item{rho}{logical: whether the \eqn{\psi}- (default) or \eqn{\rho}-function is used}
  \item{k}{tuning constant for \code{psiHuber}}
  \item{a}{tuning constant for \code{psiHampel}}
  \item{b}{tuning constant for \code{psiHampel}}
  \item{c}{tuning constant for \code{psiTukey} and \code{psiHampel}}
  \item{flag}{character: either "psi", "weights" or "deriv" to use  \eqn{\psi}-function (default), 
              weight function \eqn{\psi(t)/t} or  its derivative respectively}
  \item{type}{character: which \eqn{\psi}-function; current possibilities: "Huber", "Tukey", "Hampel", "Ident"}
}

\details{
Several possibilities are available as \eqn{\psi}-function to modify the correction step: "Huber", "Tukey", "Hampel", "Ident";
the can all be used through one interface --- \code{.psi}, returning the corresponding \eqn{\psi}-function
}


\value{Each  of the functions \code{psiLS}, \code{psiHuber}, \code{psiHampel}, and \code{psiTukey} return the value
of the corresponding \eqn{\psi}-function evaluated at \code{t}. \code{.psi} returns the corresponding \eqn{\psi}-function.
}

\author{
Bernhard Spangl \email{bernhard.spangl@boku.ac.at},\cr 
}


\seealso{
\code{\link{internalarGM}}
}


\keyword{internal}
