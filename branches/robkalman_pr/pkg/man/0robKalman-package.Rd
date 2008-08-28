\name{robKalman-package}
\alias{robKalman-package}
\alias{robKalman}
\docType{package}
\title{
robKalman -- routines for robust Kalman filtering
}
\description{
\pkg{robKalman } provides routines for robust Kalman filtering. 
Currently, the ACM-filter and the rLS-Filter are provided.
}
\details{
\tabular{ll}{
Package: \tab robKalman\cr
Version: \tab 0.2 \cr
Date: \tab 2008-01-17 \cr
Depends: \tab R(>= 2.3.0), methods, graphics, startupmsg, dse1, dse2, MASS\cr 
Imports: \tab stats\cr
SaveImage: \tab no\cr
LazyLoad: \tab yes\cr
License: \tab GPL (version 2 or later)\cr
URL: \tab http://www.uni-bayreuth.de/departments/math/org/mathe7/robKalman/\cr
Packaged: \tab Sun June 11 10:16:03 2006; Peter\cr
Built: \tab R 2.4.0dev; i386-pc-mingw32; 2006-06-11 10:20:11; windows\cr
}}

\section{Setup}{
We work in the setup of the time-invariant, linear, Gaussian state space model (ti-l-G-SSM)
with \eqn{p} dimensional states \eqn{x_t} and \eqn{q} dimensional observations \eqn{y_t},
with \strong{initial condition}
\deqn{x_0 \sim {\cal N}_p(a,S),}{x_0 ~ N_p(a,S),}

\strong{state equation}
\deqn{x_t = F x_{t-1} +  v_t, \qquad v_t \sim {\cal N}_p(0,Q),\qquad t\ge 1,}{x_t = F x_{t-1} +  v_t,          v_t ~ N_p(0,Q),  t>=1,}

\strong{observation equation}
\deqn{y_t = Z x_{t}   +  \varepsilon_t, \qquad \varepsilon_t \sim {\cal N}_q(0,V),\qquad t\ge 1,}{y_t = Z x_t   +  e_t,      e_t ~ N_q(0,V),  t>=1,}
and where all random variable \eqn{x_0}, \eqn{v_t}, \eqn{\varepsilon_t}{e_t} are independent.
}

\section{Classes}{
We have introduced 6 larger groups of classes: 
\itemize{
\item{}{ State Space Model classes: These realize state space models}
\item{}{ Filter classes: These are the classes of the return values of our filters}
\item{}{ Control classes: They gather control information for our filters }
\item{}{ SSM distributions: They gather distributional assumptions of our filters }
\item{}{ SSM + Distribution classes: They group SSM and distributional assumptions} 
\item{}{ SSM - Simulation classes: These are the return values of our simulation routines}
}
Additionally we have borrowed some code of our package \pkg{distrMod} implementing
positive (semi-)definite matrix classes. Finally there are some Class unions.

\preformatted{

%----------------------------
State Space Model classes
%----------------------------
slots: [<name>(<class>)]

%------ "SSM"
name ("character"),  
F("Hyperparamtype"), Z("Hyperparamtype"),
Q("Hyperparamtype"), V("Hyperparamtype"), 
p("numeric"), q("numeric"),  a("numeric"), 
S("Hyperparamtype"), time("zoo")

Hierarchie:
"SSM"
|>"TimeInvariantSSM"

%----------------------------
Filter classes
%----------------------------
slots: [<name>(<class>)] (skipping inherited ones)

%---"recFilter"
name ("character"),  
SSM("SSM"), Y("matrix"), time("zoo"),
X.filtered("matrix"), X.predicted("matrix"),
Cov.filtered("array"), Cov.predicted("array"), Kalman.Gain("array"),
X.filtered("matrix"), X.predicted("matrix"),

%---"robrecFilter"
name.rob ("character"),  
X.rob.filtered("matrix"), X.rob.predicted("matrix"),
Cov.rob.filtered("array"), Cov.rob.predicted("array"),
rob.correction.ctrl("list"), rob.prediction.ctrl("list"),
Cov.rob.filtered.sim("array"), Cov.rob.predicted.sim("array")
Kalman.rob.Gain("array"), IndIO("logical"), IndAO("logical"),
nsim("numeric"), RNGstate("integer"),

Hierarchie:
"recFilter"
|>"robrecFilter"

%----------------------------
Control classes
%----------------------------
slots: [<name>(<class>)] (skipping inherited ones)


%---"RecFiltControl"
name("character"), init("function"),
predict("function"), correct("function"),                                 

%---"KalmanControl"

%---"robrecControl"
name.rob("character"), init.rob("function"),
predict.rob("function"), correct.rob("function"), 
controls("list")

Hierarchie:
"RecFiltControl"
|>"KalmanControl"
|>"robrecControl"

%----------------------------
SSM distributions
%----------------------------
slots: [<name>(<class>)] (skipping inherited ones)

%---"SSMDistribution.f"
r.init("function"), r.innov("function"), r.obs("function"),

%---"SSMellDistribution.f"
m.init("sHyperparamtype"), S.init("Hyperparamtype"),
m.innov("sHyperparamtype"), S.innov("Hyperparamtype"),
m.obs("sHyperparamtype"), S.obs("Hyperparamtype")

%---"SSMConvDistribution.f"
ideal("SSMDistribution.f"), cont("SSMellDistribution.f"),
r.IO("numeric"), r.AO("numeric"),

Hierarchie:
"SSMDistribution.f"
|>"SSMellDistribution.f"


%----------------------------
SSM + Distribution classes 
%----------------------------
slots: [<name>(<class>)] 

%---"SSMwithDistribution"
SSM("SSM"), Distribution("SSMDistribution.f")

%---"SSMwithConvDistribution"
SSM("SSM"), Distribution("SSMConvDistribution.f")

%----------------------------
SSM - Simulation classes 
%----------------------------
slots: [<name>(<class>)] (skipping inherited ones)

%---"SSMsimulation" 
SSM("SSM"), Distr("SSMDistr"), RNGstate("numeric"), 
states("ArrayOrMatrix"), obs("ArrayOrMatrix"),

%---"SSMcontSimulation"
states.id("ArrayOrMatrix"), obs.id("ArrayOrMatrix"),                              
Ind.IO("logical"), Ind.AO("logical")

Hierarchie:
"SSMsimulation"
|>"SSMcontSimulation"


%----------------------------
Matrix classes
%----------------------------
slots: none

Hierarchie:
"PosSemDefSymmMatrix"
|>"PosDefSymmMatrix"

%----------------------------
Class unions
%----------------------------
"ArrayOrMatrix"   = union("array", "matrix")
"Hyperparamtype"  = union("NULL","ArrayOrMatrix", "OptionalFunction")
"sHyperparamtype" = union("Hyperparamtype", "numeric")
"SSMDistr" = union("SSMDistribution.f", "SSMConvDistribution.f")

}
}

\section{Methods}{
yet to be filled}
\section{Functions}{
\preformatted{

general recursive filters
+recursiveFilter
 -KalmanFilter 
 -rLSFilter
 -ACMfilter

ACMfilter:
+ACMfilt

GM-estimators for AR models
+arGM

utilities:
+Euclidnorm,  
+rcvmvnorm,      
+Huberize,       
+limitS

simulation of AO contaminated state space models       
+simulateState,
+simulateObs,
}}

\section{Acknowledgement}{
We thank Paul Gilbert for their help
in preparing this package.}

\author{
Peter Ruckdeschel (Maintainer) \email{Peter.Ruckdeschel@itwm.fraunhofer.de},\cr 
Bernhard Spangl \email{bernhard.spangl@boku.ac.at},\cr 
}

\references{
Martin, R.D. and Zeh, J.E. (1978): Generalized M-estimates for  Autoregression Including Small-sample Efficiency Robustness \cr
Martin, R.D. (1979): Approximate Conditional-mean Type Smoothers and Interpolators.\cr
Martin, R.D. (1980): Robust Estimation of Autoregressive Models.  \cr
Martin, R.D. (1981): Robust Methods for Time Series\cr
Martin, R.D. and Thomson, D.J. (1982): Robust-resistent Spectrum Estimation. \cr
Ruckdeschel, P. (2001) \emph{Ans\"atze zur Robustifizierung des
Kalman Filters.} Bayreuther Mathematische Schriften, Vol. 64. \cr
Stockinger, N. and Dutter, R. (1987): Robust Time Series Analysis: A Survey.  \cr
}

\section{Start-up-Banner}{
You may suppress the start-up banner/message completely by setting \code{options("StartupBanner"="off")}
somewhere before loading this package by \code{library} or \code{require} in your R-code / R-session.

If option \code{"StartupBanner"} is not defined (default) or setting    
\code{options("StartupBanner"=NULL)} or  \code{options("StartupBanner"="complete")} 
the complete start-up banner is displayed.

For any other value of option \code{"StartupBanner"} (i.e., not in \code{c(NULL,"off","complete")})
only the version information is displayed.

The same can be achieved by wrapping the \code{library} or \code{require}  call into
either \code{suppressStartupMessages()} or \code{onlytypeStartupMessages(.,atypes="version")}. 

As for general \code{packageStartupMessage}'s, you may also suppress all
 the start-up banner by wrapping the \code{library} or \code{require} 
 call into \code{suppressPackageStartupMessages()} from 
 \pkg{startupmsg}-version 0.5 on.
 }

\section{Demos}{
Demos are available --- see \code{demo(package="robKalman")}}
\keyword{robust}
\keyword{ts}
