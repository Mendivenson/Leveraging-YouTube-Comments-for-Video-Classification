\name{locToVec}
\alias{locToVec}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	A partir del vector de localizaciones obtener la vectorización de la
	matriz de adyacencia
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
locToVec(x, n = 10, symmetrical = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
	  Vector de localizaciones.
}
  \item{n}{
	  ¿Cuántos nodos originalmente tiene la red? 
}
  \item{symmetrical}{
	  ¿Es la red dirigida o no dirigida?
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
	Michel Mendivenson Barragán Zabala, statistic student at Universidad
	Nacional de Colombia.
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or standard data sets, see data().

## The function is currently defined as
function (x, n = 10, symmetrical = FALSE) 
{
    if (symmetrical) {
        vec = rep(0, (n * (n - 1))/2)
    }
    else {
        vec = rep(0, n^2)
    }
    vec[x] = 1
    return(vec)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
