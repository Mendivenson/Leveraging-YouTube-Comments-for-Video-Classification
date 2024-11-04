\name{vecToA}
\alias{vecToA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Obtener la matriz de adyacencia a partir de su vectorización
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
vecToA(x, n = 10, symmetrical = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
	  UN vector que represente la vectorización de la matriz A. 
}
  \item{n}{
	  ¿Cuántos nodos originalmente tiene la red?
}
  \item{symmetrical}{
	  ¿La red es dirigida o no dirigida?
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
        A = matrix(0, ncol = n, nrow = n)
        A[lower.tri(A)] = x
        A = A + t(A)
    }
    else {
        A = matrix(x, ncol = n, nrow = n)
    }
    return(A)
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
