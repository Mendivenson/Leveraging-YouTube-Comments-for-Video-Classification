\name{vec}
\alias{vec}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Vectorizar la matriz de adyacencia
}
\description{
Vectorizar una matriz de adyacencia (O solo su triangular superior o inferior si symmetrical = TRUE)
}
\usage{
vec(A, symmetrical = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{A}{
	  La matriz de adyacencia que representa la red.
}
  \item{symmetrical}{
	  ¿Es la red dirigida o no?
}
}
\details{
	Se vectoriza por columnas de la matriz de arriba hacia abajo.
}
\value{
	\item{vec}{La vectorización de la matriz de adyacencia}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
	Michel Mendivenson Barragán Zabala, statistics student at Universidad
	Nacional de Colombia
}
\note{
	Obtenido este vector, el vector de localización puede ser obtenido con
	la función de Rbase::which()
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
function (A, symmetrical = TRUE) 
{
    n = ncol(A)
    vec = c()
    if (symmetrical) {
        A[upper.tri(A, diag = T)] = NA
    }
    for (i in 1:ncol(A)) {
        vec = c(vec, A[!is.na(A[, i]), i])
    }
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
