\name{ErgosRenyi}
\alias{ErgosRenyi}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate a adjacency matrix using the Ergos-Renyi method
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ErgosRenyi(n = 10, symmetrical = TRUE, prob = 0.5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
	  How many nodes does the network have?
}
  \item{symmetrical}{
	  Is the network directed (TRUE) or indirected (FALSE)
}
  \item{prob}{
	  What probability has any two nodes of being linked between them?
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
  \item{A}{An adjacency matrix ready to use in igraph::graph_from_adjacency_matrix to generate a graph}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Michel Mendivenson Barragan Zabala, Statistics Student at Universidad Nacional de Colombia
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
function (n = 10, symmetrical = TRUE, prob = 0.5) 
{
    A = matrix(0, ncol = n, nrow = n)
    edges = rbinom(n = (n * (n - 1))/2, size = 1, prob = prob)
    A[upper.tri(A)] = edges
    if (symmetrical) {
        A = A + t(A)
    }
    else {
        edges = rbinom(n = (n * (n - 1))/2, size = 1, prob = prob)
        A[lower.tri(A)] = edges
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
