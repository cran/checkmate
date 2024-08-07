% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/checkComplex.R
\name{checkComplex}
\alias{checkComplex}
\alias{check_complex}
\alias{assertComplex}
\alias{assert_complex}
\alias{testComplex}
\alias{test_complex}
\alias{expect_complex}
\title{Check if an argument is a vector of type complex}
\usage{
checkComplex(
  x,
  any.missing = TRUE,
  all.missing = TRUE,
  len = NULL,
  min.len = NULL,
  max.len = NULL,
  unique = FALSE,
  names = NULL,
  typed.missing = FALSE,
  null.ok = FALSE
)

check_complex(
  x,
  any.missing = TRUE,
  all.missing = TRUE,
  len = NULL,
  min.len = NULL,
  max.len = NULL,
  unique = FALSE,
  names = NULL,
  typed.missing = FALSE,
  null.ok = FALSE
)

assertComplex(
  x,
  any.missing = TRUE,
  all.missing = TRUE,
  len = NULL,
  min.len = NULL,
  max.len = NULL,
  unique = FALSE,
  names = NULL,
  typed.missing = FALSE,
  null.ok = FALSE,
  .var.name = vname(x),
  add = NULL
)

assert_complex(
  x,
  any.missing = TRUE,
  all.missing = TRUE,
  len = NULL,
  min.len = NULL,
  max.len = NULL,
  unique = FALSE,
  names = NULL,
  typed.missing = FALSE,
  null.ok = FALSE,
  .var.name = vname(x),
  add = NULL
)

testComplex(
  x,
  any.missing = TRUE,
  all.missing = TRUE,
  len = NULL,
  min.len = NULL,
  max.len = NULL,
  unique = FALSE,
  names = NULL,
  typed.missing = FALSE,
  null.ok = FALSE
)

test_complex(
  x,
  any.missing = TRUE,
  all.missing = TRUE,
  len = NULL,
  min.len = NULL,
  max.len = NULL,
  unique = FALSE,
  names = NULL,
  typed.missing = FALSE,
  null.ok = FALSE
)

expect_complex(
  x,
  any.missing = TRUE,
  all.missing = TRUE,
  len = NULL,
  min.len = NULL,
  max.len = NULL,
  unique = FALSE,
  names = NULL,
  typed.missing = FALSE,
  null.ok = FALSE,
  info = NULL,
  label = vname(x)
)
}
\arguments{
\item{x}{[any]\cr
Object to check.}

\item{any.missing}{[\code{logical(1)}]\cr
Are vectors with missing values allowed? Default is \code{TRUE}.}

\item{all.missing}{[\code{logical(1)}]\cr
Are vectors with no non-missing values allowed? Default is \code{TRUE}.
Note that empty vectors do not have non-missing values.}

\item{len}{[\code{integer(1)}]\cr
Exact expected length of \code{x}.}

\item{min.len}{[\code{integer(1)}]\cr
Minimal length of \code{x}.}

\item{max.len}{[\code{integer(1)}]\cr
Maximal length of \code{x}.}

\item{unique}{[\code{logical(1)}]\cr
Must all values be unique? Default is \code{FALSE}.}

\item{names}{[\code{character(1)}]\cr
Check for names. See \code{\link{checkNamed}} for possible values.
Default is \dQuote{any} which performs no check at all.
Note that you can use \code{\link{checkSubset}} to check for a specific set of names.}

\item{typed.missing}{[\code{logical(1)}]\cr
If set to \code{FALSE} (default), all types of missing values (\code{NA}, \code{NA_integer_},
\code{NA_real_}, \code{NA_character_} or \code{NA_character_}) as well as empty vectors are allowed
while type-checking atomic input.
Set to \code{TRUE} to enable strict type checking.}

\item{null.ok}{[\code{logical(1)}]\cr
If set to \code{TRUE}, \code{x} may also be \code{NULL}.
In this case only a type check of \code{x} is performed, all additional checks are disabled.}

\item{.var.name}{[\code{character(1)}]\cr
Name of the checked object to print in assertions. Defaults to
the heuristic implemented in \code{\link{vname}}.}

\item{add}{[\code{AssertCollection}]\cr
Collection to store assertion messages. See \code{\link{AssertCollection}}.}

\item{info}{[\code{character(1)}]\cr
Extra information to be included in the message for the testthat reporter.
See \code{\link[testthat]{expect_that}}.}

\item{label}{[\code{character(1)}]\cr
Name of the checked object to print in messages. Defaults to
the heuristic implemented in \code{\link{vname}}.}
}
\value{
Depending on the function prefix:
 If the check is successful, the functions 
 \code{assertComplex}/\code{assert_complex} return 
 \code{x} invisibly, whereas
 \code{checkComplex}/\code{check_complex} and 
 \code{testComplex}/\code{test_complex} return 
 \code{TRUE}.
 If the check is not successful, 
 \code{assertComplex}/\code{assert_complex}
 throws an error message, 
 \code{testComplex}/\code{test_complex}
 returns \code{FALSE},
 and \code{checkComplex}/\code{check_complex} 
 return a string with the error message.
 The function \code{expect_complex} always returns an
 \code{\link[testthat]{expectation}}.
}
\description{
Check if an argument is a vector of type complex
}
\details{
This function does not distinguish between
\code{NA}, \code{NA_integer_}, \code{NA_real_}, \code{NA_complex_}
\code{NA_character_} and \code{NaN}.
}
\examples{
testComplex(1)
testComplex(1+1i)
}
\seealso{
Other basetypes: 
\code{\link{checkArray}()},
\code{\link{checkAtomic}()},
\code{\link{checkAtomicVector}()},
\code{\link{checkCharacter}()},
\code{\link{checkDataFrame}()},
\code{\link{checkDate}()},
\code{\link{checkDouble}()},
\code{\link{checkEnvironment}()},
\code{\link{checkFactor}()},
\code{\link{checkFormula}()},
\code{\link{checkFunction}()},
\code{\link{checkInteger}()},
\code{\link{checkIntegerish}()},
\code{\link{checkList}()},
\code{\link{checkLogical}()},
\code{\link{checkMatrix}()},
\code{\link{checkNull}()},
\code{\link{checkNumeric}()},
\code{\link{checkPOSIXct}()},
\code{\link{checkRaw}()},
\code{\link{checkVector}()}
}
\concept{basetypes}
