% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/checkClass.R
\name{checkClass}
\alias{checkClass}
\alias{check_class}
\alias{assertClass}
\alias{assert_class}
\alias{testClass}
\alias{test_class}
\alias{expect_class}
\title{Check the class membership of an argument}
\usage{
checkClass(x, classes, ordered = FALSE, null.ok = FALSE)

check_class(x, classes, ordered = FALSE, null.ok = FALSE)

assertClass(
  x,
  classes,
  ordered = FALSE,
  null.ok = FALSE,
  .var.name = vname(x),
  add = NULL
)

assert_class(
  x,
  classes,
  ordered = FALSE,
  null.ok = FALSE,
  .var.name = vname(x),
  add = NULL
)

testClass(x, classes, ordered = FALSE, null.ok = FALSE)

test_class(x, classes, ordered = FALSE, null.ok = FALSE)

expect_class(
  x,
  classes,
  ordered = FALSE,
  null.ok = FALSE,
  info = NULL,
  label = vname(x)
)
}
\arguments{
\item{x}{[any]\cr
Object to check.}

\item{classes}{[\code{character}]\cr
Class names to check for inheritance with \code{\link[base]{inherits}}.
\code{x} must inherit from all specified classes.}

\item{ordered}{[\code{logical(1)}]\cr
Expect \code{x} to be specialized in provided order.
Default is \code{FALSE}.}

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
 \code{assertClass}/\code{assert_class} return 
 \code{x} invisibly, whereas
 \code{checkClass}/\code{check_class} and 
 \code{testClass}/\code{test_class} return 
 \code{TRUE}.
 If the check is not successful, 
 \code{assertClass}/\code{assert_class}
 throws an error message, 
 \code{testClass}/\code{test_class}
 returns \code{FALSE},
 and \code{checkClass}/\code{check_class} 
 return a string with the error message.
 The function \code{expect_class} always returns an
 \code{\link[testthat]{expectation}}.
}
\description{
Check the class membership of an argument
}
\examples{
# Create an object with classes "foo" and "bar"
x = 1
class(x) = c("foo", "bar")

# is x of class "foo"?
testClass(x, "foo")

# is x of class "foo" and "bar"?
testClass(x, c("foo", "bar"))

# is x of class "foo" or "bar"?
\dontrun{
assert(
  checkClass(x, "foo"),
  checkClass(x, "bar")
)
}
# is x most specialized as "bar"?
testClass(x, "bar", ordered = TRUE)
}
\seealso{
Other attributes: 
\code{\link{checkMultiClass}()},
\code{\link{checkNamed}()},
\code{\link{checkNames}()}

Other classes: 
\code{\link{checkMultiClass}()},
\code{\link{checkR6}()}
}
\concept{attributes}
\concept{classes}
