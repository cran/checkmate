% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/checkFileExists.R
\name{checkFileExists}
\alias{checkFileExists}
\alias{check_file_exists}
\alias{assertFileExists}
\alias{assert_file_exists}
\alias{testFileExists}
\alias{test_file_exists}
\alias{expect_file_exists}
\alias{checkFile}
\alias{assertFile}
\alias{assert_file}
\alias{testFile}
\alias{expect_file}
\title{Check existence and access rights of files}
\usage{
checkFileExists(x, access = "", extension = NULL)

check_file_exists(x, access = "", extension = NULL)

assertFileExists(
  x,
  access = "",
  extension = NULL,
  .var.name = vname(x),
  add = NULL
)

assert_file_exists(
  x,
  access = "",
  extension = NULL,
  .var.name = vname(x),
  add = NULL
)

testFileExists(x, access = "", extension = NULL)

test_file_exists(x, access = "", extension = NULL)

expect_file_exists(
  x,
  access = "",
  extension = NULL,
  info = NULL,
  label = vname(x)
)

checkFile(x, access = "", extension = NULL)

assertFile(x, access = "", extension = NULL, .var.name = vname(x), add = NULL)

assert_file(x, access = "", extension = NULL, .var.name = vname(x), add = NULL)

testFile(x, access = "", extension = NULL)

expect_file(x, access = "", extension = NULL, info = NULL, label = vname(x))
}
\arguments{
\item{x}{[any]\cr
Object to check.}

\item{access}{[\code{character(1)}]\cr
Single string containing possible characters \sQuote{r}, \sQuote{w} and \sQuote{x} to
force a check for read, write or execute access rights, respectively.
Write and executable rights are not checked on Windows.}

\item{extension}{[\code{character}]\cr
Vector of allowed file extensions, matched case insensitive.}

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
 \code{assertFileExists}/\code{assert_file_exists} return 
 \code{x} invisibly, whereas
 \code{checkFileExists}/\code{check_file_exists} and 
 \code{testFileExists}/\code{test_file_exists} return 
 \code{TRUE}.
 If the check is not successful, 
 \code{assertFileExists}/\code{assert_file_exists}
 throws an error message, 
 \code{testFileExists}/\code{test_file_exists}
 returns \code{FALSE},
 and \code{checkFileExists}/\code{check_file_exists} 
 return a string with the error message.
 The function \code{expect_file_exists} always returns an
 \code{\link[testthat]{expectation}}.
}
\description{
Check existence and access rights of files
}
\note{
The functions without the suffix \dQuote{exists} are deprecated and will be removed
from the package in a future version due to name clashes.
\code{test_file} has been unexported already.
}
\examples{
# Check if R's COPYING file is readable
testFileExists(file.path(R.home(), "COPYING"), access = "r")

# Check if R's COPYING file is readable and writable
testFileExists(file.path(R.home(), "COPYING"), access = "rw")
}
\seealso{
Other filesystem: 
\code{\link{checkAccess}()},
\code{\link{checkDirectoryExists}()},
\code{\link{checkPathForOutput}()}
}
\concept{filesystem}
