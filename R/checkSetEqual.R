#' Check if an argument is equal to a given set
#'
#' @templateVar fn Subset
#' @template x
#' @param y [\code{atomic}]\cr
#'  Set to compare with.
#' @param ordered [\code{logical(1)}]\cr
#' Check \code{x} to have the same length and order as \code{y}, i.e.
#' check using \dQuote{==} while handling \code{NA}s nicely.
#' Default is \code{FALSE}.
#' @template fmatch
#' @template checker
#' @template set
#' @family set
#' @export
#' @examples
#' testSetEqual(c("a", "b"), c("a", "b"))
#' testSetEqual(1:3, 1:4)
#'
#' # x is not converted before the comparison (except for numerics)
#' testSetEqual(factor("a"), "a")
#' testSetEqual(1, "1")
#' testSetEqual(1, as.integer(1))
checkSetEqual = function(x, y, ordered = FALSE, fmatch = FALSE) {
  qassert(x, "a")
  qassert(y, "a")
  qassert(ordered, "B1")

  if (ordered) {
    if (!isSameType(x, y) || length(x) != length(y) || any(xor(is.na(x), is.na(y)) | x != y, na.rm = TRUE))
      return(sprintf("Must be equal to %s, but is %s", array_collapse(y), array_collapse(x)))
    return(TRUE)
  }

  if (isTRUE(fmatch) && requireNamespace("fastmatch", quietly = TRUE))
    match = fastmatch::fmatch

  check_set_equal_internal(x, y, match)
}

#' @export
#' @rdname checkSetEqual
check_set_equal = checkSetEqual

#' @export
#' @include makeAssertion.R
#' @template assert
#' @rdname checkSetEqual
assertSetEqual = makeAssertionFunction(checkSetEqual, use.namespace = FALSE)

#' @export
#' @rdname checkSetEqual
assert_set_equal = assertSetEqual

#' @export
#' @include makeTest.R
#' @rdname checkSetEqual
testSetEqual = makeTestFunction(checkSetEqual)

#' @export
#' @rdname checkSetEqual
test_set_equal = testSetEqual

#' @export
#' @include makeExpectation.R
#' @template expect
#' @rdname checkSetEqual
expect_set_equal = makeExpectationFunction(checkSetEqual, use.namespace = FALSE)
