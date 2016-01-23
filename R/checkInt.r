#' Check if an argument is a single integerish value
#'
#' @templateVar fn Int
#' @template x
#' @template na-handling
#' @param na.ok [\code{logical(1)}]\cr
#'  Are missing values allowed? Default is \code{FALSE}.
#' @template bounds
#' @template tol
#' @template checker
#' @family scalars
#' @useDynLib checkmate c_check_int
#' @export
#' @examples
#' testInt(1)
#' testInt(-1, lower = 0)
checkInt = function(x, na.ok = FALSE, lower = -Inf, upper = Inf, tol = sqrt(.Machine$double.eps)) {
  .Call(c_check_int, x, na.ok, lower, upper, tol)
}

#' @export
#' @include makeAssertion.r
#' @template assert
#' @rdname checkInt
assertInt = makeAssertionFunction(checkInt)

#' @export
#' @rdname checkInt
assert_int = assertInt

#' @export
#' @include makeTest.r
#' @rdname checkInt
testInt = makeTestFunction(checkInt)

#' @export
#' @rdname checkInt
test_int = testInt

#' @export
#' @include makeExpectation.r
#' @template expect
#' @rdname checkInt
expect_int = makeExpectationFunction(checkInt)
