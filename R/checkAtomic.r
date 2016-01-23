#' Check that an argument is an atomic vector
#'
#' @description
#' For the definition of \dQuote{atomic}, see \code{\link[base]{is.atomic}}.
#'
#' @templateVar fn Atmoic
#' @template x
#' @inheritParams checkVector
#' @template checker
#' @family basetypes
#' @useDynLib checkmate c_check_atomic
#' @export
#' @family basetypes
#' @family atomicvector
#' @examples
#' testAtomic(letters, min.len = 1L, any.missing = FALSE)
checkAtomic = function(x, any.missing = TRUE, all.missing = TRUE, len = NULL, min.len = NULL, max.len = NULL, unique = FALSE, names = NULL) {
  .Call(c_check_atomic, x, any.missing, all.missing, len, min.len, max.len, unique, names)
}


#' @export
#' @include makeAssertion.r
#' @template assert
#' @rdname checkAtomic
assertAtomic = makeAssertionFunction(checkAtomic)

#' @export
#' @rdname checkAtomic
assert_atomic = assertAtomic

#' @export
#' @include makeTest.r
#' @rdname checkAtomic
testAtomic = makeTestFunction(checkAtomic)

#' @export
#' @rdname checkAtomic
test_atomic = testAtomic

#' @export
#' @include makeExpectation.r
#' @template expect
#' @rdname checkAtomic
expect_atomic = makeExpectationFunction(checkAtomic)
