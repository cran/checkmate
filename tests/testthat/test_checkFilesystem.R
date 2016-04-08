context("checkFile")

td = tempfile("checkFile")
dir.create(td, recursive=TRUE)
fn = file.path(td, "myfile.ext")
dn = file.path(td, "dir")
ff = file.path(td, "xxx")
file.create(fn)
dir.create(dn)

test_that("check_file", {
  myobj = fn
  expect_succ_all(File, myobj)
  myobj = ff
  expect_fail_all(File, myobj)

  expect_false(testFile(character(0)))
  expect_false(testFile(NULL))
  expect_false(testFile(dn))

  expect_error(assertFile(character(0)), "provided")
  expect_error(assertFile(ff), "exist")
  expect_error(assertFile(dn))

  expect_succ_all(File, fn, extension = "ext")
  expect_succ_all(File, fn, extension = c("foo", "ext"))
  expect_fail_all(File, fn, extension = "foo")
})

test_that("check_directory", {
  myobj = dn
  expect_succ_all(Directory, myobj)
  myobj = ff
  expect_fail_all(Directory, myobj)

  expect_false(testDirectory(character(0)))
  expect_false(testDirectory(fn))

  expect_error(assertDirectory(character(0)), "provided")
  expect_error(assertDirectory(ff), "exist")
  expect_error(assertDirectory(fn))
})

test_that("check_access", {
  myobj = R.home()
  expect_succ_all(Access, myobj, "r")

  if (.Platform$OS.type != "windows") {
    Sys.chmod(fn, "0000")
    expect_true(testAccess(fn, ""))
    expect_false(testAccess(fn, "r"))
    expect_false(testAccess(fn, "w"))
    expect_false(testAccess(fn, "x"))
    Sys.chmod(fn, "0700")
    expect_true(testAccess(fn, ""))
    expect_true(testAccess(fn, "r"))
    expect_true(testAccess(fn, "w"))
    expect_true(testAccess(fn, "x"))
    Sys.chmod(fn, "0600")
    expect_true(testAccess(fn, ""))
    expect_true(testAccess(fn, "r"))
    expect_true(testAccess(fn, "rw"))
    expect_false(testAccess(fn, "rx"))
    expect_false(testAccess(fn, "wx"))

    expect_error(testAccess(fn, "a"))
    expect_error(testAccess(fn, "rrr"))
  }
})

test_that("check_path_for_output", {
  myobj = ff
  expect_succ_all(PathForOutput, myobj)
  myobj = fn
  expect_fail_all(PathForOutput, myobj)

  expect_false(testPathForOutput(character(0)))
  expect_false(testPathForOutput(NULL))

  expect_error(assertPathForOutput(character(0)), "path provided")
  expect_identical(assertPathForOutput(c("a", "b")), c("a", "b"))
  expect_identical(assertPathForOutput(ff), ff)
  expect_error(assertPathForOutput(fn), "exist")
  expect_identical(assertPathForOutput(fn, overwrite = TRUE), fn)
  expect_true(testPathForOutput(c(fn, ff, dn), overwrite = TRUE))
  expect_false(testPathForOutput(c(fn, ff, dn), overwrite = FALSE))
})
