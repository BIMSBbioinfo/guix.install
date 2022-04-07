test.guix.install <- function () {
    checkException(guix.install("foo", guix="/doesnot/exist"))
}
