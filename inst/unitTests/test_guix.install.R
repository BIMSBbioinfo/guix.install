test.guix.install <- function () {
    checkException(guix.install("foo", guix="/doesnot/exist"))

    msg <- geterrmessage()
    checkTrue(grepl("Failed to run Guix command", msg))
}
