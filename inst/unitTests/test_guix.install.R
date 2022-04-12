test.guix.install <- function () {
    checkException(guix.install("foo", guix="/doesnot/exist", cacheFile=tempfile()))

    msg <- geterrmessage()
    checkTrue(grepl("Failed to run Guix command", msg))
}
