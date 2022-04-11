\name{guix.install-package}
\alias{guix.install-package}
\encoding{UTF-8}
\docType{package}
\title{guix.install: Install R Packages Reproducibly with Guix}
\description{
\packageDescription{guix.install}
}
\details{

\packageIndices{guix.install}

This package lets you install any R package through Guix from within a
running R session.  If a desired R package is not yet available in any
of your Guix channels, this package will leverage the recursive package
importing facilities that Guix provides and transparently import
packages from CRAN, Bioconductor, or any Mercurial or Git repository
before installing them with Guix.

This replaces the need for installers like devtools and exposes
reproducible package management features through a familiar interface
within R.
}
\author{
\packageAuthor{guix.install}

Maintainer: \packageMaintainer{guix.install}
}
\references{
Courtès L., Wurmus R. (2015) Reproducible and User-Controlled Software Environments in HPC with Guix. In: Hunold S. et al. (eds) Euro-Par 2015: Parallel Processing Workshops. Euro-Par 2015. Lecture Notes in Computer Science, vol 9523. Springer, Cham. \doi{10.1007/978-3-319-27308-2_47}
}
\keyword{ package }
\keyword{ utilities }
\examples{
\dontrun{
guix.install("methylKit", cacheFile=tempfile())
guix.install("methylKit", profile="/home/foo/custom/.guix-profile", cacheFile=tempfile())
guix.install("methylKit", guix="/gnu/remote/bin/guix", cacheFile=tempfile())
}
}
