\name{guix.install}
\alias{guix.install}
\encoding{UTF-8}
\title{guix.install}
\description{
  This procedure installs a given package with Guix.  If the package is
  available in any enabled Guix channel, the package and its
  dependencies will be installed from there.

  If the package is not available in Guix the procedure attempts to
  generate a package definition by recursively importing metadata from
  CRAN, Bioconductor, or a git/mercurial repository.  Upon successful
  generation of package definitions in \code{~/.Rguix/packages.scm} the
  package is installed with Guix.

  The file \code{~/.Rguix/packages.scm} is only appended to for
  reproducibility reasons.  Each entry has a comment with a timestamp
  for convenience.  The package definitions in that file could be
  upstreamed to a Guix channel to make it easier for others to reproduce
  the environment.  Consider sending package definitions to either the
  \href{https://guix.gnu.org/en/contribute/}{Guix project} or the
  \href{https://github.com/guix-science/guix-science}{Guix Science project}.
}
\usage{
guix.install(package, profile = NULL, guix = "guix", archive = NULL,
  cacheFile = NULL, stdout = "", stderr = "")
}
\arguments{
  \item{package}{
    The name of the R package to be installed as used on CRAN or
    Bioconductor, or a HTTP URL of a git repository holding an R
    package.
  }
  \item{profile}{
    The target Guix profile where the package should be installed.  By
    default this is the default Guix profile at
    \code{$HOME/.guix-profile}.
  }
  \item{guix}{
    The absolute file name of the \code{guix} executable.  By default
    this is just \code{guix} and will be searched on the user's \code{PATH}. 
  }
  \item{archive}{
    This argument is used to override the Guix importer's \code{archive}
    option, e.g. to explicitly specify that the package should be imported
    from Bioconductor or from a Mercurial repository.
  }
  \item{cacheFile}{
    The absolute file name of the file where the imported package
    definitions should be recorded.  When \code{NULL} this is \code{$HOME/.Rguix/packages.scm}.
  }
  \item{stdout}{
    Use this argument to redirect or silence Guix output printed to the
    standard output stream.  The default value is \code{""},
    causing all Guix messages to be printed to the console.
  }
  \item{stderr}{
    Use this argument to redirect or silence Guix output printed to
    the standard error stream.  The default value is \code{""},
    causing all Guix errors to be printed to the console.
  }
}
\value{
  This procedure is run for its side-effects.
}
\author{
  Ricardo Wurmus, \email{ricardo.wurmus@mdc-berlin.de}
}

\examples{
\dontrun{
guix.install("methylKit", cacheFile=tempfile())
guix.install("methylKit", profile="/home/foo/custom/.guix-profile", cacheFile=tempfile())
guix.install("methylKit", guix="/gnu/remote/bin/guix", cacheFile=tempfile())
}
}

\keyword{ utilities }
\concept{ package management }
\concept{ reproducibility }
