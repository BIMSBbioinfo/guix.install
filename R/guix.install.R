## Copyright (C) 2020-2022 Ricardo Wurmus
##
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <http://www.gnu.org/licenses/>.

## TODO:
## - allow installation of more than one package

guix.install <- function (package, profile = NULL, guix = "guix", archive = NULL)
{
    if (is.null (profile)) {
        ## Use the default profile unless otherwise specified.
        guix_profile <- Sys.getenv ("GUIX_PROFILE", unset = NA)
        if (is.na (guix_profile)) {
            profile <- paste (Sys.getenv ("HOME"), ".guix-profile", sep = "/")
        } else {
            profile <- guix_profile
        }
    } else {
        ## Create the parent directory if necessary.
        parent <- dirname (profile)
        if (! dir.exists (parent)) {
            dir.create (parent, recursive = TRUE)
        }
    }

    ## Location of on-the-fly generated packages
    scratch <- paste (Sys.getenv ("HOME"), ".Rguix", "packages.scm", sep = "/")

    ## split package path, put scratch location first
    package_path <- NULL
    old_package_path <- Sys.getenv ("GUIX_PACKAGE_PATH")
    entries <- strsplit (old_package_path, ":")[[1]]
    package_path <- paste (unique (c(dirname (scratch), entries)), sep = ":")
    Sys.setenv (GUIX_PACKAGE_PATH=package_path)

    is_url <- length (grep ("^https?://", package)) > 0

    if (!is_url) {
        ## The normalized name used by Guix packages
        guix_name <- paste0 ("r-", gsub ("[^a-z0-9]", "-", tolower (package)))

        ## Does the package already exist?
        error <- system2 (guix, c("show", guix_name),
                          stdout = NULL, stderr = NULL)
    }
    
    ## Attempt to import the package
    if (is_url || (error > 0)) {
        ## Build a scratch module
        if (! dir.exists (dirname (scratch))) {
            dir.create (dirname (scratch), recursive = TRUE)
        }
        if (! file.exists (scratch)) {
            cat ("
(define-module (packages)
  #:use-module (gnu)
  #:use-module (guix)
  #:use-module (guix git-download)
  #:use-module (guix hg-download)
  #:use-module (guix build-system r)
  #:use-module (guix licenses))
", file = scratch)
        }

        if (is.null (archive)) {
            archive <- if (is_url) {
                           "git" # TODO: what about hg?
                       } else {
                           ## The importer will retry importing from CRAN if a
                           ## package is not found on Bioconductor.
                           "bioconductor"
                       }
        }

        definitions <- suppressWarnings (system2 (guix, c("import", "cran",
                                                          "--recursive",
                                                          "--style=specification",
                                                          paste ("--archive", archive, sep = "="),
                                                          package),
                                                  stdout = TRUE))

        ## Abort on error
        status <- attr (definitions, "status")
        if (!is.null (status) && (status > 0)) {
            stop (paste("Failed to import", package))
        }

        ## Get guix_name from definitions
        pattern <- "\\(name \"([^\"]+)\"\\)"
        name <- grep (pattern, definitions, value = TRUE)
        guix_name <- sub (pattern, "\\1", name)

        ## Store generated package definitions.
        cat (";; Imported from within R at ", date(), "\n",
             file = scratch, append = TRUE)
        cat (definitions, sep = "\n",
             file = scratch, append = TRUE)
    }

    ## Install the package.
    error <- system2 (guix, c("package", paste ("--profile", profile, sep = "="),
                              "--install", guix_name))

    ## Extend the R load path.
    if (error == 0) {
        .libPaths (paste (profile, "site-library", sep = "/"))
    }
}
