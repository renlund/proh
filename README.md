[![Travis-CI Build Status](https://travis-ci.org/renlund/proh.svg?branch=master)](https://travis-ci.org/renlund/proh)

proh
====

Tools for working with projects set up according to the arbitrary preferences
of the author and not of interest to anyone else. Builds on the R package `knitr`.

New projects (meaning statistical reports) are set up with `new_project` which
creates some standard folders and files. Some functions work (e.g. `fetch`,
`keep`, `send`, etc.) will only work within this folder structure.

There are perhaps some inconsistencies, but the need for backwards compatability
makes me not adress these.
