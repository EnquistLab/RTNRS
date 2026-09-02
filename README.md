# RTNRS

The TNRS R package (also known as RTNRS) provides access to the Taxonomic Name Resolution Service API, which  is a tool for automated standardization of plant scientific names. The TNRS corrects spelling errors and alternative spellings to a standard list of names, and converts out of date names (synonyms) to the current accepted name. More information on the TNRS is available on the BIEN website (https://bien.nceas.ucsb.edu/bien/tools/tnrs/), and the code underlying the TNRS is available on Github at https://github.com/ojalaquellueva/tnrsapi.

# Working offline

**Beta.** The offline engine is a new port of the published TNRS algorithm. On
the 100 name benchmark it returns the same matched name as the web service for
all 100 and the same accepted name for 88; most of the differences are cases
where the local sources are newer than the service's, which was last built in
January 2024, or where WFO and WCVP disagree and the two follow different ones.
`vignette("TNRS_offline")` goes through them individually. It has not been
tested broadly across taxonomic groups and sources, nor independently reviewed,
so treat the results as provisional and check a sample against `TNRS()` for work
where the answer matters.

Names can also be resolved without an internet connection, against a locally cached copy of the taxonomic sources, using the same algorithm as the web service:

```r
TNRS_local_build()   # one-off download, about 116 MB and a few minutes
results <- TNRS_local(c("Quercuss alba", "Xantium strumarium"))
TNRS_local_status()  # what is built, what else you could build, and versions
```

In one comparison on a single machine, resolving 1000 names took about 6 minutes through the web service and about 11 seconds locally once the data was loaded; for a handful of names the web service is simpler and can be quicker, since loading the local data costs around twenty seconds per source. Timings will vary with hardware and with how busy the service is. See `vignette("TNRS_offline")` for the full comparison.

The offline engine is not limited to plants. `TNRS_local_build("mdd")` adds the
Mammal Diversity Database, and `TNRS_local_build("col")` the Catalogue of Life,
which covers all life. Sources record which nomenclatural code they follow, so
zoological names are read correctly, and asking for a plant and an animal source
together makes a name shared by both codes visible rather than silently resolved
under one of them.

You can also resolve against a checklist of your own, with `TNRS_local_add_source()`. That covers taxonomic authorities the TNRS does not distribute, groups outside the flowering plants, and in-house lists; only the name column is required, and a registered checklist can be blended with `wfo` and `wcvp` like any other source.

`TNRS_local()` returns the same columns as `TNRS()`. It is a separate function rather than an option on `TNRS()` because the two do not always give the same answer: the local copy of the sources is usually newer than the one the web service is running, and the local version consults a single source by default rather than blending two. See `vignette("TNRS_offline")` for the differences and how to interpret them.

# Blank names

Names that are missing, empty, or only whitespace used to cause the names after
them to be returned against the wrong IDs, so the advice was to remove them
before querying. That is fixed as of 0.4.0: blank names are held back from the
request and returned unmatched, so the IDs still line up and you no longer need
to filter them out yourself. Repeated names are also returned once per row
rather than sharing a single row with their IDs pasted together.


# When something goes wrong

If a query fails and it is not clear whether the problem is at your end or the
server's, ask:

```r
TNRS_status()
#> TNRS server status
#>   Internet  : connected
#>   Server    : https://tnrsapi.xyz/tnrs_api.php
#>   Reachable : yes, HTTP 200 in 0.3 seconds
#>   Version   : app 5.3.1, database 4.4.1, built 2024-01-17
```

It reports separately on your connection and on the server, so the two cannot be
confused, and returns the same information invisibly for use in a script.

Messages you may see:

* **"This function requires internet access, please check your connection."** The connection is checked before a query is attempted, by contacting www.google.com. Note this says nothing about the TNRS server itself, which may be down while your connection is fine; `TNRS_status()` is what distinguishes them.
* **A request that fails now names its cause** rather than reporting every failure the same way. A timeout says how long it waited and how to wait longer; a name that will not resolve, a refused connection and a certificate problem are each identified. Certificate problems are usually local, and are discussed in https://github.com/EnquistLab/RTNRS/issues/7#issuecomment-1094680196
* **"Problem with the API: HTTP Status ..."** The connection succeeded but the server reported a problem. The status code shown can be consulted to figure out what might have gone wrong.
* **"There seems to be a problem with the query, which returned the following: ..."** The server returned content that could not be parsed.

A large batch legitimately takes minutes. If it times out, wait longer rather
than giving up:

```r
results <- TNRS(my_names, timeout = 900)
```

Or resolve the names offline, which needs no server at all.

  [![R-CMD-check](https://github.com/EnquistLab/RTNRS/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/EnquistLab/RTNRS/actions/workflows/R-CMD-check.yaml)
  [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.11186237.svg)](https://doi.org/10.5281/zenodo.11186237)

