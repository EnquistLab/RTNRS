# RTNRS

The TNRS R package (also known as RTNRS) provides access to the Taxonomic Name Resolution Service API, which  is a tool for automated standardization of plant scientific names. The TNRS corrects spelling errors and alternative spellings to a standard list of names, and converts out of date names (synonyms) to the current accepted name. More information on the TNRS is available on the BIEN website (https://bien.nceas.ucsb.edu/bien/tools/tnrs/), and the code underlying the TNRS is available on Github at https://github.com/ojalaquellueva/tnrsapi.

# Working offline

Names can also be resolved without an internet connection, against a locally cached copy of the taxonomic sources, using the same algorithm as the web service:

```r
TNRS_local_build()   # one-off download, about 116 MB and a few minutes
results <- TNRS_local(c("Quercuss alba", "Xantium strumarium"))
TNRS_local_status()  # which sources and versions you resolved against
```

In one comparison on a single machine, resolving 1000 names took about 6 minutes through the web service and about 11 seconds locally once the data was loaded; for a handful of names the web service is simpler and can be quicker, since loading the local data costs around twenty seconds per source. Timings will vary with hardware and with how busy the service is. See `vignette("TNRS_offline")` for the full comparison.

`TNRS_local()` returns the same columns as `TNRS()`. It is a separate function rather than an option on `TNRS()` because the two do not always give the same answer: the local copy of the sources is usually newer than the one the web service is running, and the local version consults a single source by default rather than blending two. See `vignette("TNRS_offline")` for the differences and how to interpret them.

# Important Note

Before submitting names to the TNRS, we strongly recommend that you exclude any names which are all whitespace, NULL, NA, or empty strings.  These "blank" names may cause the submitted names to become associated with incorrect IDs.  We are working on fixing this bug, but in the meantime we recommend that you omit such names from your queries.


# Warning Messages and Errors

* **"This function requires internet access, please check your connection."** The TNRS package checks the internet connection before attempting a query.  It does so by attempting to contact www.google.com.  If this connection fails, it assumes there is no internet connection and issues this warning.
* **"There appears to be a problem reaching the API."** This message is shown if an error is thrown when using the POST() in the httr package to connect to the API.  This is usually caused by temporary server outages (e.g. due to upgrades), but may also occur due to issues with curl, as noted here: https://github.com/EnquistLab/RTNRS/issues/7#issuecomment-1094680196
* **"Problem with the API: HTTP Status ..."** This message is returned when the API connection is successful, but the API returns a status message indicating that something weird happened.  The status code shown can be consulted to figure out what might have gone wrong.
* **"There seems to be a problem with the query, which returned the following: ..."** This message is shown if the API returns content that cannot be properly parsed.

  [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.11186237.svg)](https://doi.org/10.5281/zenodo.11186237)

