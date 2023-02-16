# RTNRS

The TNRS R package (also known as RTNRS) provides access to the Taxonomic Name Resolution Service API, which  is a tool for automated standardization of plant scientific names. The TNRS corrects spelling errors and alternative spellings to a standard list of names, and converts out of date names (synonyms) to the current accepted name. More information on the TNRS is available on the BIEN website (https://bien.nceas.ucsb.edu/bien/tools/tnrs/), and the code underlying the TNRS is available on Github at https://github.com/ojalaquellueva/tnrsapi.

# Important Note

Before submitting names to the TNRS, we strongly recommend that you exclude any names which are all whitespace, NULL, NA, or empty strings.  These "blank" names may cause the submitted names to become associated with incorrect IDs.  We are working on fixing this bug, but in the meantime we recommend that you omit such names from your queries.


# Warning Messages and Errors

* **"This function requires internet access, please check your connection."** The TNRS package checks the internet connection before attempting a query.  It does so by attempting to contact www.google.com.  If this connection fails, it assumes there is no internet connection and issues this warning.
* **"There appears to be a problem reaching the API."** This message is shown if an error is thrown when using the POST() in the httr package to connect to the API.  This is usually caused by temporary server outages (e.g. due to upgrades), but may also occur due to issues with curl, as noted here: https://github.com/EnquistLab/RTNRS/issues/7#issuecomment-1094680196
* **"Problem with the API: HTTP Status ..."** This message is returned when the API connection is successful, but the API returns a status message indicating that something weird happened.  The status code shown can be consulted to figure out what might have gone wrong.
* **"There seems to be a problem with the query, which returned the following: ..."** This message is shown if the API returns content that cannot be properly parsed.
