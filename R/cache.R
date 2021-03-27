#' Cache for metagenomicsClientR
#'
#' @return A BiocFileCache class object
#' @importFrom rappdirs user_cache_dir
#' @importFrom BiocFileCache BiocFileCache
#' @export
#'
#' @examples
metagenomics_cache <- function() {
    cache <- rappdirs::user_cache_dir(appname="metagenomicsClientR")
    metagenomics_cache <- BiocFileCache::BiocFileCache(cache)
    return(metagenomics_cache)
}

#' Add files to metagenomicsClientR
#'
#' This function wraps bcadd from BiocFileCache
#'
#' @param rname A character vector indicating the resource name (file).
#' @param url A character vector indicating the url of the resource
#'
#' @return Character vector of legnth 1, indicating resource name and path
#' @importFrom BiocFileCache bfcadd
#' @importFrom rappdirs user_cache_dir
#' @importFrom BiocFileCache bfcquery
#' @importFrom BiocFileCache bfcdownload
#' @importFrom BiocFileCache bfcneedsupdate
#' @export
#'
#' @examples
#' 
#' 
#' 
download_file <- function(rname, url) {
    
    cache_path <-  rappdirs::user_cache_dir(appname="metagenomicsClientR")
    if (!isTRUE(dir.exists(cache_path))) {
        stop(paste("A cache for metagenomicsClientR does not exit.",
        "You can create a cache with the metagenomics_cache() function."))
    }
    
    cache <- metagenomics_cache()
    rid <- BiocFileCache::bfcquery(cache, query = rname,
                                   field = "rname", exact = TRUE)$rid
    if (!length(rid)) {
        message(paste("Downloading", rname, "to cache..."))
        added_resource <- BiocFileCache::bfcadd(x = cache, 
                                                rname = rname,
                                                fpath = url)
        return(added_resource)
    } 
    
    if (!isFALSE(BiocFileCache::bfcneedsupdate(cache, rid))) {
        message(paste0("A file named as \"", rname, 
                      "\" is arlready present in the cache, ",
                      "but it needs to be updated. ",
                      "Check bfcneedsupdate(cache, \"",rid,"\").",
                      " You can overwrite the file with",
                      " bfcdownload(cache, \"", rid, "\")."))
        # updated_resource <- bfcdownload(cache, rid)
        # return(updated_resource)
        existing_resource <- cache[[rid]]
        return(existing_resource)
    }
    
    message(
        paste0("A file named as \"", rname, 
              "\" is already present and up to date in",
              " cache with the rid \"", rid,".\"")
    )
    
    existing_resource <- cache[[rid]]
    return(existing_resource)
}
