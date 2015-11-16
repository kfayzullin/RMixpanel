mixpanelGetEventsForUsers <- function(
  account,
  ## TODO: flatten the parameter structure.
  args=list(
    distinct_ids,    # Array of IDs.
    ## limit=1000,
    from_date="2015-01-01", ## Attention w/ month numbers starting w/ 0!!!
    to_date="2015-06-01"
    )
) {
  args$distinct_ids = arrayRtoJSON(args$distinct_ids)
  res = mixpanelGetData(account, "stream/query", args, data=TRUE)
  res = jsonlite::fromJSON(res)
  
  if ("status" %in% names(res) && res$status == "ok") {
    data = res$results$events
    if (length(data) > 0) {
      data = cbind(event=data[, 1], data[, 2], stringsAsFactors=FALSE)
      data
    
    } else {
      cat("Event list empty")
      data.frame()
    }
    
  } else {
    print(res)
    data.frame()
  }
}
