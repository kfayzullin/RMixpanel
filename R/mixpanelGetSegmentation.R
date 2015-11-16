mixpanelGetSegmentation <- function(
  account,
  event,                   # Must be included. E.g. '[Action] Video content view started'.
  from,
  to=from,
  unit="day",              # 
  type="unique",           # This can be "general", "unique", or "average".
  on='properties["$os"]',  # Array of up to 2 segmentation properties. An empty array returns un-segmented counts.
  ...                      # Additional arguments to Mixpanel API.
) {
  args = list(...)
  args$event = event
  args$from_date = createDateSequence(from)
  args$to_date = createDateSequence(to)
  args$unit = unit
  args$type = type
  
  if (length(on) > 2)
    stop("Up to 2 segmentation variables are handled by API.")
  
  if (length(on) == 2) {
    args$inner = on[1]
    args$outer = on[2]
    data = mixpanelGetData(account, "segmentation/multiseg", args, data=TRUE)
    data = jsonlite::fromJSON(data)$data
    
    outerNames = names(data$values)
    innerNames = names(data$values[[1]])
    timeNames = names(data$values[[1]][[1]])
    
    kOuter = length(outerNames)
    kInner = length(innerNames)
    kTimes = length(timeNames)

    data = array(unlist(data$values), c(kTimes, kInner, kOuter), dimnames=list(timeNames, innerNames, outerNames))
    data[order(timeNames), , , drop=FALSE]
    
  } else {
    args$on = on
    
    data = mixpanelGetData(account, "segmentation/", args, data=TRUE)
    data = jsonlite::fromJSON(data)$data
    
    labels = names(data$values[[1]])
    n = length(labels)
    k = length(data[[2]])
    groups = names(data[[2]])
    
    data = matrix(unlist(data[[2]]), n, k, byrow=FALSE, dimnames=list(labels, groups))
    data[order(labels), , drop=FALSE]
  }
}
