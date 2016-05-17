# Creates a waiter function that can be used in a loop while trying some task many times.
# The waiter sleeps while waiting to try again, with sleep times determined by exponential back-off.
#
# timeout: How long (in seconds) to keep trying before timing out (NULL means no timeout)
#         delay Initial delay between tries (in seconds)
# maxDelay: Maximim delay (in seconds) between tries
#
# Returns function which gets the waiter status. This function returns a list with these
# items:
#  index: How many times we have waited
#  secondsWaited: How long (in seconds) since we started the timer
#  stillTrying: Whether we should keep trying or give up (logical)

StartRetryWaiter <- function(timeout = NULL, delay = .1, maxdelay = 1.0) {
  if (is.null(timeout)) {
    timeout <- Inf
  }
  index <- 0
  secondsWaited <- 0
  startTime <- Sys.time()
  delay <- delay / 2.0
  GetWaitStatus <- function() {
    remaining <- timeout - secondsWaited
    if (index > 0 && remaining > 0) { #  Don't sleep on first try
      delay <<- min(delay * 2, maxdelay, remaining)
      stillTrying <- TRUE
      Sys.sleep(delay)
    }
    index <<- index + 1
    secondsWaited <<- as.numeric(Sys.time() - startTime, units = "secs")
    stillTrying <- secondsWaited < timeout
    return(list(index = index, secondsWaited = secondsWaited, stillTrying = stillTrying))
  }
  return(GetWaitStatus)
}
