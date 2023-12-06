find_range <- function(arr, target) {
  left <- 1
  right <- length(arr)
  
  while (left <= right) {
    mid <- floor((left + right) / 2)
    
    if (arr[mid] == target) {
      return(c(mid, mid))  # Number found, return a range with just the number itself
    } else if (arr[mid] < target) {
      right <- mid - 1  # Adjust the right boundary for a decreasing array
    } else {
      left <- mid + 1
    }
  }
  
  # At this point, left and right have crossed, indicating the range where the number is situated
  if (right >= 1 && left <= length(arr)) {
    return(c(right, left))  # Return the range
  } else if (right >= 1) {
    return(c(right, NA))  # The number is larger than the largest element in the array
  } else if (left <= length(arr)) {
    return(c(NA, left))  # The number is smaller than the smallest element in the array
  } else {
    return(NA)  # The number is out of the array's range
  }
}