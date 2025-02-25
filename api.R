# api.R
library(plumber)
library(data.table)
library(jsonlite)

# Load the processed data
flights_dt <- fread("C:/Users/LENOVO/Pictures/MYPROJECTS/R-PROJECT/processed_flights.csv")

#* @apiTitle Flights API

#* Create a new flight entry
#* @post /flight
#* @description Add a new flight with JSON payload
#* @response 200 Returns flight_id on success
#* @response 400 Error if invalid data
#* @parser json
#* @serializer unboxedJSON
#* @requestBody {"description": "JSON payload with flight details", "content": {"application/json": {"schema": {"type": "object", "properties": {"carrier": {"type": "string"}, "dest": {"type": "string"}, "dep_delay": {"type": "number"}}, "example": {"carrier": "XX", "dest": "XYZ", "dep_delay": 20}}}}}
function(req, res) {
  cat("req$postBody:", req$postBody, "\n")
  if (is.null(req$postBody) || nchar(req$postBody) == 0) {
    res$status <- 400
    return(list(error = "No flight data provided in request body"))
  }
  new_flight <- tryCatch(
    as.data.table(fromJSON(req$postBody)),
    error = function(e) {
      res$status <- 400
      return(list(error = paste("Invalid JSON:", e$message)))
    }
  )
  if (is.null(new_flight) || nrow(new_flight) == 0 || !("dep_delay" %in% names(new_flight))) {
    res$status <- 400
    return(list(error = "Invalid flight data, must include dep_delay"))
  }
  new_flight[, flight_id := max(flights_dt$flight_id, na.rm = TRUE) + 1]
  new_flight[, is_delayed := ifelse(dep_delay > 15, TRUE, FALSE)]
  new_flight[is.na(is_delayed), is_delayed := FALSE]
  
  flights_dt <<- rbind(flights_dt, new_flight, fill = TRUE)
  list(message = "Flight added", flight_id = new_flight$flight_id)
}

#* Get flight details by ID
#* @param id Flight ID
#* @get /flight/<id:int>
#* @serializer unboxedJSON
function(id) {
  flight <- flights_dt[flight_id == id]
  if (nrow(flight) == 0) {
    return(list(error = "Flight not found"))
  }
  as.list(flight)
}

#* Check if a flight is delayed
#* @param id Flight ID
#* @get /check-delay/<id:int>
#* @serializer unboxedJSON
function(id) {
  flight <- flights_dt[flight_id == id, .(is_delayed)]
  if (nrow(flight) == 0) {
    return(list(error = "Flight not found"))
  }
  list(delayed = flight$is_delayed)
}

#* Get average departure delay for an airline
#* @param id Airline carrier code (optional)
#* @get /avg-dep-delay
#* @serializer unboxedJSON
function(id = NULL) {
  if (is.null(id)) {
    return(as.list(flights_dt[, .(avg_dep_delay = mean(dep_delay, na.rm = TRUE)), by = carrier]))
  }
  delay <- flights_dt[carrier == id, .(avg_dep_delay = mean(dep_delay, na.rm = TRUE))]
  if (nrow(delay) == 0) {
    return(list(error = "Airline not found"))
  }
  list(carrier = id, avg_dep_delay = delay$avg_dep_delay)
}

#* Get top N destinations
#* @param n Number of destinations
#* @get /top-destinations/<n:int>
#* @serializer unboxedJSON
function(n) {
  top_n <- flights_dt[, .N, by = dest][order(-N)][1:n]
  as.list(top_n)
}

#* Update flight details by ID
#* @put /flights/<id:int>
#* @description Update an existing flight with JSON payload
#* @response 200 Returns success message
#* @response 400 Error if invalid data
#* @response 404 Flight not found
#* @parser json
#* @serializer unboxedJSON
#* @requestBody {"description": "JSON payload with updated flight details", "content": {"application/json": {"schema": {"type": "object", "properties": {"carrier": {"type": "string"}, "dest": {"type": "string"}, "dep_delay": {"type": "number"}}, "example": {"carrier": "YY", "dest": "ABC", "dep_delay": 10}}}}}
function(id, req, res) {
  cat("req$postBody:", req$postBody, "\n")
  if (nrow(flights_dt[flight_id == id]) == 0) {
    res$status <- 404
    return(list(error = "Flight not found"))
  }
  if (is.null(req$postBody) || nchar(req$postBody) == 0) {
    res$status <- 400
    return(list(error = "No flight data provided in request body"))
  }
  updated_flight <- tryCatch(
    as.data.table(fromJSON(req$postBody)),
    error = function(e) {
      res$status <- 400
      return(list(error = paste("Invalid JSON:", e$message)))
    }
  )
  if (is.null(updated_flight) || nrow(updated_flight) == 0 || !("dep_delay" %in% names(updated_flight))) {
    res$status <- 400
    return(list(error = "Invalid flight data, must include dep_delay"))
  }
  updated_flight[, flight_id := id]
  updated_flight[, is_delayed := ifelse(dep_delay > 15, TRUE, FALSE)]
  updated_flight[is.na(is_delayed), is_delayed := FALSE]
  
  flights_dt <<- flights_dt[flight_id != id]  # Remove old record
  flights_dt <<- rbind(flights_dt, updated_flight, fill = TRUE)
  list(message = "Flight updated", flight_id = id)
}

#* Delete flight by ID
#* @delete /flights/<id:int>
#* @description Delete an existing flight
#* @response 200 Returns success message
#* @response 404 Flight not found
#* @serializer unboxedJSON
function(id, res) {
  if (nrow(flights_dt[flight_id == id]) == 0) {
    res$status <- 404
    return(list(error = "Flight not found"))
  }
  flights_dt <<- flights_dt[flight_id != id]
  list(message = "Flight deleted", flight_id = id)
}