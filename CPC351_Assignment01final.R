
rm(list = ls())

# QUESTION 1: EXAM TIMETABLING

# Q1(a) READ AND CLEAN THE SIX COURSE FILES
course_files <- c(
  "CCS592", "CDS501", "CDS506",
  "CDS512", "CDS521", "CDS523"
)
courses <- list()
for (course in course_files) {
  file_name <- paste0("q1/Q1_", course, ".txt")
  students <- readLines(file_name)
  students <- toupper(trimws(students))
  students <- students[students != ""]
  students <- sub("^NEMA", "NAME", students)
  numbers <- sub("^NAME[-_ ]*", "", students)
  students <- paste0("NAME-", sprintf("%03d", as.numeric(numbers)))
  courses[[course]] <- unique(students)
}
student_count <- sapply(courses, length)
cat("\n===== Q1(a): CLEANED STUDENT COUNTS =====\n")
print(student_count)
cat("Total different students:",
    length(unique(unlist(courses))), "\n")

# Q1(b) CREATE THE CONFLICT MATRIX
number_courses <- length(courses)
conflict_matrix <- matrix(
  0,
  nrow = number_courses,
  ncol = number_courses
)
rownames(conflict_matrix) <- course_files
colnames(conflict_matrix) <- course_files
for (i in 1:number_courses) {
  for (j in 1:number_courses) {
    common_students <- intersect(courses[[i]], courses[[j]])
    conflict_matrix[i, j] <- length(common_students)
  }
}
diag(conflict_matrix) <- 0
cat("\n===== Q1(b): CONFLICT MATRIX =====\n")
print(conflict_matrix)
write.csv(conflict_matrix, "q1/conflict_matrix.csv")

# Q1(c) AND Q1(d) CREATE THE EXAM TIMETABLE
venue_capacity <- c(40, 70)
conflict_count <- rowSums(conflict_matrix > 0)
course_order <- order(conflict_count, decreasing = TRUE)
# Set no_back_to_back to TRUE for Question 1(d).
create_timetable <- function(no_back_to_back = FALSE) {
  assigned_slots <- rep(NA, number_courses)
  names(assigned_slots) <- course_files
  for (course_number in course_order) {
    placed <- FALSE
    for (period in 1:12) {
      for (venue in 1:2) {
        if (student_count[course_number] > venue_capacity[venue]) {
          next
        }
        slot <- (period - 1) * 2 + venue
        if (slot %in% assigned_slots) {
          next
        }
        valid_slot <- TRUE
        for (other in 1:number_courses) {
          if (is.na(assigned_slots[other])) {
            next
          }
          if (conflict_matrix[course_number, other] > 0) {
            other_period <- ceiling(assigned_slots[other] / 2)
            if (period == other_period) {
              valid_slot <- FALSE
            }
            if (no_back_to_back == TRUE) {
              same_day <- ceiling(period / 2) ==
                          ceiling(other_period / 2)
              next_to_each_other <- abs(period - other_period) == 1
              if (same_day && next_to_each_other) {
                valid_slot <- FALSE
              }
            }
          }
        }
        if (valid_slot == TRUE) {
          assigned_slots[course_number] <- slot
          placed <- TRUE
          break
        }
      }
      if (placed == TRUE) {
        break
      }
    }
    if (placed == FALSE) {
      stop("A course could not be scheduled.")
    }
  }
  period <- ceiling(assigned_slots / 2)
  venue_number <- ifelse(assigned_slots %% 2 == 1, 1, 2)
  timetable <- data.frame(
    Course = course_files,
    Students = as.numeric(student_count),
    Slot = as.numeric(assigned_slots),
    Period = as.numeric(period),
    Venue = paste("Venue", venue_number),
    Day = paste("Day", ceiling(period / 2)),
    Session = ifelse(period %% 2 == 1, "Morning", "Afternoon")
  )
  timetable <- timetable[order(timetable$Slot), ]
  return(timetable)
}
timetable_c <- create_timetable(FALSE)
cat("\n===== Q1(c): EXAM TIMETABLE =====\n")
print(timetable_c, row.names = FALSE)
cat("Periods required:",
    max(timetable_c$Period), "\n")
write.csv(timetable_c, "q1/timetable_Q1c.csv", row.names = FALSE)
timetable_d <- create_timetable(TRUE)
cat("\n===== Q1(d): TIMETABLE WITHOUT BACK-TO-BACK EXAMS =====\n")
print(timetable_d, row.names = FALSE)
cat("Periods required:",
    max(timetable_d$Period), "\n")
write.csv(timetable_d, "q1/timetable_Q1d.csv", row.names = FALSE)

# QUESTION 2: DELIVERY ROUTE OPTIMISATION
points <- data.frame(
  Point = paste0("P", 1:10),
  X = c(60, 180, 80, 140, 20, 100, 200, 140, 40, 100),
  Y = c(200, 200, 180, 180, 160, 160, 160, 140, 120, 120)
)

# Q2(a) READ AND CLEAN THE TRAVEL DATA
travel_data <- read.csv(
  "q2/ahmad_travel_logs.csv",
  stringsAsFactors = FALSE
)
travel_data$Weather <- toupper(trimws(travel_data$Weather))
travel_data$Time_of_Day <- toupper(trimws(travel_data$Time_of_Day))
rainy_morning <- travel_data[
  travel_data$Weather == "RAINY" &
  travel_data$Time_of_Day == "MORNING",
]
rainy_morning$Travel_Time_Minutes <-
  as.numeric(rainy_morning$Travel_Time_Minutes)
clean_data <- rainy_morning[
  !is.na(rainy_morning$Travel_Time_Minutes) &
  rainy_morning$Travel_Time_Minutes >= 0,
]
cat("\n===== Q2(a): DATA CLEANING =====\n")
cat("Original records:", nrow(travel_data), "\n")
cat("Rainy Morning records:", nrow(rainy_morning), "\n")
cat("Valid records:", nrow(clean_data), "\n")
cat("Invalid records removed:",
    nrow(rainy_morning) - nrow(clean_data), "\n")

# Q2(a) CREATE THE 10 x 10 COST MATRIX
cost_matrix <- matrix(
  NA,
  nrow = 10,
  ncol = 10
)
rownames(cost_matrix) <- points$Point
colnames(cost_matrix) <- points$Point
diag(cost_matrix) <- 0
point_keys <- paste(points$X, points$Y)
origin_keys <- paste(clean_data$Origin_X, clean_data$Origin_Y)
destination_keys <- paste(clean_data$Dest_X, clean_data$Dest_Y)
origin_number <- match(origin_keys, point_keys)
destination_number <- match(destination_keys, point_keys)
if (any(is.na(origin_number)) || any(is.na(destination_number))) {
  stop("Some coordinates do not match the ten delivery points.")
}
for (i in 1:10) {
  for (j in 1:10) {
    if (i != j) {
      route_times <- clean_data$Travel_Time_Minutes[
        origin_number == i & destination_number == j
      ]
      if (length(route_times) > 0) {
        cost_matrix[i, j] <- quantile(
          route_times,
          0.75,
          na.rm = TRUE
        )
      }
    }
  }
}

# Q2(a) GIVE BLOCKED ROUTES A LARGE PENALTY
valid_costs <- cost_matrix[
  !is.na(cost_matrix) & row(cost_matrix) != col(cost_matrix)
]
if (length(valid_costs) == 0) {
  stop("There are no valid travel times in the dataset.")
}
penalty <- max(valid_costs) * 10 + 1
cost_matrix[is.na(cost_matrix)] <- penalty
cat("Penalty for blocked routes:", penalty, "minutes\n")
cat("\n===== Q2(a): COST MATRIX =====\n")
print(cost_matrix)
write.csv(cost_matrix, "q2/cost_matrix_Q2.csv")

# Q2(b) NEAREST NEIGHBOUR ALGORITHM
current_point <- 1
visited <- rep(FALSE, 10)
visited[current_point] <- TRUE
route <- c(current_point)
for (step in 1:9) {
  available_points <- which(visited == FALSE)
  available_costs <- cost_matrix[current_point, available_points]
  next_point <- available_points[which.min(available_costs)]
  route <- c(route, next_point)
  visited[next_point] <- TRUE
  current_point <- next_point
}
route <- c(route, 1)
total_time <- 0
cat("\n===== Q2(b): NEAREST NEIGHBOUR ROUTE =====\n")
for (i in 1:10) {
  from <- route[i]
  to <- route[i + 1]
  journey_time <- cost_matrix[from, to]
  cat(
    points$Point[from], "to", points$Point[to],
    ":", journey_time, "minutes\n"
  )
  total_time <- total_time + journey_time
}
cat("\nFinal route:\n")
cat(paste(points$Point[route], collapse = " -> "), "\n")
cat("Total expected travel time:", total_time, "minutes\n")
route_output <- data.frame(
  From = points$Point[route[1:10]],
  To = points$Point[route[2:11]]
)
write.csv(route_output, "q2/nearest_neighbour_route.csv",
          row.names = FALSE)
cat("\n===== ASSIGNMENT SCRIPT FINISHED =====\n")
