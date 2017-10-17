# Start execution
start_time <- proc.time()

# Set working directory
setwd("/Users/552143/Desktop/T20-cricket-prediction/")

# Lib imports
require(readr)

# Import matches
match_files <- list.files("matches/")

info_headers <- list()
match_info <- list()
for (match in match_files) {
  if (strsplit(match, split = "[.]")[[1]][2] == "csv") {
    raw_txt <- scan(file = paste("matches", match, sep = "/"), what = character(), sep = "\n", quiet = TRUE)
    info_txt <- c()
    for (line in raw_txt) {
      if (strsplit(line, split = ",")[[1]][1] == "info") {
        info_txt[length(info_txt) + 1] = line
        if (strsplit(line, split = ",")[[1]][2] %in% names(info_headers)) {
          info_headers[[strsplit(line, split = ",")[[1]][2]]] <- info_headers[[strsplit(line, split = ",")[[1]][2]]] + 1
        } else {
          info_headers[[strsplit(line, split = ",")[[1]][2]]] <- 1
        }
      }
    }
    match_info[[strsplit(match, split = "[.]")[[1]][1]]] <- list(length=length(info_txt), text=info_txt)
  }
}



info_header_list <- names(info_headers)
info_header_list[grep("gender", info_header_list)] <- "team2"
info_header_list <- c("Match_Id", info_header_list)
info_header_index <- list()
for (header in info_header_list) {
  info_header_index[[header]] <- grep(paste0("^", header, "$"), info_header_list)
}

ball_header_list <- c("Innings", "Overs", "Batting_Team", "Striker", "Non_Striker", "Bowler", "Run_Scored", "Extras", "Dismissal", "Dismissed")
ball_header_list <- c("Match_Id", ball_header_list)
ball_header_index <- list()
for (header in ball_header_list) {
  ball_header_index[[header]] <- grep(paste0("^", header, "$"), ball_header_list)
}

matches_data <- data.frame()
for (column in info_header_list) matches_data[column] <- as.character()

balls_data <- data.frame()
for (column in ball_header_list) balls_data[column] <- as.character()

data_validation <- data.frame()
for (column in c("Match_Id", "Non_NA_info_Count", "Ball_Count")) data_validation[column] <- as.character()

create_ball_data <- function(match_id, raw_ball_data) {
  ball_data_tmp <- as.data.frame(t(mapply(function(x,y) c(x, as.character(sapply(strsplit(y, split = ",")[[1]], function(x) gsub("\"", "", x)))), match_id, raw_ball_data))[,-2])
  names(ball_data_tmp) <- ball_header_list
  row.names(ball_data_tmp) <- as.character(1:nrow(ball_data_tmp))
  balls_data <<- rbind(balls_data, ball_data_tmp)
}


for (match in match_files) {
  if (strsplit(match, split = "[.]")[[1]][2] == "csv") {
    raw_txt <- scan(file = paste("matches", match, sep = "/"), what = character(), sep = "\n", quiet = TRUE)
    match_row <- c(strsplit(match, split = "[.]")[[1]][1], rep(NA, 24))
    info_count <- 0
    for (line in raw_txt) {
      if (strsplit(line, split = ",")[[1]][1] == "info") {
        info_count <- info_count + 1
        if (strsplit(line, split = ",")[[1]][2] %in% info_header_list) {
          if (strsplit(line, split = ",")[[1]][2] == "team") {
            if (is.na(match_row[info_header_index[["team"]]])) {
              match_row[info_header_index[["team"]]] <- strsplit(line, split = ",")[[1]][3]
            } else {
              match_row[info_header_index[["team2"]]] <- strsplit(line, split = ",")[[1]][3]
            }
          } else {
            match_row[info_header_index[[strsplit(line, split = ",")[[1]][2]]]] <- strsplit(line, split = ",")[[1]][3]
          }
        }
      } 
    }
    match_row_df <- as.data.frame(t(match_row))
    names(match_row_df) <- info_header_list
    matches_data <- rbind(matches_data, match_row_df)
    
    validation_df <- as.data.frame(t(c(strsplit(match, split = "[.]")[[1]][1], sum(as.numeric(is.na(match_row))), length(raw_txt) - (info_count+1))))
    names(validation_df) <- c("Match_Id", "Non_NA_info_Count", "Ball_Count")
    data_validation <- rbind(data_validation, validation_df)
    
    create_ball_data(strsplit(match, split = "[.]")[[1]][1], raw_txt[(info_count+2):(length(raw_txt))])
  }
}

sum(as.numeric(as.character(data_validation$Ball_Count)))
table(data_validation$Non_NA_info_Count == apply(matches_data, 1, function(x) sum(as.numeric(is.na(x)))))

matches_data <- merge(x = matches_data, y = balls_data, by = "Match_Id", all.x = TRUE)
matchid_ballcount_map <- list(table(matches_data$Match_Id))[[1]]
data_validation$Ball_Count_Post_Merge <- sapply(data_validation$Match_Id, function(x) matchid_ballcount_map[[x]])
data_validation$Final_Check <- ifelse(data_validation$Ball_Count == data_validation$Ball_Count_Post_Merge, 1, 0)
if (sum(data_validation$Final_Check) == nrow(data_validation)) {
  print("All data validation steps were completed SUCCESSFULLY")
  write_csv(matches_data, "T20_matches_ball_by_ball_data.csv", na = "")
} else {
  print("Final data validation check FAILED")
}

print(proc.time() - start_time)
