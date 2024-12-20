# Final Project by Derek (002)
# This project will aim to practice data visualization and data formatting using public data.

#Import and read Data
data <- readr::read_csv("Data/臺北市勞動力及就業按半年別.csv", locale = readr::locale(encoding = "BIG5"))

#Entries from Row 2 to Row 69 is incomplete, so i want to delete them.
data <- data |> 
  dplyr::slice(-c(1:68)) |> 
  dplyr::ungroup()

#Check if deletion is completed
head(data)

#Check data type of all columns
column_types <- sapply(data, class)
print(column_types)

#Change string value into numeric
data <- data |> 
  dplyr::mutate(
    `就業者教育程度/大專以上人數[千人]` = as.numeric(`就業者教育程度/大專以上人數[千人]`),
    `就業者教育程度/大專以上占就業者人數[%]` = as.numeric(`就業者教育程度/大專以上占就業者人數[%]`),
    `就業者教育程度/高中職人數[千人]` = as.numeric(`就業者教育程度/高中職人數[千人]`),
    `就業者教育程度/高中職占就業者人數[%]` = as.numeric(`就業者教育程度/高中職占就業者人數[%]`),
    `就業者教育程度/國中以下人數[千人]` = as.numeric(`就業者教育程度/國中以下人數[千人]`),
    `就業者教育程度/國中以下占就業者人數[%]` = as.numeric(`就業者教育程度/國中以下占就業者人數[%]`)
  )

#Change Column A into "date"

library(dplyr)
library(lubridate)

# Function to convert 中華民國年 to Date
convert_to_date <- function(year_month) {
  # Extract year and month
  year <- as.integer(sub("年.*", "", year_month)) + 1911
  month <- as.integer(sub(".*?(\\d+)月", "\\1", year_month))
  
  # Create date
  date_string <- sprintf("%d-%02d-01", year, month)
  
  # Return as Date
  return(as.Date(date_string))
}

# Apply the function to Column A
data <- data %>%
  mutate(`半年[季/月]平均別` = sapply(`半年[季/月]平均別`, convert_to_date))

# Check the structure of the updated data frame
str(data)
head(data)

#Draw Chart to see unemployment trend over time.
ggplot(data, aes(x = `半年[季/月]平均別`, y = `失業率[%]`)) +
  geom_line() +
  labs(title = "Unemployment Rate Over Time",
       x = "Time (半年[季/月]平均別)",
       y = "Unemployment Rate [%]") +
  theme_minimal() +
  xlim(min(data$`半年[季/月]平均別`), max(data$`半年[季/月]平均別`)) +
  ylim(min(data$`失業率[%]`, na.rm = TRUE), max(data$`失業率[%]`, na.rm = TRUE))
