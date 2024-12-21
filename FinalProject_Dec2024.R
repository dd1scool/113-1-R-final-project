# Final Project by Derek (002)
# This project will aim to practice data visualization and data formatting using public dataset.

#Import and read Data
data <- readr::read_csv("Data/臺北市勞動力及就業按半年別.csv", locale = readr::locale(encoding = "BIG5"))


#Check data
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

#Again, check data type of all columns
column_types <- sapply(data, class)
print(column_types)

# Convert Column to factor type and ensure it is ordered
data <- data |> 
  dplyr::mutate(`半年[季/月]平均別` = factor(`半年[季/月]平均別`, levels = unique(`半年[季/月]平均別`)))

# Create line graph for unemployment rate
ggplot(data, aes(x = `半年[季/月]平均別`, y = `失業率[%]`, group = 1)) +
  geom_line() +
  geom_point() +
  labs(title = "失業率變化圖", x = "半年[季/月]平均別", y = "失業率 [%]") +
  theme_minimal()



#delete incomplete Entries from Row 2 to Row 69.
data <- data |> 
  dplyr::slice(-c(1:68)) |> 
  dplyr::ungroup()

#Check if deletion is completed
head(data)


# Reshape data to long format
data_long <- data |> 
  tidyr::pivot_longer(
    cols = c(`就業者教育程度/大專以上占就業者人數[%]`, 
             `就業者教育程度/高中職占就業者人數[%]`, 
             `就業者教育程度/國中以下占就業者人數[%]`),
    names_to = "教育程度",
    values_to = "占比"
  ) |> 
  # Order the factor levels for 教育程度
  dplyr::mutate(教育程度 = factor(教育程度, 
                              levels = c("就業者教育程度/大專以上占就業者人數[%]", 
                                         "就業者教育程度/高中職占就業者人數[%]", 
                                         "就業者教育程度/國中以下占就業者人數[%]")))

# Create stacked bar chart
ggplot(data_long, aes(x = `半年[季/月]平均別`, y = 占比, fill = 教育程度)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "就業者教育程度占比", x = "半年[季/月]平均別", y = "占比 [%]") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")  # Optional: change color palette

