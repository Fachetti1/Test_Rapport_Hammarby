# Load required libraries
library(readxl)
library(dplyr)
library(lubridate)
library(rmarkdown)
library(readr)

# Create 'reports' directory if it doesn't exist
if (!dir.exists("reports")) {
  dir.create("reports")
}

# Load test data
test_data <- read_excel("Test_data.xlsx")

# Function to prepare and standardize the test data
prepare_test_data <- function(data) {
  # Function to convert POSIXct time to seconds
  time_to_seconds <- function(time_value) {
    if (is.na(time_value)) return(NA)
    
    tryCatch({
      # Extract time component and convert to seconds
      time_component <- as.period(time_value - floor_date(time_value, "day"))
      as.numeric(time_component, "seconds")
    }, error = function(e) {
      warning(paste("Error converting time:", as.character(time_value), "-", conditionMessage(e)))
      NA
    })
  }
  
  # Clean and prepare the data
  cleaned_data <- data %>%
    mutate(
      across(c(Bänkpress, Knäböj, Frivändning), as.numeric),
      Cooper_seconds = sapply(Cooper, time_to_seconds),
      Cooper_formatted = sapply(Cooper_seconds, function(x) {
        if(is.na(x)) return(NA)
        sprintf("%02d:%02d", floor(x / 60), round(x %% 60))
      }),
      Datum = as.Date(Datum)
    ) %>%
    select(Namn, Bänkpress, Knäböj, Frivändning, Cooper_seconds, Cooper_formatted, Lag, Datum)
  
  return(cleaned_data)
}

# Function to generate a single player report
generate_player_report <- function(player_name, image_url, test_data) {
  tryCatch({
    player_data <- test_data %>% filter(Namn == player_name)
    if (nrow(player_data) == 0) {
      cat("No data found for player:", player_name, "\n")
      return()
    }
    
    cat("Generating report for", player_name, "\n")
    cat("Data for player:\n")
    print(head(player_data))
    cat("Full test data dimensions:", dim(test_data), "\n")
    
    rmarkdown::render(
      "Player_Reports.Rmd",
      output_file = paste0("reports/", player_name, "_report.html"),
      params = list(
        player_name = player_name,
        image_url = image_url,
        test_data = test_data
      )
    )
    cat("Report generated for", player_name, "\n")
  }, error = function(e) {
    cat("Error generating report for", player_name, ":", conditionMessage(e), "\n")
  })
}

# Prepare the test data
test_data_clean <- prepare_test_data(test_data)

write_csv(test_data_clean, "test_data_clean.csv")

# Create player data
player_data <- data.frame(
  player_name = c("Sam Rydberg", "Hugo Solback", "Theo Lagerdahl", "Simon Hedlund", "Astor Wintzell",
                  "Aron Gergö Csonka", "Ivar Mehra", "Hugo Rasmusson", "Neo Brattnäs", "Oscar Nyman",
                  "Oscar Westblad", "Tage Lönnqvist", "Theo Friberg", "Carl Ahlden",
                  "Harry Andersson", "Elis Lindblom", "Viktor Kornfehl", "Noah Dahlqvist",
                  "Sigge Karlsson", "Casper Höök", "Sindri Svend Thorisson", "Sebastian Simon",
                  "Sixten Nilsson", "Vincent Axell", "Linus Jidell Reichardt", "Anton Grevin", 
                  "Cesar Wikström", "Rasmus Antonsson", "Pontus Kettels", "Alvar Rehnström", 
                  "Anton Fabricius-Hansen", "Arni Arnason", "Ivan Rönnögård", "Jonathan Karlsson Westin", 
                  "Emmet Broman", "Hampus Frimodig", "Alvin Silfverstedt", "Tim Hed", "Benjamin Vos",
                  "Melker Melander", "Adrian Tajalli","Kevin Wikström",
                  "Wilhelm Lagerdahl", "Filip Praetorius", "Jakob Hirsch", "Måns Möller", "Anton Waldna", 
                  "Jack Almenäs", "Andreas Eklöw", "Emil Zander", "Edwin Kukacka", "Linus Jernberg", "Fredric Berntsson",
                  "Alex Lundell", "Andreas Fuchs Edberg", " Max Andersson", "Per Dahlberg", "Gentzi Maturana Gonzalez",
                  "Felix Engelhardt", " Rasmus Strömberg", "Jack Lindblom", "Fredrik Broman", "Antonio Yevenes-Brandt",
                  "Melvin Andre", "Anton Fahlén", "Isac Gausel", "Julian Göranson Ransgart"),
  image_url = c(
    #Sam Rydberg 
    "https://photos.svenskalag.se/1ea31695-261d-4c76-a304-8fe2de3dc090/1ea31695-261d-4c76-a304-8fe2de3dc090_profile.jpg",
    #Hugo Solback
    "",
    #Theo Lagerdahl
    "",
    #Simon Hedlund
    "",
    #Astor Wintzell
    "https://cdn03.svenskalag.se/photos/eae/eaecd2c4-c76e-43a5-a6dc-0d3ff7072970_profile.jpg",
    #Aron Csonka
     "https://photos.svenskalag.se/852c50c1-14ed-4084-8ab0-eba0aab07bae/852c50c1-14ed-4084-8ab0-eba0aab07bae_profile.jpg",
    #Ivan Mehra
    "",
    #Hugo Rasmusson 
    "", 
    #Neo Brattnäs
    "https://photos.svenskalag.se/6ae785ab-2ff8-4032-8bb2-06f7f82b52cf/6ae785ab-2ff8-4032-8bb2-06f7f82b52cf_profile.jpg",
    #Oscar Nyman 
    "", 
    #Oscar Westblad
    "https://photos.svenskalag.se/50594d1a-cdd4-4e2e-a0b1-7b3a6b63ac1d/50594d1a-cdd4-4e2e-a0b1-7b3a6b63ac1d_profile.jpg",
    #Tage Lönnqvist
    "https://photos.svenskalag.se/676a1bbd-4a1d-4f5c-ac76-f3f60714cbeb/676a1bbd-4a1d-4f5c-ac76-f3f60714cbeb_profile.jpg",
    #Theo Friberg
    "",
    #Carl Ahlden
    "https://photos.svenskalag.se/d0e80d6c-0d42-412d-93b2-a562aca6d98a/d0e80d6c-0d42-412d-93b2-a562aca6d98a_profile.jpg",
    #Harry Andersson
    "",
    #Elis Lindblom 
    "https://photos.svenskalag.se/433a23da-f7ff-42eb-b3ef-6332a6af0e27/433a23da-f7ff-42eb-b3ef-6332a6af0e27_profile.jpg",
    #Viktor Kornfehl
    "", 
    #Noah Dalhqvist
    "", 
    #Sigge Karlsson 
    "https://photos.svenskalag.se/4f282522-1310-479d-abb1-65a13fc30c15/4f282522-1310-479d-abb1-65a13fc30c15_profile.jpg",
    #Casper Höök 
    "https://photos.svenskalag.se/0323f1c8-43eb-418f-a23f-38980399815f/0323f1c8-43eb-418f-a23f-38980399815f_profile.jpg",
    #Sindri Svend Thorisson
    "",
    #Sebastian Simon 
    "https://photos.svenskalag.se/44b1ff9e-8779-459f-98e6-42d48e38aecb/44b1ff9e-8779-459f-98e6-42d48e38aecb_profile.jpg",
    #Sixten Nilsson
    "",
    #Vincent Axell
    "",
    #Linus Jidell
    "",
    #Anton Grevin 
    "", 
    #Cesar Wikström 
    "https://photos.svenskalag.se/62f09fe5-184b-4f1d-a9ff-161f81affdf8/62f09fe5-184b-4f1d-a9ff-161f81affdf8_profile.jpg",
    #Rasmus Antonsson 
    "",
    #Pontus Ketels 
    "https://photos.svenskalag.se/7c073a69-b8de-4953-bf81-b35e09cf9773/7c073a69-b8de-4953-bf81-b35e09cf9773_profile.jpg",
    #Alvar Rehnström
    "https://photos.svenskalag.se/c54aaf07-5bbb-49c0-a637-d117e1586a40/c54aaf07-5bbb-49c0-a637-d117e1586a40_profile.jpg",
    #Anton Fabricius-Hansen
    "",
    # Arni Arnason
    "",
    #Ivan Rönnögård
    "https://photos.svenskalag.se/7cf96c64-d017-49cf-acb4-9a4dae4d8818/7cf96c64-d017-49cf-acb4-9a4dae4d8818_profile.jpg",
    #Jonathan Karlsson Westin
    "",
    #Emmet Broman 
    "",
    #Hampus Frimodig
    "", 
    #Alvin Silferstedt
    "https://photos.svenskalag.se/9f593a6e-86cb-487a-be9c-cbee731ca476/9f593a6e-86cb-487a-be9c-cbee731ca476_profile.jpg",
    #Tim Heed
    "https://photos.svenskalag.se/2dcbd281-6898-4482-adc8-1c6e686a53bc/2dcbd281-6898-4482-adc8-1c6e686a53bc_profile.jpg",
    #Benjamin Vos
    "https://photos.svenskalag.se/353c0790-509e-45b1-b6d1-0df4849c00d4/353c0790-509e-45b1-b6d1-0df4849c00d4_profile.jpg",
    #Melker Melander
    "https://photos.svenskalag.se/6114e962-d41c-4248-b151-acfc071f38c9/6114e962-d41c-4248-b151-acfc071f38c9_profile.jpg",
    #Adrian Tajalli 
    "https://photos.svenskalag.se/7a157da3-4b9f-43b0-aef5-a30b9cd3fcbe/7a157da3-4b9f-43b0-aef5-a30b9cd3fcbe_profile.jpg",
    #Kevin Wikström 
    "https://photos.svenskalag.se/41f5b6ec-e5c2-48ca-85aa-f1f2927b1ca9/41f5b6ec-e5c2-48ca-85aa-f1f2927b1ca9_profile.jpg",
    #Wilhelm Lagerdahl
    "https://photos.svenskalag.se/c9656e22-c6af-4500-966b-8a4225512a01/c9656e22-c6af-4500-966b-8a4225512a01_profile.jpg",
    #Filip Praretorius
    "https://photos.svenskalag.se/439b4a22-1cff-4fc6-947e-4ae66e60801f/439b4a22-1cff-4fc6-947e-4ae66e60801f_profile.jpg",
    #Jacob Hirsch
    "https://photos.svenskalag.se/44b15ca8-4178-4a6c-8dd8-aeff6e0f095d/44b15ca8-4178-4a6c-8dd8-aeff6e0f095d_profile.jpg",
    #Måns Möller
    "https://photos.svenskalag.se/67500c42-d9a4-4ab1-a982-33b0783b253b/67500c42-d9a4-4ab1-a982-33b0783b253b_profile.jpg",
    #Anton Waldna
    "https://photos.svenskalag.se/3cf78f0f-38ea-46b4-baca-f0efdb9a49e5/3cf78f0f-38ea-46b4-baca-f0efdb9a49e5_profile.jpg",
    #Jack Almenäs
    "https://photos.svenskalag.se/0bcf9579-9060-4916-957a-d24e79d5533c/0bcf9579-9060-4916-957a-d24e79d5533c_profile.jpg",
    #Andreas Eklöw
    "https://photos.svenskalag.se/4cc8285e-d0e4-4351-beca-88dba8cf4efb/4cc8285e-d0e4-4351-beca-88dba8cf4efb_profile.jpg",
    #Emil Zander
    "https://photos.svenskalag.se/e59da599-6a70-4a64-b13b-6dabfd887b0f/e59da599-6a70-4a64-b13b-6dabfd887b0f_profile.jpg",
    #Edwin Kuckaka
    "https://photos.svenskalag.se/2147c81d-bec7-4ef6-83cc-191a03e63482/2147c81d-bec7-4ef6-83cc-191a03e63482_profile.jpg",
    #Linus Jernberg
    "https://photos.svenskalag.se/c954ddf2-d733-407a-9c17-7ca2c31776e3/c954ddf2-d733-407a-9c17-7ca2c31776e3_profile.jpg",
    #Fredric Berntsson 
    "https://photos.svenskalag.se/88ba680f-dfac-47f1-af6a-e41d0eb09586/88ba680f-dfac-47f1-af6a-e41d0eb09586_profile.jpg", 
    #Alex Lundell 
    "https://photos.svenskalag.se/d1e69c1e-616b-4ae4-b3ba-9197c8653224/d1e69c1e-616b-4ae4-b3ba-9197c8653224_profile.jpg",
    #Andreas Fuchs Edberg
    "https://photos.svenskalag.se/9634cb1e-64d5-46c3-943d-ca547659235f/9634cb1e-64d5-46c3-943d-ca547659235f_profile.jpg",
    #Max Andersson 
    "https://photos.svenskalag.se/4cbe4c52-623e-43d0-ba62-246cf49fa931/4cbe4c52-623e-43d0-ba62-246cf49fa931_profile.jpg",
    #Per Dahlberg
    "https://photos.svenskalag.se/358424c8-7991-4b62-aa64-faf0d6a9b6a3/358424c8-7991-4b62-aa64-faf0d6a9b6a3_profile.jpg",
    #Gentzi Maturana Gonzalez
    "https://photos.svenskalag.se/6c151d39-2c44-4927-a103-898ce056c12d/6c151d39-2c44-4927-a103-898ce056c12d_profile.jpg",
    #Felix Engelhardt
    "https://photos.svenskalag.se/9c7ce661-d45f-4694-9e11-eef54ff80040/9c7ce661-d45f-4694-9e11-eef54ff80040_profile.jpg",
    #Rasmus Strömberg
    "https://photos.svenskalag.se/c3cf8422-47a3-4d47-8f95-3b8e11a7492a/c3cf8422-47a3-4d47-8f95-3b8e11a7492a_profile.jpg",
    #Jack Lindblom 
    "https://photos.svenskalag.se/18224c9a-ee03-417c-ae8c-6bc0f190a8f8/18224c9a-ee03-417c-ae8c-6bc0f190a8f8_profile.jpg",
    #Fredrik Broman 
    "https://cdn03.svenskalag.se/photos/8be/8bead4fa-8f33-41ce-a65c-617a3d05f39d_profile.jpg",
    #Antonio yvenes
    "https://photos.svenskalag.se/bba2f962-19fc-4323-875f-8a3c9dbf1876/bba2f962-19fc-4323-875f-8a3c9dbf1876_profile.jpg",
    #Melvin Andre
    "https://photos.svenskalag.se/9c273585-8ce9-47f9-a323-701eccf87d63/9c273585-8ce9-47f9-a323-701eccf87d63_profile.jpg",
    #Anton Fahlen
    "",
    #Isac Gausel
    "",
    #Julian Göranson Ransgart
    ""
    ))


# Print debug information before generating reports
cat("Debug: test_data_clean dimensions:", dim(test_data_clean), "\n")
cat("Debug: First few rows of test_data_clean:\n")
print(head(test_data_clean))

# Generate reports for all players
for (i in 1:nrow(player_data)) {
  generate_player_report(
    player_data$player_name[i],
    player_data$image_url[i],
    test_data_clean
  )
}

cat("All reports have been generated.\n")

# Generate master report
rmarkdown::render(
  "Master_Report.Rmd",
  output_file = "reports/Master_Report.html",
  params = list(
    test_data = Test_data_AP_clean
  )
)

str(Test_data_AP)
