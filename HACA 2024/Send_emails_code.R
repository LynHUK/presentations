##############################################
#Install and Load Packages
##############################################
list.of.packages <- c("readxl", "emayili")  # List the needed packages
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]  # Of the needed packages, list those that are not already installed
if(length(new.packages)) install.packages(new.packages) # Install any packages which arent already installed
for (k in 1:length(list.of.packages)) {
  library(list.of.packages[k], character.only = TRUE)  # Load the needed packages using library
}
rm(list = c("k","list.of.packages","new.packages"))

##############################################
#Run the following code to open the.Renviron file.
#usethis::edit_r_environ()
#
#Then set 
#  EMAIL_ADDRESS = "***@***"
#  EMAIL_PASSWORD = 
#
#Save and close the file.
#The variables EMAIL_ADDRESS & EMAIL_PASSWORD can be retrieved using the Sys.getenv() function
##############################################

smtp <- emayili::server(host = "send.nhs.net",
                        port = 587,
                        username = Sys.getenv("EMAIL_ADDRESS"),
                        password = Sys.getenv("EMAIL_PASSWORD"))

email_df <- read_excel(file.path(Sys.getenv("MY_PATH"),"07. ERIC 2022 23/Validation/Summer validation/04. Distribution list/ERIC and Sustainability Contacts Lists.xlsx", sheet = "ExportContacts")) |>  rename(trust_code = "Trust Code", trust_name = "Trust Name")
review_deadline <- "27/09/2023"

for(t in unique(email_df$trust_code)){ 

  Selected_Row = email_df |>  filter(trust_code == t) 
  
email <- emayili::envelope() %>%
  emayili::from(Sys.getenv("EMAIL_ADDRESS")) %>%
  emayili::to(unlist(strsplit(Selected_Row$`Combined email`, ";")))%>%
  emayili::cc(Sys.getenv("EMAIL_ADDRESS")) %>%
  emayili::subject(paste0("Trust: ", Selected_Row$trust_code, " - ERIC 2022-23 Validation Report (ACTION REQUIRED)")) %>% 
  emayili::text(paste0("Dear Colleagues,

Trust: ", Selected_Row$trust_code, " - ", Selected_Row$trust_name, "  

Many thanks for completing your trust ERIC 22/23 data return, we appreciate the effort that your teams have put in to complete this work.
We appreciate your continued involvement in this journey with us, ensuring published data regarding NHS trusts is as accurate as possible.

Note: Saving HTML report.
Right-click on the attached file and select Save As, save where appropriate.

NHS England National Estates Team
")
  ) %>% 
  emayili::attachment(file.path(Sys.getenv("MY_PATH"),"07. ERIC 2022 23/Validation/Summer validation/03. Outputs/Reports", paste0("Report_", t,"_(", Sys.Date(), ").html")))

#Send the email
smtp(email, verbose = TRUE)
}