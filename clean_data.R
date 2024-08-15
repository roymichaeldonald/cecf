
## TEST COMMENT TO SEE IF YOU CAN PULL FROM GITHUB...


## Script to pull raw data from google drive, format data, and save clean .csv


## If these throw an error, run install.packages("package_name")
library(tidyverse)
library(dplyr)
library(googlesheets4)
library(googledrive)
library(glue)
library(stringr)
select <- dplyr::select

CURRENT_DIRECTORY = substr(rstudioapi::getSourceEditorContext()$path, 1, 30)
READ_LOCAL_DATA = F # Set to T if you want to read data from computer

STATES <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "Florida", "Georgia", 
            "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", 
            "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", 
            "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", 
            "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", 
            "West Virginia", "Wisconsin", "Wyoming", "District of Columbia")

print("Loading data...")
if(READ_LOCAL_DATA==T){
  # read data from local file
  raw_data = read_csv('Path/To/Raw/Data/On/Local/Machine')
  
}else{
  # read data from GoogleDrive
  raw_data = read_sheet('https://docs.google.com/spreadsheets/d/1pT2hoYN6oVbGkiTV-Jr_ofwoI2c4RIAG8cO6nPxJ1k0/edit?gid=0#gid=0')
}

print("Data Loaded")
print("Formatting Data...")

# set up df so there's 1 row per Organization
long_data = raw_data %>% 
  select(State, `Organization Name`, URL) %>%
  mutate(nationwide = str_detect(State, 'Nationwide'),
         URL = if_else(substr(URL,1,4)!='http',
                       paste0('https://',URL),
                       URL),
         State = str_replace_all(State, "; ", ";")) %>% 
  mutate(State = str_replace_all(State, ", ",";")) %>% 
  mutate(State = str_replace_all(State,",",";")) %>% 
  separate_rows(State, sep = ";") %>% 
  mutate(State = if_else(State == 'Washington DC', 'District of Columbia', State)) %>% 
  mutate(State = if_else(State == 'Washington D.C', 'District of Columbia', State)) %>% 
  mutate(State = if_else(State == 'Washington D.C.', 'District of Columbia', State)) %>% 
  mutate(State = if_else(State == 'Pennslyvannia', 'Pennsylvania',State)) %>% 
  filter(State != "Nationwide")

# Count number of Organizations per state
state_org_count_df = raw_data %>% 
  select(State, `Organization Name`) %>% 
  separate_rows(State, sep = "; ") %>% 
  filter(State != "Nationwide") %>% 
  group_by(State) %>% 
  summarise(Number = n())

# construct a label that lists the name of the state and below that the number
# of organizations in that state

# Add column for multi-state or state-based and join dataframes
long_data = long_data %>% 
  inner_join(raw_data %>% 
  select(State, `Organization Name`) %>% 
  separate_rows(State, sep = "; ") %>% 
  filter(State != "Nationwide") %>% 
  group_by(`Organization Name`) %>% 
  summarise(state_count = n()) %>% 
  mutate(Category = if_else(state_count==1, 'state-based','multi-state')) %>% 
  select(`Organization Name`, Category), by="Organization Name") %>% 
  inner_join(state_org_count_df, by = "State") %>% 
  mutate(Label = paste(State, Number, sep = '<br>'))

# Add states without organizations to df
no_org_df = data.frame()

for(state in STATES){
  
  if((state %in% unique(long_data$State))==F){
      
      state_row = data.frame(State = state,
                             `Organization Name` = NA,
                             URL = NA,
                             Category = NA,
                             Number = 0,
                             Label = paste(state, "0", sep="<br>"))
      
      no_org_df = rbind(no_org_df, state_row)
  
  }
}

long_data = long_data %>% 
  rbind(no_org_df) 
         # rename(`Organization Name` = Organization.Name))

# Add Tooltip
# Create a “tooltip” that lists all of the orgs
# in that state, and the url for the orgs (which the mapping program can then 
# use to make a clickable link by formatting it using html)

long_data = long_data %>%
  group_by(State) %>% 
  arrange(`Organization Name`) %>% 
  ungroup() %>% 
  group_by(State,Number,Label, nationwide) %>%
  reframe(Nationwide_tooltip = if_else(nationwide==T,
                                       paste(glue('<b>National organizations with community partnerships or local chapters in {State}</b><br>'),
                                             str_c("<ul>", paste0("<li>", paste0('<a href="', URL, '">', `Organization Name`, '</a>'), "</li><br>", collapse = ""), "</ul><br>")),
                                       NA),
          local_tooltip = if_else(nationwide==F,
                                  str_c("<br><b>Local and Regional Organizations </b><br> <ul>", paste0("<li>", paste0('<a href="', URL, '">', `Organization Name`, '</a>'), "</li><br>", collapse = ""), "</ul><br>"),
                                  NA)) %>% 
  ungroup() %>% 
  group_by(State) %>% 
  fill(Nationwide_tooltip, .direction = 'up') %>% 
  fill(local_tooltip, .direction = 'down') %>% 
  mutate(local_tooltip = if_else(is.na(local_tooltip), "", local_tooltip),
         Nationwide_tooltip = if_else(is.na(Nationwide_tooltip), "", Nationwide_tooltip)) %>% 
  ungroup() %>% 
  mutate(combo_tooltip = paste(local_tooltip, Nationwide_tooltip)) %>% 
  group_by(State, Number, Label) %>% 
  reframe(
    Tooltip = paste(glue('{State} has the following {Number} climate action organizations collaborating with CECF <br>'),
                    combo_tooltip)
  ) %>% 
  mutate(Tooltip = if_else(Number==0, NA, Tooltip)) %>% 
    select(State,Number,Label,Tooltip) %>% 
    unique() %>% 
  filter(State != 'http://')

print("Data Formatted")

print("Saving Data...")
# Save data to Google Drive and Local
file_name = paste0(paste0("cecf_map_sourcefile_",
                          format(Sys.time(), "%Y-%m-%d")),
                   '.csv')
folder_id = '1NNAUdtfyjCbgRo09t0Veb8xZmV4BS7rW'
local_data_dir = paste0(CURRENT_DIRECTORY, '/data/clean/')
file_name_local_dir = paste0(local_data_dir, file_name)

# write to local directory
# long_data %>% write_csv(file_name_local_dir)

# write to google drive
sheet = drive_create(substr(file_name,1,30),
                      path = as_id(folder_id),
                      type = "spreadsheet",
                     overwrite=T)

sheet %>%  sheet_append(data.frame(t(colnames(long_data))))
long_data %>% sheet_append(ss = sheet$id)

print("Data saved")

