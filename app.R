#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Data import, cleaning
library(shiny)
library(stringr)
library(here)
library(tidyverse)
library(pdftools)
library(purrr)
library(ggplot2)
library(scales)

'%ni%' <- Negate("%in%")

#vector_text <- here("Data", "social_report_2017_en.pdf") %>% 
#    pdf_text() %>% 
#    str_split("\n") %>% 
#    unlist()


# Function looping through the years and processing the PDF files

# Years from 2012 to 2022, excluding 2013
years <- c(2012, 2014:2022)
# Function to process PDF and create raw_text
process_pdf <- function(year) {
    # Generate the PDF file name based on the year
    pdf_file <- here("Data", glue::glue("social_report_{year}_en.pdf"))
    
    # Read the PDF content and create the raw_text vector
    raw_text <- pdf_text(pdf_file) %>%
        str_split("\n") %>%
        unlist()
    
    # Return the raw_text
    return(raw_text)
}

# Use map to process PDFs and create raw_text({year} names)
raw_text_2019 <- readRDS(paste0(here(), "/raw_text_2019.rds")) 

raw_text_list <- years %>% map_at(c(1:6, 8:10), process_pdf) %>% 
    set_names(glue::glue("raw_text_{years}"))

raw_text_list[[7]] <- raw_text_2019
#############################################_




table_no_staff_site <- function(raw){
    # Regex pattern to match the heading with variations
    regex_pattern <- "Breakdown of number of (staff|employees).*site"
    # Find the index of the heading using regex
    heading_index <- str_which(raw, regex_pattern)
    if (length(heading_index) > 0) {
        # Find the position of the first instance of "Munich" following the heading
        start_pos <- heading_index + 1
        following_Munich_pos <- NULL
        for (i in start_pos:length(raw)) {
            if (str_detect(raw[i], "\\bMunich\\b")) {
                following_Munich_pos <- i
                break
            }
        }
        
        # Calculate the position of the first instance of "Brussels" following the "Munich"
        if (!is.null(following_Munich_pos)) {
            table_end <- NULL
            for (i in (following_Munich_pos + 1):length(raw)) {
                if (str_detect(raw[i], "\\bBrussels\\b")) {
                    table_end <- i
                    break
                }
            }
        } else {
            table_end <- NA
        }
        
        # Set the output as the positions calculated
        table_start <- following_Munich_pos
    } else {
        table_start <- NA
        table_end <- NA
    }
    
    
    table <- raw[(table_start):(table_end)]
    table <- str_replace_all(table, "\\s{2,}", "|")
    text_con <- textConnection(table)
    data_table <- read.csv(text_con, sep = "|", header = F)
    if (ncol(data_table) == 8) {
        data_table <- data_table %>% select(c(1, 5:7)) %>% 
            mutate(across(c(2:4), ~as.numeric(str_remove_all(.x, " ")))) %>% 
            rename("Site" = 1, "Female" = 2, "Male" = 3, "Staff number" = 4) %>% 
            mutate(`Total staff` = sum(`Staff number`))
    } else {
        data_table <- data_table %>% 
            select(1, 3) %>% 
            mutate(across(c(2), ~as.numeric(str_remove_all(.x, " ")))) %>% 
            rename("Site" = 1, "Staff number" = 2) %>% 
            mutate(`Total staff` = sum(`Staff number`))
        
    }
    
}


#########################################

table_nationality_compare_2015_to_2022 <- function(raw){
    # Regex pattern to match the heading with variations
    regex_pattern <- "Comparison between nationality"
    # Find the index of the heading using regex
    heading_index <- str_which(raw, regex_pattern)
    if (length(heading_index) > 0) {
        # Find the position of the first instance of "German" following the heading
        start_pos <- heading_index + 1
        following_german_pos <- NULL
        for (i in start_pos:length(raw)) {
            if (str_detect(raw[i], "\\bGerman\\b")) {
                following_german_pos <- i
                break
            }
        }
        
        # Calculate the position of the first instance of "Total" following the "German"
        if (!is.null(following_german_pos)) {
            table_end <- NULL
            for (i in (following_german_pos + 1):length(raw)) {
                if (str_detect(raw[i], "\\bTotal\\b")) {
                    table_end <- i
                    break
                }
            }
        } else {
            table_end <- NA
        }
        
        # Set the output as the positions calculated
        table_start <- following_german_pos
    } else {
        table_start <- NA
        table_end <- NA
    }
    
    '%ni%' <- Negate("%in%")
    table <- raw[(table_start):(table_end)]
    table <- str_replace_all(table, "\\s{2,}", "|")
    text_con <- textConnection(table)
    data_table <- read.csv(text_con, sep = "|", header = F) %>% 
        select(c(1,2, 4, 6)) %>% 
        slice(1:(n()-1)) %>% 
        mutate(across(c(2:4), ~as.numeric(str_remove_all(.x, " ")))) %>% 
        rename("Nationality" = 1, "EPO Staff" = 2, , "Country population" = 3, "Patent applications per country" = 4) %>% 
        mutate(Nationality = str_replace_all(Nationality, "\\*", "")) %>%  
        mutate(Nationality = ifelse(Nationality %in% c(" FYROM", " Macedonian"), " Northern Macedonian", Nationality)) %>% 
        arrange(Nationality) %>%
        filter(Nationality %ni% (c("", " Nationality", " Liechtenstein", " Monegasque", " San Marino", " Montenegrin"))) %>% 
        filter(!str_detect(Nationality, " EPO (average|Average)")) %>% 
        mutate(`% of total EPO` = `EPO Staff`/sum(`EPO Staff`)) %>% 
        mutate(`% of total EPC population` = `Country population`/sum(`Country population`)) %>% 
        mutate(`Over/under representation` = `% of total EPO`/`% of total EPC population`) %>% 
        mutate(Country = c("Albania", "Austria", "Belgium", "United Kingdom", "Bulgaria", "Croatia",
                           "Cyprus", "Czech Republic", "Denmark", "Netherlands", "Estonia", "Finland",
                           "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Italy",
                           "Latvia", "Lithuania", "Luxembourg", "Malta", "North Macedonia", "Norway",
                           "Poland", "Portugal", "Romania", "Serbia", "Slovakia", "Slovenia", "Spain",
                           "Sweden", "Switzerland", "Turkey"))
    
    
}


#########################################

table_basic_salaries_2016_to_2022 <- function(vector){
    # Regex pattern to match the heading with variations
    regex_pattern <- "Basic salar(y|ies) paid.+(EUR)?"
    # Find the index of the heading using regex
    heading_index <- str_which(vector, regex_pattern)
    if (length(heading_index) > 0) {
        # Find the position of the first instance of "Job group" following the heading
        start_pos <- heading_index + 1
        following_job_group_pos <- NULL
        for (i in start_pos:length(vector)) {
            if (str_detect(vector[i], "\\bJG1\\b")) {
                following_job_group_pos <- i
                break
            }
        }
        
        # Calculate the position of the first instance of "Applications" following the "Average_number"
        if (!is.null(following_job_group_pos)) {
            table_end <- NULL
            for (i in (following_job_group_pos + 1):length(vector)) {
                if (str_detect(vector[i], "\\bJG6\\b")) {
                    table_end <- i
                    break
                }
            }
        } else {
            table_end <- NA
        }
        
        # Set the output as the positions calculated
        table_start <- following_job_group_pos
    } else {
        table_start <- NA
        table_end <- NA
    }
    
    
    table <- vector[(table_start):(table_end)]
    table <- str_replace_all(table, "\\s{2,}", "|")
    text_con <- textConnection(table)
    data_table <- read.csv(text_con, sep = "|", header = F) %>% 
        select(c(1, 3, 4)) %>% 
        mutate(across(c(2,3), ~str_remove_all(.x, " "))) %>% 
        mutate(across(c(2,3), ~as.numeric(.))) %>% 
        rename('Job group' = 1, 'Salaries' = 2, 'Average monthly salary' = 3)
    data_table <- data_table[!str_detect(data_table$`Job group`, "Total JG1-4|B|C"), ]
    
    return(data_table)
    
}   


#########################################



table_general_welfare <- function(raw){
    # Regex pattern to match the heading with variations
    regex_pattern <- "Breakdown of expenditure for general staff welfare"
    # Find the index of the heading using regex
    heading_index <- str_which(raw, regex_pattern)
    if (length(heading_index) > 0) {
        # Find the position of the first instance of "Canteen" following the heading
        start_pos <- heading_index + 1
        following_canteen_pos <- NULL
        for (i in start_pos:length(raw)) {
            if (str_detect(raw[i], "Canteen")) {
                following_canteen_pos <- i
                break
            }
        }
        
        # Calculate the position of the first instance of "Total" following the "Canteen"
        if (!is.null(following_canteen_pos)) {
            table_end <- NULL
            for (i in (following_canteen_pos + 1):length(raw)) {
                if (str_detect(raw[i], "\\bTotal\\b")) {
                    table_end <- i
                    break
                }
            }
        } else {
            table_end <- NA
        }
        
        # Set the output as the positions calculated
        table_start <- following_canteen_pos
    } else {
        table_start <- NA
        table_end <- NA
    }
    
    
    table <- raw[(table_start):(table_end)]
    table <- str_replace_all(table, "\\s{2,}", "|")
    text_con <- textConnection(table)
    data_table <- read.csv(text_con, sep = "|", header = F) %>% 
        slice(1:(n()-1))
    
    # Check the number of columns and drop columns 2 and 4 if needed
    
    if (ncol(data_table) == 4) {
        data_table <- data_table %>% select(-c(2, 4))
    } else {
        data_table <- data_table %>% 
            select(-1) 
        
    }
    
    data_table <- data_table %>% 
        mutate(across(c(2), ~as.numeric(str_remove_all(.x, " ")))) %>% 
        rename('Category' = 1, 'Euros' = 2) %>% 
        mutate(Category = str_replace_all(Category, "\\*", "")) %>% 
        mutate(Category = str_replace_all(Category, "\\s\\(.*\\)", "")) %>% 
        mutate(Category = str_replace(Category, "^\\s", "")) %>% 
        filter(Category %ni% c("Special circumstances", "DG 1 team reinforcement budget")) %>% 
        mutate(Category = case_when(
            Category == "Social events Office" ~"Social Events office (SEO)",
            Category == "Social events office" ~"Social Events office (SEO)",
            Category == "Pension association subsidy"     ~ "Pensioners' Association subsidy (PA)",
            Category == "Pensioners’ association subsidy" ~ "Pensioners' Association subsidy (PA)",
            Category == "Pensioners' Association subsidy" ~ "Pensioners' Association subsidy (PA)",
            Category == "Welfare Miscellaneous"  ~ "Welfare miscellaneous (WM)",
            Category == "Welfare, miscellaneous" ~ "Welfare miscellaneous (WM)",
            Category == "Welfare miscellaneous" ~ "Welfare miscellaneous (WM)",
            Category == "AMICALE, culture and sport clubs, social events" ~ "AMICALE, culture and sports clubs, social events (AMICALE)",
            Category == "AMICALE, culture and sports clubs, social events" ~ "AMICALE, culture and sports clubs, social events (AMICALE)",
            Category == "Canteen subsidies" ~ "Canteen subsidies (Canteen)",
            Category == "Motivation budget" ~ "Motivation budget (Motivation)",
            TRUE ~ Category) 
        ) %>% 
        mutate(Category_abbreviated = str_extract(Category, "\\(\\w+\\)")) %>% 
        mutate(Category_abbreviated = str_remove_all(Category_abbreviated, "\\(|\\)"))
    
    
    
    return(data_table)
}

#table_general_welfare_df <- table_general_welfare(vector_text)


table_no_staff_type <- function(raw) {
    # Regex pattern to match the heading with variations
    regex_pattern <- "Breakdown of number of (staff|employees).*type"
    # Find the index of the heading using regex
    heading_index <- str_which(raw, regex_pattern)
    if (length(heading_index) > 0) {
        # Find the position of the first instance of "Examiners" following the heading
        start_pos <- heading_index + 1
        following_examiners_pos <- NULL
        for (i in start_pos:length(raw)) {
            if (str_detect(raw[i], "\\bExaminers\\b")) {
                following_examiners_pos <- i
                break
            }
        }
        
        # Calculate the position of the first instance of "Other" following the "Examiners"
        if (!is.null(following_examiners_pos)) {
            table_end <- NULL
            for (i in (following_examiners_pos + 1):length(raw)) {
                if (str_detect(raw[i], "\\bOther\\b")) {
                    table_end <- i
                    break
                }
            }
        } else {
            table_end <- NA
        }
        
        # Set the output as the positions calculated
        table_start <- following_examiners_pos
    } else {
        table_start <- NA
        table_end <- NA
    }
    
    
    table <- raw[(table_start):(table_end)]
    table <- str_replace_all(table, "\\s{2,}", "|")
    text_con <- textConnection(table)
    
    data_table <- read.csv(text_con, sep = "|", header = F)
    if (ncol(data_table) == 6) {
        data_table <- data_table %>% select(c(1, 3:5)) %>% 
            mutate(across(c(2:4), ~as.numeric(str_remove_all(.x, " ")))) %>% 
            rename("Function" = 1, "Female" = 2, "Male" = 3, "Staff number" = 4) %>% 
            mutate(Function = str_replace(Function, "^\\s+", "")) %>% 
            mutate(Function = case_when(
                Function == "Examiners and Boards of Appeal" ~"Examiners",
                Function == "Members of Boards of Appeal"     ~ "Members of Boards of Appeal",
                Function == "Members of boards of appeal"     ~ "Members of Boards of Appeal",
                Function == "Patent procedure support" ~ "Patent procedures support",
                Function == "Patent procedure support1" ~ "Patent procedures support",
                Function == "Patent procedures support staff" ~ "Patent procedures support",
                TRUE ~ Function) 
            ) %>%
            
            mutate(`Total staff` = sum(`Staff number`))
        #mutate(Function_abbreviated = str_extract(Function, "\\(\\w+\\)")) %>% 
        #mutate(Function_abbreviated = str_remove_all(Function_abbreviated, "\\(|\\)"))
        
        
    } else {
        data_table <- data_table %>% 
            select(1, 3) %>% 
            mutate(across(c(2), ~as.numeric(str_remove_all(.x, " ")))) %>%
            rename("Function" = 1, "Staff number" = 2) %>% 
            mutate(Function = str_replace(Function, "^\\s+", "")) %>% 
            mutate(Function = case_when(
                Function == "Examiners and Boards of Appeal" ~"Examiners",
                Function == "Members of Boards of Appeal"     ~ "Members of Boards of Appeal",
                Function == "Members of boards of appeal"     ~ "Members of Boards of Appeal",
                Function == "Patent procedure support" ~ "Patent procedures support",
                Function == "Patent procedure support1" ~ "Patent procedures support",
                Function == "Patent procedures support staff" ~ "Patent procedures support",
                TRUE ~ Function) 
            )%>% 
            mutate(`Total staff` = sum(`Staff number`))
    }
    
    
    
    return(data_table)
}

########################################



table_accidents <- function(raw){
    # Regex pattern to match the heading with variations
    regex_pattern <- "occupational accidents.*\\d{4}"
    # Find the index of the heading using regex
    heading_index <- str_which(raw, regex_pattern)
    if (length(heading_index) > 0) {
        # Find the position of the first instance of "Munich" following the heading
        start_pos <- heading_index + 1
        following_Munich_pos <- NULL
        for (i in start_pos:length(raw)) {
            if (str_detect(raw[i], "\\bMunich\\b")) {
                following_Munich_pos <- i
                break
            }
        }
        
        # Calculate the position of the first instance of "Applications" following the "Average_number"
        if (!is.null(following_Munich_pos)) {
            table_end <- NULL
            for (i in (following_Munich_pos + 1):length(raw)) {
                if (str_detect(raw[i], "\\bBrussels\\b")) {
                    table_end <- i
                    break
                }
            }
        } else {
            table_end <- NA
        }
        
        # Set the output as the positions calculated
        table_start <- following_Munich_pos
    } else {
        table_start <- NA
        table_end <- NA
    }
    
    
    table <- raw[(table_start):(table_end)]
    table <- str_replace_all(table, "\\s{2,}", "|")
    text_con <- textConnection(table)
    data_table <- read.csv(text_con, sep = "|", header = F) %>% 
        select(-2) %>% 
        mutate(across(c(2), ~str_remove_all(.x, " "))) %>% 
        mutate(across(c(2), ~as.numeric(.))) %>% 
        rename('Site' = 1, 'Number of accidents' = 2)
    
    return(data_table)
    
}   

########################################


table_ending_service <- function(raw){
    # Regex pattern to match the heading with variations
    regex_pattern <- "ending active service,\\s\\d{4}"
    # Find the index of the heading using regex
    heading_index <- str_which(raw, regex_pattern)
    if (length(heading_index) > 0) {
        # Find the position of the first instance of "Retirement" following the heading
        start_pos <- heading_index + 1
        following_retirement_exp_pos <- NULL
        for (i in start_pos:length(raw)) {
            if (str_detect(raw[i], "\\bRetirement\\b")) {
                following_retirement_exp_pos <- i
                break
            }
        }
        
        # Calculate the position of the first instance of "Other" following the "Average_number"
        if (!is.null(following_retirement_exp_pos)) {
            table_end <- NULL
            for (i in (following_retirement_exp_pos + 1):length(raw)) {
                if (str_detect(raw[i], "Other")) {
                    table_end <- i
                    break
                }
            }
        } else {
            table_end <- NA
        }
        
        # Set the output as the positions calculated
        table_start <- following_retirement_exp_pos
    } else {
        table_start <- NA
        table_end <- NA
    }
    
    
    table <- raw[(table_start):(table_end)]
    table <- str_replace_all(table, "\\s{2,}", "|")
    text_con <- textConnection(table)
    data_table <- read.csv(text_con, sep = "|", header = F) %>% 
        select(c(1, 4)) %>% 
        mutate(across(c(2), ~as.numeric(str_remove_all(.x, " ")))) %>% 
        rename('Reason' = 1, 'Number of people' = 2) %>% 
        mutate(Reason = str_replace_all(Reason, "\\*", "")) %>% 
        mutate(Reason = str_replace(Reason, "^\\s+", "")) %>% 
        mutate(Reason = case_when(
            Reason == "Other"      ~ "Other reason",
            Reason == "Other type" ~ "Other reason",
            TRUE ~ Reason)
        )
    
    
}   


########################################

table_talent <- function(raw){
    # Regex pattern to match the heading with variations
    regex_pattern <- "planned and (incurred|spent)"
    # Find the index of the heading using regex
    heading_index <- str_which(raw, regex_pattern)
    if (length(heading_index) > 0) {
        # Find the position of the first instance of "Planned" following the heading
        start_pos <- heading_index + 1
        following_talent_exp_pos <- NULL
        for (i in start_pos:length(raw)) {
            if (str_detect(raw[i], "\\bPlanned\\b")) {
                following_talent_exp_pos <- i
                break
            }
        }
        
        # Calculate the position of the first instance of "Applications" following the "Average_number"
        if (!is.null(following_talent_exp_pos)) {
            table_end <- NULL
            for (i in (following_talent_exp_pos + 1):length(raw)) {
                if (str_detect(raw[i], "\\bTotal\\b")) {
                    table_end <- i
                    break
                }
            }
        } else {
            table_end <- NA
        }
        
        # Set the output as the positions calculated
        table_start <- following_talent_exp_pos
    } else {
        table_start <- NA
        table_end <- NA
    }
    
    
    table <- raw[(table_start):(table_end)]
    table <- str_replace_all(table, "\\s{2,}", "|")
    text_con <- textConnection(table)
    data_table <- read.csv(text_con, sep = "|", header = F) %>% 
        select(-c(1:4, 7)) %>% 
        slice(2) %>%
        mutate(across(c(1,2), ~as.numeric(str_remove_all(.x, " ")))) %>% 
        rename("Planned expenditure" = 1, , "Amount spent" = 2) %>% 
        mutate(`Percentage of budget spent` = (`Amount spent`/`Planned expenditure`)*100)
    
    
} 


########################################
table_applications_per_vacancy_2017_to_2022 <- function(raw) {
    # Regex pattern to match the heading with variations
    regex_pattern <- "applications per vacancy,\\s\\d{4}-\\d{4}"
    # Find the index of the heading using regex
    heading_index <- str_which(raw, regex_pattern)
    if (length(heading_index) > 0) {
        # Find the position of the first instance of 'Average' following the heading
        start_pos <- heading_index + 1
        following_years_pos <- NULL
        for (i in start_pos:length(raw)) {
            if (str_detect(raw[i], "^\\s\\w{4}")) {
                following_years_pos <- i
                break
            }
        }
        
        # Calculate the position of the first instance of "*Applications" following the "Average"
        if (!is.null(following_years_pos)) {
            table_end <- NULL
            for (i in (following_years_pos + 1):length(raw)) {
                if (str_detect(raw[i], "(\\Applications|Source)")) {
                    table_end <- i
                    break
                }
            }
        } else {
            table_end <- NA
        }
        
        # Set the output as the positions calculated
        table_start <- following_years_pos
    } else {
        table_start <- NA
        table_end <- NA
    }
    
    
    table <- raw[(table_start):(table_end)]
    table <- str_replace_all(table, "\\s{2,}", "|")
    text_con <- textConnection(table)
    data_table <- read.csv(text_con, sep = "|", header = F) %>% 
        select(-4) %>% 
        slice(c(3:5)) %>% 
        mutate(across(c(2,3), ~str_remove_all(.x, " "))) %>% 
        mutate(across(c(2,3), ~as.numeric(.))) %>% 
        rename("Year" = 1, "Examiner post" = 2, "Non-examiner posts" = 3) %>% 
        mutate(Year = str_replace_all(Year, "\\*", "")) %>% 
        
        #pivot_longer(cols = -Year, names_to = "Job Type", values_to = "Number of Applications")
        pivot_longer(cols = c(`Examiner post`, `Non-examiner posts`), names_to = "Job Type", values_to = "Number of Applications")
    
    
    
}



########################################
table_conflicts <- function(raw){
    # Regex pattern to match the heading with variations
    regex_pattern <- "Nature\\s\\w+\\sconflict"
    # Find the index of the heading using regex
    heading_index <- str_which(raw, regex_pattern)
    if (length(heading_index) > 0) {
        # Find the position of the first instance of "Manager" following the heading
        start_pos <- heading_index + 1
        following_manager_exp_pos <- NULL
        for (i in start_pos:length(raw)) {
            if (str_detect(raw[i], "Manager")) {
                following_manager_exp_pos <- i
                break
            }
        }
        
        # Calculate the position of the first instance of "Total" following the "manager"
        if (!is.null(following_manager_exp_pos)) {
            table_end <- NULL
            for (i in (following_manager_exp_pos + 1):length(raw)) {
                if (str_detect(raw[i], "\\bTotal\\b")) {
                    table_end <- i
                    break
                }
            }
        } else {
            table_end <- NA
        }
        
        # Set the output as the positions calculated
        table_start <- following_manager_exp_pos
    } else {
        table_start <- NA
        table_end <- NA
    }
    
    
    table <- raw[(table_start):(table_end)]
    table <- str_replace_all(table, "\\s{2,}", "|")
    text_con <- textConnection(table)
    data_table <- read.csv(text_con, sep = "|", header = F) %>%
        slice(1:(n()-1)) %>% 
        select(where(function(x) any(!is.na(x)))) %>%   # removing colums where all values are NA. This is the case with years 2020:2022 where first column is NA
        select(c(1, 4)) %>%
        mutate(across(c(2), ~as.numeric(str_remove_all(.x, " ")))) %>% 
        rename("Conflict_type" = 1, , "Number of cases" = 2) %>% 
        mutate(Conflict_type = str_remove(Conflict_type, "^\\s")) %>% 
        #mutate(Conflict_type = str_replace(Conflict_type, "/", "-")))
        na.omit() %>% 
        mutate(Conflict_type = case_when(
            str_detect(Conflict_type, "the hierarchical line") ~ "Manager employee (ME)",
            str_detect(Conflict_type, "the hierarchical line") ~ "Manager employee (ME)",
            str_detect(Conflict_type, "Managers/employees in the") ~ "Manager employee (ME)",
            
            Conflict_type == "Private matter"           ~ "Private matters (PM)",
            Conflict_type == "Private matters"          ~ "Private matters (PM)",
            Conflict_type == "Manager and manager"      ~ "Between managers (MM)",
            Conflict_type == "Between managers"         ~ "Between managers (MM)",
            Conflict_type == "Between colleagues"       ~ "Between colleagues (CC)",
            Conflict_type == "Other"  ~ "Other (Other)",
            Conflict_type == "Requests for information" ~ "Request for information (RI)",
            Conflict_type == "Request for information"  ~ "Request for information (RI)",
            Conflict_type == "Counselling on workplace difficulties" ~ "Counselling on workplace difficulties (CWD)",
            TRUE ~Conflict_type)) %>% 
        mutate(Conflict_abbreviated = str_extract(Conflict_type, "\\(\\w+\\)")) %>% 
        mutate(Conflict_abbreviated = str_remove_all(Conflict_abbreviated, "\\(|\\)"))
    
    
    
    
    
    
    
    return(data_table)    
} 


########################################
years <- c(2015:2022)

# Function to extract and process table for a given year
extract_table_conflicts <- function(year) {
    index <- which(years == year)
    raw_text <- pluck(raw_text_list[3:10], index)
    
    # Extract the table using the custom function
    table_data <- table_conflicts(raw_text) 
    
    # Add a year column to the table_data
    table_data <- table_data %>% mutate(Year = year)
    
    # Return the processed table
    return(table_data)
}

# Use map to process raw_text_list, extract tables for each year and rbind them
final_table_conflicts <- map(years, extract_table_conflicts) %>% 
    bind_rows() %>% 
    mutate(Year = as.integer(Year))
########################################_
years <- c(2012, 2014:2022)
# Function to extract and process table for a given year
extract_table_no_staff_type <- function(year) {
    index <- which(years == year)
    raw_text <- pluck(raw_text_list[], index)
    
    # Extract the table using the custom function
    table_data <- table_no_staff_type(raw_text)
    
    # Add a year column to the table_data
    table_data <- table_data %>% mutate(Year = year)
    
    # Return the processed table
    return(table_data)
}

# Use map to process raw_text_list, extract tables for each year and rbind them
final_table_no_staff_type <- map(years, extract_table_no_staff_type) %>% 
    bind_rows() %>% 
    arrange(Function, Year) %>% 
    pivot_longer(cols = c("Male", "Female"), names_to = "Gender", values_to = "Gender staff") %>% 
    mutate(Year = as.integer(Year))


########################################
years <- c(2012, 2014:2022)
# Function to extract and process table for a given year
extract_table_no_staff_site <- function(year) {
    index <- which(years == year)
    raw_text <- pluck(raw_text_list, index)
    
    # Extract the table using the custom function
    table_data <- table_no_staff_site(raw_text)
    
    # Add a year column to the table_data
    table_data <- table_data %>% mutate(Year = year)
    
    # Return the processed table
    return(table_data)
}

# Use map to process raw_text_list, extract tables for each year and rbind them
final_table_no_staff_site <- map(years, extract_table_no_staff_site) %>% 
    bind_rows() %>% 
    pivot_longer(cols = c("Male", "Female"), names_to = "Gender", values_to = "Gender staff") %>% 
    mutate(Year = as.integer(Year))


########################################

years <- c(2019:2022)
# Function to extract and process table for a given year
extract_table_applications_per_vacancy_2017_to_2022 <- function(year) {
    index <- which(years == year)
    raw_text <- pluck(raw_text_list[7:10], index)
    
    # Extract the table using the custom function
    table_data <- table_applications_per_vacancy_2017_to_2022(raw_text) 
    
    # Add a year column to the table_data
    #table_data <- table_data %>% mutate(Year = year)
    
    # Return the processed table
    return(table_data)
}

# Use map to process raw_text_list, extract tables for each year and rbind them
final_table_applications_per_vacancy_2017_to_2022 <- map(years, extract_table_applications_per_vacancy_2017_to_2022) %>% 
    bind_rows() %>%
    distinct() %>% 
    mutate(Year = as.numeric(Year)) %>% 
    arrange(Year) %>% 
    mutate(Year = as.integer(Year))

########################################



years <- c(2012, 2014:2022)
# Function to extract and process table for a given year
extract_table_talent <- function(year) {
    index <- which(years == year)
    raw_text <- pluck(raw_text_list, index)
    
    # Extract the table using the custom function
    table_data <- table_talent(raw_text)
    
    # Add a year column to the table_data
    table_data <- table_data %>% mutate(Year = year)
    
    # Return the processed table
    return(table_data)
}

# Use map to process raw_text_list, extract tables for each year and rbind them
final_table_talent <- map(years, extract_table_talent) %>% 
    bind_rows() %>% 
    mutate(Year = as.integer(Year)) #%>% 
#pivot_longer(cols = c(`Planned expenditure`, `Amount spent`), names_to = "Talent expenses", values_to = "EUR")

########################################

years <- c(2014:2022)
# Function to extract and process table for a given year
extract_table_general_welfare <- function(year) {
    index <- which(years == year)
    raw_text <- pluck(raw_text_list, index)
    
    # Extract the table using the custom function
    table_data <- table_general_welfare(raw_text)
    
    # Add a year column to the table_data
    table_data <- table_data %>% mutate(Year = year)
    
    # Return the processed table
    return(table_data)
}

# Use map to process raw_text_list, extract tables for each year and rbind them
final_table_general_welfare <- map(years, extract_table_general_welfare) %>% 
    bind_rows() %>% 
    arrange(Category) %>% 
    mutate(Year = as.integer(Year))

########################################
years <- c(2012, 2014:2022)
# Function to extract and process table for a given year
extract_table_accidents <- function(year) {
    index <- which(years == year)
    raw_text <- pluck(raw_text_list, index)
    
    # Extract the table using the custom function
    table_data <- table_accidents(raw_text)
    
    # Add a year column to the table_data
    table_data <- table_data %>% mutate(Year = year)
    
    # Return the processed table
    return(table_data)
}

# Use map to process raw_text_list, extract tables for each year and rbind them
final_table_accidents <- map(years, extract_table_accidents) %>% 
    bind_rows() %>% 
    left_join(final_table_no_staff_site) %>% 
    select(c(1:4)) %>% 
    mutate(`Per capita accidents` = (`Number of accidents`/`Staff number`)*100) %>% 
    mutate(Year = as.integer(Year))



########################################
years <- c(2012, 2014:2022)

extract_table_service_ending <- function(year) {
    index <- which(years == year)
    raw_text <- pluck(raw_text_list, index)
    
    # Extract the table using the custom function
    table_data <- table_ending_service(raw_text)
    
    # Add a year column to the table_data
    table_data <- table_data %>% mutate(Year = year)
    
    # Return the processed table
    return(table_data)
}

# Use map to process raw_text_list, extract tables for each year and rbind them
final_table_service_ending <- map(years, extract_table_service_ending) %>% 
    bind_rows() %>% 
    left_join(final_table_no_staff_type, by = "Year") %>% 
    select(c(1:3, 6)) %>% 
    mutate(`Per capita ending` = (`Number of people`/`Total staff`)*100) %>% 
    mutate(Year = as.integer(Year))


########################################
# List of years
years <- c(2016:2022)

extract_table_basic_salaries_2016_to_2022 <- function(year) {
    index <- which(years == year)
    raw_text <- pluck(raw_text_list[4:10], index)
    
    # Extract the table using the custom function and add year column
    table_data <- table_basic_salaries_2016_to_2022(raw_text)%>%
        mutate(Year = year)
    
    # Add a year column to the table_data
    total_salary <- table_data %>% 
        group_by(Year) %>% 
        summarize(`Total Annual Salaries` = sum(Salaries))
    
    table_data <- table_data %>% 
        left_join(total_salary, by = "Year") %>% 
        mutate(`Share of total salary` = (Salaries/`Total Annual Salaries`)*100)
    
    # Return the processed table
    return(table_data)
}

# Use map to process raw_text_list, extract tables for each year and rbind them
final_table_basic_salaries_2016_to_2022 <-  map(years, extract_table_basic_salaries_2016_to_2022) %>% 
    bind_rows() %>% 
    mutate(Year = as.integer(Year))

# EPO_interactive_plots

library(transformr)
library(viridis)
library(viridisLite)
library(scales)
library(plotly)
library(cowplot)
library(ggrepel)
library(hrbrthemes)

#_______________________________________________________________________________
# Create an interactive plot of final_table_coflict using gganimate
options(repr.plot.width = 8, repr.plot.height =3)
plot_conflict <- final_table_conflicts %>% 
    
    ggplot(aes(x=Year,  y=`Number of cases`, color = Conflict_type)) +
    geom_line(linewidth = 1.1)+
    geom_point() +
    ggtitle("Fight or flight",
            subtitle = "Number and types of conflicts")+
    scale_color_viridis(discrete = TRUE) +
    #labs(title = "Fight or flight", subtitle = 'Number and types of conflicts')+
    theme_ipsum()+
    xlab("")+
    ylab("Conflict numbers") +
    theme(aspect.ratio=16/9,
          legend.position = "right",
          axis.text.x = element_text(size = 8, hjust = 0.5),
          axis.text.y = element_text(size = 7, hjust = 0.5)) +
    scale_x_continuous(breaks = 2012:2023)

interactive_plot_conflict <- ggplotly(plot_conflict) %>% 
    layout(title = list(text = paste0("Fight or flight",
                                      '<br>',
                                      '<sup>',
                                      "Number and types of conflicts")))


#_______________________________________________________________________________
# Create an interactive plot  of EPO accidents using plotly
plot_accidents <- final_table_accidents %>% 
    
    ggplot(aes(x=Year,  y=`Number of accidents`, color = Site)) +
    geom_line(linewidth = 1.1)+
    geom_point() +
    ggtitle("Watch your step!",
            subtitle = "Work accidents at EPO sites") +
    scale_color_viridis(discrete = TRUE) +
    #labs(subtitle = 'Work accidents at EPO sites')
    theme_ipsum()+
    xlab("")+
    ylab("Number of accidents") +
    theme(aspect.ratio=16/9,
          legend.position = "right",
          axis.text.x = element_text(size = 8, hjust = 0.5),
          axis.text.y = element_text(size = 7, hjust = 0.5))+
    scale_x_continuous(breaks = 2012:2023)

interactive_plot_accidents <- ggplotly(plot_accidents) %>% 
    layout(title = list(text = paste0("Watch your step!",
                                      '<br>',
                                      '<sup>',
                                      "Work accidents at EPO sites")))





# Create an interactive plot  of per capita accidents using plotly
plot_pc_accidents <- final_table_accidents %>% 
    
    ggplot(aes(x=Year,  y=`Per capita accidents`, color = Site)) +
    geom_line(linewidth = 1.1)+
    geom_point() +
    ggtitle("Watch your step!",
            subtitle = "Accidents at EPO sites/100 people") +
    scale_color_viridis(discrete = TRUE) +
    #labs(subtitle = 'Work accidents at EPO sites')
    theme_ipsum()+
    xlab("")+
    ylab("Number of accidents") +
    theme(aspect.ratio=16/9,
          legend.position = "right",
          axis.text.x = element_text(size = 8, hjust = 0.5),
          axis.text.y = element_text(size = 7, hjust = 0.5))+
    scale_x_continuous(breaks = 2012:2023)

interactive_plot_pc_accidents <- ggplotly(plot_pc_accidents) %>% 
    layout(title = list(text = paste0("",
                                      '<br>',
                                      '<sup>',
                                      "Accidents at EPO sites/100 people")))



#_______________________________________________________________________________

# Create an interactive plot of  applications per_vacancy using plotly

plot_applications <- final_table_applications_per_vacancy_2017_to_2022 %>% 
    
    ggplot(aes(x=Year,  y=`Number of Applications`, color = `Job Type`)) +
    geom_line(linewidth = 1.1)+
    geom_point() +
    ggtitle("Popularity contest",
            subtitle = "Job applications per vacancy") +
    scale_color_viridis(discrete = TRUE) +
    #labs(subtitle = 'Work accidents at EPO sites')
    theme_ipsum()+
    ylab("Number of applications") +
    theme(aspect.ratio=16/9,
          legend.position = "right",
          axis.text.x = element_text(size = 8, hjust = 0.5),
          axis.text.y = element_text(size = 7, hjust = 0.5)) +
    scale_x_continuous(breaks = 2012:2023)

interactive_plot_applications <- ggplotly(plot_applications) %>% 
    layout(title = list(text = paste0("Popularity contest",
                                      '<br>',
                                      '<sup>',
                                      "Job applications per vacancy")))
#_______________________________________________________________________________


# Create an interactive plot of final_table_general_welfare using plotly

plot_welfare <- final_table_general_welfare %>% 
    
    ggplot(aes(x=Year,  y= Euros, color = Category)) +
    geom_line(linewidth = 1.1)+
    geom_point() +
    ggtitle("Live long and prosper",
            subtitle = "Expenses (in EUR) on general welfare") +
    scale_color_viridis(discrete = TRUE) +
    #labs(subtitle = 'Work accidents at EPO sites')
    theme_ipsum()+
    ylab("Amount spent") +
    theme(aspect.ratio=16/9,
          legend.position = "right",
          axis.text.x = element_text(size = 8, hjust = 0.5),
          axis.text.y = element_text(size = 7, hjust = 0.5)) +
    scale_x_continuous(breaks = 2012:2023)

interactive_plot_welfare <- ggplotly(plot_welfare) %>% 
    layout(title = list(text = paste0("Live long and prosper",
                                      '<br>',
                                      '<sup>',
                                      "Expenses (in EUR) on general welfare")))



#_______________________________________________________

# Create an interactive plot of final_table_no_staff_type using plotly
plot_staff_type <- final_table_no_staff_type %>% 
    
    ggplot(aes(x=Year,  y= `Staff number`, color = Function)) +
    geom_line(linewidth = 1.1)+
    geom_point() +
    ggtitle("United in numbers",
            subtitle = "Number of EPO staff by function-type") +
    scale_color_viridis(discrete = TRUE) +
    #labs(subtitle = 'Work accidents at EPO sites')
    theme_ipsum()+
    ylab("Number of staff") +
    theme(aspect.ratio=16/9,
          legend.position = "right",
          axis.text.x = element_text(size = 8, hjust = 0.5),
          axis.text.y = element_text(size = 7, hjust = 0.5)) +
    scale_x_continuous(breaks = 2012:2023)

interactive_plot_staff_type <- ggplotly(plot_staff_type) %>% 
    layout(title = list(text = paste0("United in numbers",
                                      '<br>',
                                      '<sup>',
                                      "Number of EPO staff by function-type")))

plot_staff_site <- final_table_no_staff_site %>% 
    
    ggplot(aes(x=Year,  y= `Staff number`, color = Site)) +
    geom_line(linewidth = 1.1)+
    geom_point() +
    ggtitle("Number of EPO staff by site", subtitle = "") +
    scale_color_viridis(discrete = TRUE) +
    #labs(subtitle = 'Work accidents at EPO sites')
    theme_ipsum()+
    ylab("Number of staff") +
    theme(aspect.ratio=16/9,
          legend.position = "right",
          axis.text.x = element_text(size = 8, hjust = 0.5),
          axis.text.y = element_text(size = 7, hjust = 0.5)) +
    scale_x_continuous(breaks = 2012:2023)

interactive_plot_staff_site <- ggplotly(plot_staff_site) %>% 
    layout(title = list(text = paste0("",
                                      '<br>',
                                      '<sup>',
                                      "Number of EPO staff by site")))


final_table_no_staff_type_gender <- final_table_no_staff_type %>% na.omit() 

plot_staff_type_gender <- final_table_no_staff_type_gender %>% 
    ggplot(aes(x=Year,  y= `Gender staff`, color = Gender)) +
    geom_line(linewidth = 1.1)+
    geom_point() +
    ggtitle("Mind the gender gap", 
            subtitle = "Number of EPO staff by function-type and gender") +
    scale_color_viridis(discrete = TRUE) +
    #labs(subtitle = 'Work accidents at EPO sites')
    theme_ipsum()+
    xlab("")+
    ylab("Number of staff") +
    theme(aspect.ratio=16/9,
          legend.position = "right",
          axis.text.x = element_text(size = 8, hjust = 0.5),
          axis.text.y = element_text(size = 7, hjust = 0.5))+
    scale_x_continuous(breaks = 2012:2023)+
    facet_wrap(~Function)

interactive_plot_staff_type_gender <- ggplotly(plot_staff_type_gender) %>% 
    layout(title = list(text = paste0("Mind the gender gap",
                                      '<br>',
                                      '<sup>',
                                      "Number of EPO staff by function-type and gender")))


final_table_no_staff_site_gender <- final_table_no_staff_site %>% na.omit() 

plot_staff_site_gender <- final_table_no_staff_site_gender %>% 
    ggplot(aes(x=Year,  y= `Gender staff`, color = Gender)) +
    geom_line(linewidth = 1.1)+
    geom_point() +
    ggtitle("Number of EPO staff by site and gender", subtitle = "") +
    scale_color_viridis(discrete = TRUE) +
    #labs(subtitle = 'Work accidents at EPO sites')
    theme_ipsum()+
    xlab("")+
    ylab("Number of staff") +
    theme(aspect.ratio=16/9,
          legend.position = "right",
          axis.text.x = element_text(size = 8, hjust = 0.5),
          axis.text.y = element_text(size = 7, hjust = 0.5))+
    scale_x_continuous(breaks = 2012:2023) + 
    facet_wrap(~Site)

interactive_plot_staff_site_gender <- ggplotly(plot_staff_site_gender) %>% 
    layout(title = list(text = paste0("",
                                      '<br>',
                                      '<sup>',
                                      "Number of EPO staff by site and gender")))

#___________________________________________________________________________________________________

# Create an interactive plot of EPO salaries using plotly

plot_salaries <- final_table_basic_salaries_2016_to_2022 %>% 
    ggplot(aes(x=Year,  y= `Average monthly salary`, color = `Job group`)) +
    geom_line(linewidth = 1.1)+
    geom_point() +
    ggtitle("", subtitle = "EPO average monthly salaries by job group") +
    scale_color_viridis(discrete = TRUE) +
    #labs(subtitle = 'Work accidents at EPO sites')
    theme_ipsum()+
    xlab("")+
    ylab("EUR") +
    theme(aspect.ratio=16/9,
          legend.position = "right",
          axis.text.x = element_text(size = 8, hjust = 0.5),
          axis.text.y = element_text(size = 7, hjust = 0.5))+
    scale_x_continuous(breaks = 2012:2023) #+ 
#facet_wrap(~Site)

interactive_plot_salaries <- ggplotly(plot_salaries) %>% 
    layout(title = list(text = paste0("Who gets what",
                                      '<br>',
                                      '<sup>',
                                      "EPO average monthly salaries by job group")))


plot_salary_shares <- final_table_basic_salaries_2016_to_2022 %>% 
    ggplot(aes(x=Year,  y= `Share of total salary`, color = `Job group`)) +
    geom_line(linewidth = 1.1)+
    geom_point() +
    ggtitle("", subtitle = "EPO salary shares by job group") +
    scale_color_viridis(discrete = TRUE) +
    #labs(subtitle = 'Work accidents at EPO sites')
    theme_ipsum()+
    xlab("")+
    ylab("EUR") +
    theme(aspect.ratio=16/9,
          legend.position = "right",
          axis.text.x = element_text(size = 8, hjust = 0.5),
          axis.text.y = element_text(size = 7, hjust = 0.5))+
    scale_x_continuous(breaks = 2012:2023) #+ 
#facet_wrap(~Site)

interactive_plot_salary_shares <- ggplotly(plot_salary_shares) %>% 
    layout(title = list(text = paste0("",
                                      '<br>',
                                      '<sup>',
                                      "EPO salary shares by job group")))


#___________________________________________________________________________________________________

# Create interactive plots of EPO service ending  using plotly
plot_service_end <- final_table_service_ending %>% 
    ggplot(aes(x=Year,  y= `Number of people`, color = Reason)) +
    geom_line(linewidth = 1.1)+
    geom_point() +
    ggtitle("", subtitle = "EPO staff ending their service") +
    scale_color_viridis(discrete = TRUE) +
    #labs(subtitle = 'Work accidents at EPO sites')
    theme_ipsum()+
    xlab("")+
    ylab("Number of staff") +
    theme(aspect.ratio=16/9,
          legend.position = "right",
          axis.text.x = element_text(size = 8, hjust = 0.5),
          axis.text.y = element_text(size = 7, hjust = 0.5))+
    scale_x_continuous(breaks = 2012:2023) #+ 
#facet_wrap(~Site)


interactive_plot_service_end <- ggplotly(plot_service_end) %>% 
    layout(title = list(text = paste0("End of the road",
                                      '<br>',
                                      '<sup>',
                                      "EPO staff ending their service")))


plot_pc_service_end <- final_table_service_ending %>% 
    ggplot(aes(x=Year,  y= `Per capita ending`, color = Reason)) +
    geom_line(linewidth = 1.1)+
    geom_point() +
    ggtitle("", subtitle = "EPO staff ending their service/100 people") +
    scale_color_viridis(discrete = TRUE) +
    #labs(subtitle = 'Work accidents at EPO sites')
    theme_ipsum()+
    xlab("")+
    ylab("Number of staff") +
    theme(aspect.ratio=16/9,
          legend.position = "right",
          axis.text.x = element_text(size = 8, hjust = 0.5),
          axis.text.y = element_text(size = 7, hjust = 0.5))+
    scale_x_continuous(breaks = 2012:2023) #+ 
#facet_wrap(~Site)


interactive_plot_pc_service_end <- ggplotly(plot_pc_service_end) %>% 
    layout(title = list(text = paste0("",
                                      '<br>',
                                      '<sup>',
                                      "EPO staff ending their service/100 people")))
#________________________________________________________________________________

library(rsconnect)


ui <- fluidPage(
    titlePanel(
        div(
            style = "display: flex; align-items: center;",
            tags$img(src = "https://www.epo.org/images/logo.gif", height = "80px", width = "140px"),
            HTML("<h1>EPO HR statistics at a glance</h1>")        )
    ),
    tags$head(
        tags$style(
            HTML("
        .sidebar {
          padding-bottom: 0px !important;
        }
      ")
        )
    ),
    tabsetPanel(
        tabPanel("Staff numbers",
                 plotlyOutput("plot_staff_type"),
                 plotlyOutput("plot_staff_site"),
                 plotlyOutput("plot_staff_type_gender"),
                 plotlyOutput("plot_staff_site_gender")),
        tabPanel("Salaries",
                 plotlyOutput("plot_salaries"),
                 plotlyOutput("plot_salary_shares")),
        tabPanel("End of Service",
                 plotlyOutput("plot_service_end"),
                 plotlyOutput("plot_pc_service_end")),
        tabPanel("Applications", plotlyOutput("plot_applications")),
        tabPanel("Accidents",
                 plotlyOutput("plot_accidents"),
                 plotlyOutput("plot_pc_accidents")),
        tabPanel("Conflicts", plotlyOutput("plot_conflict")),
        tabPanel("Welfare", plotlyOutput("plot_welfare"))
        
        
    )
)

server <- function(input, output) {
    
    output$plot_staff_type <- renderPlotly({
        interactive_plot_staff_type
    })
    output$plot_staff_site <- renderPlotly({
        interactive_plot_staff_site
    })
    output$plot_staff_type_gender <- renderPlotly({
        interactive_plot_staff_type_gender
    })
    output$plot_staff_site_gender <- renderPlotly({
        interactive_plot_staff_site_gender
    })
    output$plot_salaries <- renderPlotly({
        interactive_plot_salaries
    })
    output$plot_salary_shares <- renderPlotly({
        interactive_plot_salary_shares
    })
    output$plot_service_end <- renderPlotly({
        interactive_plot_service_end
    })
    output$plot_pc_service_end <- renderPlotly({
        interactive_plot_pc_service_end
    })
    
    output$plot_applications <- renderPlotly({
        interactive_plot_applications
    })
    
    output$plot_accidents <- renderPlotly({
        interactive_plot_accidents
    })
    
    output$plot_pc_accidents <- renderPlotly({
        interactive_plot_pc_accidents
    })
    
    output$plot_conflict <- renderPlotly({
        interactive_plot_conflict
    })
    
    output$plot_welfare <- renderPlotly({
        interactive_plot_welfare
    })
   
}

shinyApp(ui = ui, server = server)

