################################# Run Upon App Execution #################################

#Importing libraries
library(shiny)
library(dplyr)
library(tidyr)
library(stringr)
library(writexl)
library(readxl)
library(MASS)
library(tools)
library(plotly)
library(ggplot2)
library(ggridges)
library(hrbrthemes)
library(viridis)
library(ggiraph)
library(data.table)
library(gt)
library(scales)
library(htmltools)
library(countrycode)
library(htmlwidgets)

#Setting filepaths
main_dataset_file_path = "Career Mode player datasets - FIFA 15-21.xlsx"
main_dataset_cleaned_filepath = "FIFA_data_cleaned.xlsx"
option1_dataset_filepath = "FIFA_data_option1.xlsx"
option3_dataset_filepath = "FIFA_data_option3.xlsx"
option5_dataset_filepath = "FIFA_data_option5.xlsx"

#Prepare/load all datasets 
#Preparing/loading cleaned dataset
if (file.exists(main_dataset_cleaned_filepath)) {
  FIFA_cleaned_DF = read_excel(main_dataset_cleaned_filepath)
} else{
  #List columns to keep
  columns_to_keep_LIST = c("sofifa_id","short_name","long_name","age","dob","height_cm","weight_kg","nationality","club_name","league_name",
                           "league_rank","overall","potential","value_eur","wage_eur","preferred_foot","weak_foot","skill_moves",
                           "team_position","team_jersey_number","pace","shooting","passing","dribbling","defending","physic",
                           "gk_diving","gk_handling","gk_kicking","gk_reflexes","gk_speed","gk_positioning")
  
  #Read all sheets from the excel file
  main_dataset_file_path = "Career Mode player datasets - FIFA 15-21.xlsx"
  sheet_names_LIST = excel_sheets(main_dataset_file_path)
  
  #Initialize an empty data frame to store all cleaned data
  FIFA_cleaned_DF = data.frame()
  #Loop through each sheet, clean it, and append it
  for (sheet in sheet_names_LIST) {
    #Extract year from sheet name
    fifa_edition = as.numeric(str_extract(sheet,"\\d{2}")) 
    #Read the sheet
    FIFA_DF = read_excel(path=main_dataset_file_path,sheet=sheet)
    
    #Loop through rows to replace team_position where needed
    for (i in 1:nrow(FIFA_DF)) {
      if (FIFA_DF$team_position[i] %in% c("SUB","RES")) {
        #Split the player_positions string by commas and take the first element
        first_position = str_split(FIFA_DF$player_positions[i],",")[[1]][1]
        #Update the team_position with the first position
        FIFA_DF$team_position[i] = first_position
      }
    }
    
    #Select relevant columns from the columns_to_keep_LIST
    FIFA_DF_subset = FIFA_DF[,columns_to_keep_LIST]
    #Add the year column
    FIFA_DF_subset$fifa_edition = fifa_edition
    #Convert 'dob' to 'yob'
    FIFA_DF_subset$dob = as.numeric(substr(FIFA_DF_subset$dob,1,4))
    colnames(FIFA_DF_subset)[colnames(FIFA_DF_subset)=="dob"] = "yob"
    #Append cleaned data from this sheet to final data frame
    FIFA_cleaned_DF = rbind(FIFA_cleaned_DF,FIFA_DF_subset)
  }
  
  #Get rid of rows where fifa_edition is NA
  FIFA_cleaned_DF = FIFA_cleaned_DF[!is.na(FIFA_cleaned_DF$fifa_edition), ]
  #Save final cleaned dataset to an excel file
  write_xlsx(FIFA_cleaned_DF,main_dataset_cleaned_filepath)
}
#Preparing/loading option 1 dataset
if (file.exists(option1_dataset_filepath)) {
  FIFA_option1_DF = read_excel(option1_dataset_filepath)
} else{
  #List columns to keep
  columns_to_keep_option1_LIST = c("overall","potential","shooting","passing","dribbling","defending","pace","physic","value_eur","wage_eur")
  
  #Filter data to only keep non-goalkeeper data
  FIFA_filtered_DF = FIFA_cleaned_DF[FIFA_cleaned_DF$team_position!="GK", ]

  #Filter data rows to eliminate NA values
  FIFA_option1_DF = na.omit(FIFA_filtered_DF[, c("fifa_edition","team_position",columns_to_keep_option1_LIST)])
  
  #Old and new column names
  old_names_LIST = copy(columns_to_keep_option1_LIST)
  new_names_LIST = c("Overall","Potential","Shooting","Passing","Dribbling","Defending","Pace","Physical","Market Value (€)",
                     "Weekly Wage (€)")
  #Find the indices of the columns to rename
  col_indices = which(names(FIFA_option1_DF) %in% old_names_LIST)
  #Assign new names to those columns
  names(FIFA_option1_DF)[col_indices] = new_names_LIST
  
  #Save option 1 dataset to an excel file
  write_xlsx(FIFA_option1_DF,option1_dataset_filepath)
}
#Preparing/loading option 3 dataset
if (file.exists(option3_dataset_filepath)) {
  FIFA_option3_DF = read_excel(option3_dataset_filepath)
} else {
  #List columns to keep for both categorical and continuous variables
  columns_to_keep_cat_var_option3_LIST = c("age_group","position_type","continent_of_origin","continent_of_league")
  columns_to_keep_cont_var_option3_LIST = c("overall","potential","shooting","passing","dribbling","defending","pace","physic","value_eur",
                                            "wage_eur")
  
  #Filter data to only keep non-goalkeeper data
  FIFA_filtered_DF = FIFA_cleaned_DF[FIFA_cleaned_DF$team_position!="GK", ]
  
  #Create variable to store age groups
  FIFA_filtered_DF$age_group = cut(FIFA_filtered_DF$age,breaks=c(16,20,25,30,Inf),labels=c("16-20","21-25","26-30","31+"),right=TRUE)
  
  #Create position_type variable from team_position
  FIFA_filtered_DF$position_type = NA
  FIFA_filtered_DF$position_type[FIFA_filtered_DF$team_position %in% c("LWB", "LB", "LCB", "CB", "RCB", "RB", "RWB")] = "Defender"
  FIFA_filtered_DF$position_type[FIFA_filtered_DF$team_position %in% c("LDM", "CDM", "RDM")] = "Defensive Midfield"
  FIFA_filtered_DF$position_type[FIFA_filtered_DF$team_position %in% c("LCM", "CM", "RCM", "LM", "RM")] = "Midfield"
  FIFA_filtered_DF$position_type[FIFA_filtered_DF$team_position %in% c("LAM", "CAM", "RAM")] = "Attacking Midfield"
  FIFA_filtered_DF$position_type[FIFA_filtered_DF$team_position %in% c("LF", "CF", "RF", "LS", "ST", "RS", "LW", "RW")] = "Forward"
  
  #Create continent_of_origin from nationality
  FIFA_filtered_DF$continent_of_origin = countrycode(FIFA_filtered_DF$nationality,origin="country.name",destination="continent")
  extra_nation_to_cont_LIST = c("Wales"="Europe","England"="Europe","Northern Ireland"="Europe","Scotland"="Europe",
                                "Kosovo"="Europe")
  FIFA_filtered_DF$continent_of_origin[is.na(FIFA_filtered_DF$continent_of_origin) &
                                         FIFA_filtered_DF$nationality %in% names(extra_nation_to_cont_LIST)] =
    extra_nation_to_cont_LIST[FIFA_filtered_DF$nationality[is.na(FIFA_filtered_DF$continent_of_origin)]]
  
  #Create continent_of_league from league_name
  FIFA_filtered_DF$continent_of_league = countrycode(FIFA_filtered_DF$league_name,origin="country.name",destination="continent")
  extra_league_to_cont_LIST = c("German 1. Bundesliga"="Europe","French Ligue 1"="Europe",
                                "English Premier League"="Europe","Italian Serie A"="Europe","Turkish Süper Lig"="Europe",
                                "Portuguese Liga ZON SAGRES"="Europe","Danish Superliga"="Europe","Greek Super League"="Europe",
                                "Spanish Segunda División"="Europe","English League Championship"="Europe",
                                "Saudi Abdul L. Jameel League"="Asia","Scottish Premiership"="Europe","Belgian Jupiler Pro League"="Europe",
                                "Chilian Campeonato Nacional"="Americas","German 2. Bundesliga"="Europe","Norwegian Eliteserien"="Europe",
                                "Italian Serie B"="Europe","Polish T-Mobile Ekstraklasa"="Europe","Swedish Allsvenskan"="Europe",
                                "French Ligue 2"="Europe","Scottish Championship"="Europe","English League One"="Europe",
                                "English League Two"="Europe","Campeonato Brasileiro Série A"="Americas",
                                "Campeonato Brasileiro Série B"="Americas","Finnish Veikkausliiga"="Europe",
                                "German 3. Bundesliga"="Europe","Chinese Super League"="Asia","Rest of World"=NA,
                                "UAE Arabian Gulf League"="Asia")
  FIFA_filtered_DF$continent_of_league[is.na(FIFA_filtered_DF$continent_of_league) & FIFA_filtered_DF$league_name %in%
      names(extra_league_to_cont_LIST)] = extra_league_to_cont_LIST[FIFA_filtered_DF$league_name[is.na(FIFA_filtered_DF$continent_of_league)]]
  
  #Filter data rows to eliminate NA values
  FIFA_option3_DF = na.omit(FIFA_filtered_DF[, c("fifa_edition",columns_to_keep_cat_var_option3_LIST,columns_to_keep_cont_var_option3_LIST)])
  
  #Old and new column names
  old_names_LIST = c("age_group","position_type","continent_of_origin","continent_of_league",
                     "overall","potential","shooting","passing","dribbling","defending","pace","physic","value_eur","wage_eur")
  new_names_LIST = c("Age Group","Position Type","Continent of Origin","Continent of League",
                     "Overall","Potential","Shooting","Passing","Dribbling","Defending","Pace","Physical","Market Value (€)",
                     "Weekly Wage (€)")
  #Find the indices of the columns to rename
  col_indices = which(names(FIFA_option3_DF) %in% old_names_LIST)
  #Assign new names to those columns
  names(FIFA_option3_DF)[col_indices] = new_names_LIST
  
  #Save option 3 dataset to an excel file
  write_xlsx(FIFA_option3_DF,option3_dataset_filepath)
}
#Preparing/loading option 5 dataset
if (file.exists(option5_dataset_filepath)) {
  FIFA_option5_DF = read_excel(option5_dataset_filepath)
} else {
  columns_to_keep_option5_LIST = c("short_name","age","team_position","nationality","club_name","league_name","value_eur",
                                   "overall","potential","shooting","passing","dribbling","defending","pace","physic")
  
  #Filter data to only keep non-goalkeeper data
  FIFA_filtered_DF = FIFA_cleaned_DF[FIFA_cleaned_DF$team_position!="GK", ]
  
  #Create position_type variable from team_position
  FIFA_filtered_DF$position_type = NA
  FIFA_filtered_DF$position_type[FIFA_filtered_DF$team_position %in% c("LWB","LB","LCB","CB","RCB","RB","RWB")] = "Defender"
  FIFA_filtered_DF$position_type[FIFA_filtered_DF$team_position %in% c("LDM","CDM","RDM")] = "Defensive Midfielder"
  FIFA_filtered_DF$position_type[FIFA_filtered_DF$team_position %in% c("LCM","CM","RCM","LM","RM")] = "Midfielder"
  FIFA_filtered_DF$position_type[FIFA_filtered_DF$team_position %in% c("LAM","CAM","RAM")] = "Attacking Midfielder"
  FIFA_filtered_DF$position_type[FIFA_filtered_DF$team_position %in% c("LF","CF","RF","LS","ST","RS","LW","RW")] = "Forward"
  
  FIFA_filtered_DF$player_rating = NA
  FIFA_filtered_DF$player_rating = cut(FIFA_filtered_DF$overall,breaks=c(0,64,74,Inf),labels=c("Bronze","Silver","Gold"),right=TRUE)
  
  FIFA_filtered_DF$iso2c = NA
  if (!dir.exists("flags")) {
    dir.create("flags")
  }
  
  FIFA_filtered_DF$iso2c[!is.na(FIFA_filtered_DF$nationality) 
                         & !(FIFA_filtered_DF$nationality %in% c("England", "Northern Ireland", "Scotland", "Wales","Kosovo"))] =
    tolower(countrycode(FIFA_filtered_DF$nationality[!is.na(FIFA_filtered_DF$nationality) 
                                                     & !(FIFA_filtered_DF$nationality %in% c("England", "Northern Ireland", "Scotland", "Wales","Kosovo"))],
                        origin="country.name",destination="iso2c"))
  FIFA_filtered_DF$iso2c[FIFA_filtered_DF$nationality %in% c("England", "Northern Ireland", "Scotland", "Wales")] = "gb"
  FIFA_filtered_DF$iso2c[FIFA_filtered_DF$nationality == "Kosovo"] = "xk"
  FIFA_filtered_DF = FIFA_filtered_DF[!is.na(FIFA_filtered_DF$iso2c), ]
  
  for (iso_code in unique(FIFA_filtered_DF$iso2c)) {
    img_path = paste0("flags/",iso_code,".png")
    if (!file.exists(img_path)) {
      download.file(url = paste0("https://flagcdn.com/w40/",iso_code,".png"),destfile=img_path,mode="wb")
    }
  }
  FIFA_filtered_DF$country_url = paste0("flags/",FIFA_filtered_DF$iso2c,".png")
  
  #Filter data rows to eliminate NA values
  FIFA_option5_DF = na.omit(FIFA_filtered_DF[, c("fifa_edition","position_type","player_rating","country_url",columns_to_keep_option5_LIST)])
  
  #Old and new column names
  old_names_LIST = copy(columns_to_keep_option5_LIST)
  new_names_LIST = c("Player","Age","Position","Country","Club","League","Market Value (€)",
                     "Overall","Potential","Shooting","Passing","Dribbling","Defending","Pace","Physical")
  
  #Find the indices of the columns to rename
  col_indices = which(names(FIFA_option5_DF) %in% old_names_LIST)
  #Assign new names to those columns
  names(FIFA_option5_DF)[col_indices] = new_names_LIST
  
  #Save option 5 dataset to an excel file
  write_xlsx(FIFA_option5_DF,option5_dataset_filepath)
}

#Get min and max FIFA editions
min_fifa_ed = min(FIFA_cleaned_DF$fifa_edition)
max_fifa_ed = max(FIFA_cleaned_DF$fifa_edition)

countries_alphabetical_LIST = sort(unique(FIFA_option5_DF$Country))
club_alphabetical_LIST = sort(unique(FIFA_option5_DF$Club))
player_rating_LIST = rev(unique(FIFA_option5_DF$player_rating))
min_num_players_option5 = 1
max_num_players_option5 = 100
parameter_option5_LIST = c("Age","Position","Country","Club","League","Market Value (€)",
                           "Overall","Potential","Shooting","Passing","Dribbling","Defending","Pace","Physical")
parameter_option5_default_LIST = c("Age","Position","Country","Club",
                                   "Overall","Shooting","Passing","Dribbling","Defending","Pace","Physical")
num_players_option5_default = 10
###########################################################################################





################################### App User Interface ###################################
#Creates the user interface
ui = fluidPage(
  #Add a main title at the top of the app
  titlePanel(div(style="text-align: left; font-weight: bold;","FIFA Player Analysis")),
  #Add all tabs
  tabsetPanel(
    ############################### Tab for Dataset Info ##############################
    tabPanel("About the Data",
             #Create a fluidRow for layout
             fluidRow(column(12,h3("Original Dataset"),p("Dataset taken from: ",a("FIFA 21 Complete Player Dataset on Kaggle", 
                          href="https://www.kaggle.com/datasets/stefanoleone992/fifa-21-complete-player-dataset",target="_blank")),
                      p("The original dataset contains 7 sheets, each containing between 15623 and 18944 rows (excluding the headers for
                          variable/column names), as well as 106 columns of information."),br(),h3("Cleaned Dataset"),
                      p("The cleaned dataset consolidates all editions into a single sheet with 16,155 rows and 33 key variables used 
                        in this app."),
                      tableOutput("data_description_table"),
                      br(),h3("Additional Information"),
                      p("Note that a dataset was also cleaned and filtered for each visualization."),br()
                      ))),
    
    ############ Tab for Option 1- 3D Surface Plot of a 2D Kernel Estimate ############
    tabPanel("3D Surface Plot of 2D KDE",
             #Layout with a sidebar for inputs and a main panel for output
             sidebarLayout(
               #Sidebar for user inputs
               sidebarPanel(
                 h3("Inputs"),
                 br(),
                 selectInput("x_var_option1","X Variable:",
                     choices=c("Overall","Potential","Shooting","Passing","Dribbling","Defending","Pace","Physical","Market Value (€)",
                               "Weekly Wage (€)"),
                     selected="Shooting"),
                 selectInput("y_var_option1","Y Variable:",
                     choices=c("Overall","Potential","Shooting","Passing","Dribbling","Defending","Pace","Physical","Market Value (€)",
                               "Weekly Wage (€)"),
                     selected="Overall"),
                 selectInput("team_position_option1","Club Team Position:",choices = c("All Positions",
                        list("Defense"=c("LWB","LB","LCB","CB","RCB","RB","RWB"),
                             "Defensive Midfield"=c("LDM","CDM","RDM"),"Midfield"=c("LCM","CM","RCM","LM","RM"),
                             "Attacking Midfield"=c("LAM","CAM","RAM"),
                             "Attack"=c("LF","CF","RF","LS","ST","RS","LW","RW"))),selected="All Positions"),
                 sliderInput("fifa_ed_option1","FIFA Edition:",value=21,min=min_fifa_ed,max=max_fifa_ed,step=1),
                 actionButton("reset_view_option1","Reset View",style="background-color:#009E73;color:white;"),
                 actionButton("reset_inputs_option1","Reset All Settings",style="background-color:#D55E00;color:white;")),
               #Main panel for displaying plot output
               mainPanel(
                 plotlyOutput("threedim_surface_plot_option1_FUNC",height="550px"),
                 fluidRow(column(12,br(),p("Above is a 3D surface plot of the 2D kernel density estimate between selected X and Y continuous variables. 
                                                   2D contour surface plots have also been plotted at the top plane. The user can additionally examine how the distribution of selected continuous variables compares over 
                                                   specific positions (i.e. ST, CDM, ect.), as well as specify which edition of FIFA data they would like to 
                                                   visualize. The 'Reset View' button allows the zoom and view of the plot to reset for the current selected 
                                                   variables and the 'Reset All Settings' reverts all settings back to default.")))))
    ),
    
    ########################### Tab for Option 3- Ridge Plot ##########################
    tabPanel("Ridgeline Plot",
             #Layout with a sidebar for inputs and a main panel for output
             sidebarLayout(
               #Sidebar for user inputs
               sidebarPanel(
                 h3("Inputs"),
                 br(),
                 selectInput("cont_var_option3","Continuous Variable:",
                             choices=c("Overall","Potential","Shooting","Passing","Dribbling","Defending","Pace","Physical",
                                       "Market Value (€)","Weekly Wage (€)"),selected="Overall"),
                 selectInput("cat_var_option3", "Categorical Variable:",
                             choices=c("Age Group","Position Type","Continent of Origin","Continent of League"),selected="Age Group"),
                 sliderInput("fifa_ed_option3","FIFA Edition:",value=21,min=min_fifa_ed,max=max_fifa_ed,step=1),
                 actionButton("reset_inputs_option3","Reset All Settings",style="background-color:#D55E00;color:white;")),
               #Main panel for displaying plot output
               mainPanel(girafeOutput("ridgePlot_option3_FUNC",height="500px"),
                         fluidRow(column(12,p("Here, a ridgeline plot shows the kernel density estimate of a selected continuous 
                                              variable accross various groups within a specified categorical attribute 
                                               (i.e. age group, continent of origin, etc.). Furthermore, the user 
                                               is able to hover over the 25th quartile (yellow dotted) line, 
                                               the median (black solid) line, as well as the 75th quartile 
                                               (orange dotted) line to see additional statistical information.")))))
    ),
    
    ##################### Tab for Option 5- GT Summarization Table ####################
    tabPanel("GT Summarization Table",
             #Layout with a sidebar for inputs and a main panel for output
             sidebarLayout(
               #Sidebar for user inputs
               sidebarPanel(
                 h3("Inputs"),
                 br(),
                 selectInput("team_position_option5","Team Position:",choices = c("Any Position",
                             list("General"=c("Defender","Defensive Midfielder","Midfielder","Attacking Midfielder","Forward"),
                                  "Defense"=c("LWB","LB","LCB","CB","RCB","RB","RWB"),
                                  "Defensive Midfield"=c("LDM","CDM","RDM"),"Midfield"=c("LCM","CM","RCM","LM","RM"),
                                  "Attacking Midfield"=c("LAM","CAM","RAM"),
                                  "Attack"=c("LF","CF","RF","LS","ST","RS","LW","RW"))),selected="Any Position"),
                 selectInput("country_option5_LIST","Country:",choices=c("Any Country",countries_alphabetical_LIST),
                                selected="Any Country",multiple=TRUE),
                 selectInput("club_option5_LIST","Club:",choices=c("Any Club",club_alphabetical_LIST),
                             selected="Any Club",multiple=TRUE),
                 selectInput("player_rating_option5","Player Rating:",choices=c("Any Rating",player_rating_LIST),
                             selected="Any Rating"),
                 sliderInput("num_players_option5","Number of Players:",value=10,min=1,max=100,step=1),
                 sliderInput("fifa_ed_option5","FIFA Edition:",value=21,min=min_fifa_ed,max=max_fifa_ed,step=1),
                 selectInput("parameters_to_display_option5_LIST","Parameters to Display:",choices=c("All Parameters",parameter_option5_LIST),
                             selected=parameter_option5_default_LIST,multiple=TRUE),
                 actionButton("reset_inputs_option5","Reset All Settings",style="background-color:#D55E00;color:white;")),
               #Main panel for displaying plot output
               mainPanel(gt_output("gt_summarization_table_option5_FUNC"),
                         fluidRow(column(12,p(em("Note: The flags are obtained from https://flagpedia.net/")),
                                         p("Here, a table is built from the GT library, comparing attributes of the top players.
                                                   The user has the flexibility of specifying a specific position they may want to focus on,
                                                   or select several options of nationalities and/or clubs to represent the players being 
                                                   displayed."),
                                         br(),h3("Attribute Ratings"),p("Each of the following six attributes: 
                                                                 Shooting, Passing, Dribbling, Defending, Pace, and Physical were represented using stars, 
                                                                 where ⭐ represents a full star and ☆ represents a half star. 
                                                                The scoring system was inspired by a similar one FIFA uses to assign a number of stars to 
                                                                a team based on the aggregated `Overall` attribute of its players:"),
                                         tableOutput("stars_description_table"),
                                  br(),h3("Player Rarity"),p("The 'Overall' attribute has been colour coded in the table above to represent their rarity according
                                                              to the FIFA video game, following the criteria below:"),
                                  tableOutput("rarity_description_table"),
                                  br(),h3("Other Features"),
                                  p("Colour schemes were also applied to age, with lighter shades of blue being assigned to younger athletes
                                  and darker shades for their older peers. Simiarly, players with lower potential were assigned lighter greens, 
                                    compared to darker shades for their counterparts with higher values. This notion was also
                                    applied for `Market Value`, going from yellow to an dark orange/red hue. Moreover, 
                                    positions were assigned a unique colour scheme, transitioning from blue, to yellow, to red, going from defenders to forwards.
                                    Finally, the nationalities of players was replaced by their respective flag in order to provide a better visual appeal.")))))
    ),
    
    ##################### Tab for Option 6- Manim Bar Chart ####################
    tabPanel("Manim Bar Chart",
             fluidRow(column(12,mainPanel(img(src="q6_output.png",style="width: 100%; height: auto;"),p("For the Manim component, a bar plot was created 
                                              to demonstrate the average market value (in euros) of players wearing the most common jersey numbers (1 through 11). 
                                              However, the bars were replaced by soccer balls to add a unique effect. 
                                              It was no surprise that players wearing No. 10 seem to carry the most value, as 
                                              many of the famous players pick that as their first choice (i.e. Messi, Neymar)."))))
    )
  )
)
###########################################################################################





####################################### App Server #######################################
#Define app server logic
server = function(input,output,session) {
  ############################# Server Logic For Dataset Info ############################
  #Renders the variable description table
  output$data_description_table = renderTable({
    data.frame(
      #Variable names
      Variable=c("sofifa_id","short_name","long_name","age","yob","height_cm","weight_kg","nationality","club_name","league_name",
                   "league_rank","overall","potential","value_eur","wage_eur","preferred_foot","weak_foot","skill_moves","team_position",
                   "team_jersey_number","pace","shooting","passing","dribbling","defending","physic","gk_diving","gk_handling",
                   "gk_kicking","gk_reflexes","gk_speed","gk_positioning","fifa_edition"),
      #Variable descriptions
      Description=c("The player's ID from Sofifa","The player's short name (e.g., 'L. Messi')","The player's full Name",
        "The player's age","The player's year of birth","The player's height (cm)","The player's weight (kg)",
        "The player's nationality","The player's current club","The player's current league","The league's rank",
        "The player's overall rating","The player's potential rating","The player's market value (€)","The player's weekly wage (€)",
        "The player's preferred foot (left of right)","The player's weak foot rating (1–5)","The player's skill moves rating (1–5)",
        "The player's club team position","The player's club jersey number","The player's pace rating","The player's shooting rating",
        "The player's passing rating","The player's dribbling rating","The player's defence rating","The player's physical rating",
        "The goalkeeper's diving rating","The goalkeeper's handling rating","The goalkeeper's kicking rating",
        "The goalkeeper's reflexes rating","The goalkeeper's speed rating","The goalkeeper's positioning rating",
        "The FIFA edition the data pertains to (e.g., FIFA 21)"),
      stringsAsFactors=FALSE
    )
  })
  
  ########## Server Logic For Option 1- 3D Surface Plot of a 2D Kernel Estimate ##########
  #Create 3D Surface of 2D Kernel Density Estimate plot
  output$threedim_surface_plot_option1_FUNC = bindCache(renderPlotly({
    reactive_data_option1_FUNC = reactive({
      FIFA_option1_filt_DF = FIFA_option1_DF[FIFA_option1_DF$fifa_edition == input$fifa_ed_option1, ]
      
      #Filter data based on selected age group
      if (input$team_position_option1 != "All Positions") {
        FIFA_option1_filt_DF = FIFA_option1_filt_DF[FIFA_option1_filt_DF$team_position == input$team_position_option1, ]
      } 
      
      #Extract relevant columns
      FIFA_option1_filt_DF = FIFA_option1_filt_DF[, c("team_position",input$x_var_option1,input$y_var_option1)]
      #Remove NA values
      FIFA_option1_filt_DF = na.omit(FIFA_option1_filt_DF)
      
      #Define number of contours to draw
      num_contours = 10
      
      #Get kernel density estimate 
      kde = kde2d(FIFA_option1_filt_DF[[input$x_var_option1]],FIFA_option1_filt_DF[[input$y_var_option1]],n=100)
      #Get kernel density estimate value
      max_kde = max(kde$z)
      
      #Preprocessing title and labels
      input_team_position = input$team_position_option1
      if (input$team_position_option1 != "All Positions") {
        input_team_position = paste(input_team_position,"Position")   
      }
      y_title = toTitleCase(gsub("_"," ",input$y_var_option1))
      x_title = toTitleCase(gsub("_"," ",input$x_var_option1))
      if (!(x_title %in% c("Market Value (€)","Weekly Wage (€)"))) {
        x_title = paste(x_title,"Rating")
      }    
      
      #Create plot
      fig = plot_ly(x=kde$x,y=kde$y,z=kde$z,type="surface",colorscale="Viridis",showscale=TRUE,
                    colorbar=list(title=list(text="Density",font=list(size=14)),len=0.5),
                    contours=list(z=list(show=TRUE,start=0,end=max_kde,size=max_kde/num_contours,usecolormap=TRUE,highlightcolor="#ff0000",
                    project=list(z=TRUE))))
      
      #Add title, labels, ticks, set view
      fig = layout(fig,title=list(text=paste0("2D KDE of ",y_title," vs ",x_title,
                            "<br><span style='font-size:16px;'>","FIFA ",input$fifa_ed_option1," Edition (",input_team_position,")","</span>"),
                              font=list(size=20)),
                   scene=list(xaxis=list(title=toTitleCase(input$x_var_option1),ticks="outside",nticks=10),
                              yaxis=list(title=toTitleCase(input$y_var_option1),ticks="outside",nticks=10),
                              zaxis=list(title="Density",ticks="outside",nticks=10),
                              camera=list(eye=list(x=1.6,y=1.6,z=1.2))),
                   margin=list(l=0,r=0,b=0,t=50))
      return(fig)
    })
    reactive_data_option1_FUNC()
  }), input$x_var_option1,input$y_var_option1,input$team_position_option1,input$fifa_ed_option1)
  
  #Reset graph view if Reset View button is pressed
  observeEvent(input$reset_view_option1, {
    plot_proxy=plotlyProxy("threedim_surface_plot_option1_FUNC",session)
    plotlyProxyInvoke(plot_proxy,"relayout",list("scene.camera.eye"=list(x=1.6,y=1.6,z=1.2)))
  })
  
  #Reset graph settings if Reset All Settings button is pressed
  observeEvent(input$reset_inputs_option1, {
    updateSelectInput(session,"x_var_option1",selected="Shooting")
    updateSelectInput(session,"y_var_option1",selected="Overall")
    updateSelectInput(session,"team_position_option1",selected="All Positions")
    updateSliderInput(session,"fifa_ed_option1",value=max_fifa_ed)
  })
  
  ######################### Server Logic For Option 3- Ridgeline Plot ########################
  # Create Ridge Plot
  output$ridgePlot_option3_FUNC = bindCache(renderGirafe({
    reactive_data_option3_FUNC = reactive({
      
      FIFA_option3_filt_DF = FIFA_option3_DF[FIFA_option3_DF$fifa_edition == input$fifa_ed_option3, ]
      
      #Select relevant columns
      selected_cols_LIST = c(input$cat_var_option3,input$cont_var_option3)
      FIFA_option3_filt_DF = na.omit(FIFA_option3_filt_DF[, selected_cols_LIST])
      
      #Get top 500 per group
      unique_groups = unique(FIFA_option3_filt_DF[[input$cat_var_option3]])
      top500_LIST = list()
      
      for (group in unique_groups) {
        group_DF = FIFA_option3_filt_DF[FIFA_option3_filt_DF[[input$cat_var_option3]] == group, ]
        group_DF = group_DF[order(-group_DF[[input$cont_var_option3]]), ]
        n_rows = nrow(group_DF)
        top_n = ifelse(n_rows>=500,500,n_rows)
        top500_LIST[[as.character(group)]] = group_DF[1:top_n, ]
      }
      top500_DF = do.call(rbind,top500_LIST)
      
      #Summary stats_DF per group
      summary_stats_DF = data.frame()
      
      for (group in unique_groups) {
        group_DF = top500_DF[top500_DF[[input$cat_var_option3]] == group, ]
        
        #Extract values of the selected continuous variable
        x_vals = as.numeric(group_DF[[input$cont_var_option3]])
        
        #Compute summary statistics
        stats_DF = data.frame(group=group,x_q1=quantile(x_vals,0.25,na.rm=TRUE),x_median=quantile(x_vals,0.5,na.rm=TRUE),
                              x_q3=quantile(x_vals,0.75,na.rm=TRUE),stringsAsFactors=FALSE)
        summary_stats_DF = rbind(summary_stats_DF,stats_DF)
      }
      
      #Ensure consistent factor levels for group and numeric group IDs
      if (input$cat_var_option3 == "Age Group") {
        curr_levels = rev(unique(FIFA_option3_filt_DF[[input$cat_var_option3]]))  
      } else if (input$cat_var_option3 == "Position Type"){
        curr_levels = c("Defender","Defensive Midfield","Midfield","Attacking Midfield","Forward")
      } else {
        curr_levels = sort(unique(FIFA_option3_filt_DF[[input$cat_var_option3]])) 
      }
      summary_stats_DF$y_numeric = as.numeric(factor(summary_stats_DF$group,levels=curr_levels))
      
      #Ensure consistent ordering of groups
      summary_stats_DF$group = factor(summary_stats_DF$group,levels=curr_levels)
      top500_DF[[input$cat_var_option3]] = factor(top500_DF[[input$cat_var_option3]],levels=curr_levels)
      
      #Build ridge plot just to extract density info
      ridge_PLOT = ggplot(top500_DF,aes(x=.data[[input$cont_var_option3]],y=.data[[input$cat_var_option3]])) + 
        geom_density_ridges(scale=1.5)
      ridge_DF = ggplot_build(ridge_PLOT)$data[[1]]
      
      #Peak height per group
      ridge_heights_DF = aggregate(ymax~group,ridge_DF,max)
      ridge_heights_DF = rename(ridge_heights_DF,y_numeric=group)
      
      #Merge for annotations based on group labels
      summary_merged_DF = merge(summary_stats_DF,ridge_heights_DF,by="y_numeric",all.x=TRUE)
      
      #Initialize plot with appropriate variables
      curr_ggplot = ggplot(top500_DF,aes(x=.data[[input$cont_var_option3]],y=.data[[input$cat_var_option3]], 
                                          fill=after_stat(x),color=.data[[input$cat_var_option3]])) +
        
        #Add density ridgeline with gradient fill
        geom_density_ridges_gradient(scale=1.5,rel_min_height=0.01) +
        
        #Add vertical lines for KDE-based median, Q1, and Q3
        geom_segment_interactive(data=summary_merged_DF,aes(x=x_median,xend=x_median,y=y_numeric,yend=ymax, 
                                 tooltip=paste0("Median: ", round(x_median,2))),
                                 inherit.aes=FALSE,color= "black",linewidth=0.75) +
        geom_segment_interactive(data=summary_merged_DF,aes(x=x_q1,xend=x_q1, 
                                                               y=y_numeric,yend=ymax, 
                                                               tooltip=paste0("Q1: ",round(x_q1,2))),
                                 inherit.aes=FALSE, color = "#FFA500",linetype = "dashed",linewidth=0.75) +
        geom_segment_interactive(data=summary_merged_DF,aes(x=x_q3,xend=x_q3, 
                                                               y=y_numeric,yend=ymax, 
                                                               tooltip=paste0("Q3: ",round(x_q3,2))),
                                 inherit.aes=FALSE,color="#FF6347",linetype="dashed",linewidth=0.75) +
        
        #Set color scales
        scale_color_viridis_d(option = "C") +
        scale_fill_viridis_c(option = "D") +
        
        #Turn off clipping
        coord_cartesian(clip = "off") +
        
        #Add titles and labels
        labs(title = paste0("Ridgeline Plot of ",input$cont_var_option3," (Top 500 Players)"),
             subtitle = paste0("Comparing Across ", input$cat_var_option3, "s"),
             x = input$cont_var_option3, y = input$cat_var_option3) +
        
        #Set theme
        theme_ipsum(base_family = "sans") +
        
        #Remove legend
        theme(legend.position = "none",
              plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(b = 4)),
              plot.subtitle = element_text(size = 13, color = "gray30", hjust = 0.5),
              axis.title.x = element_text(size = 11, hjust = 0.5),
              axis.title.y = element_text(size = 11, hjust = 0.5),
              axis.text = element_text(size = 9.5)) + 
        
        #Add instructive text
        annotate("text",x=Inf,y=min(as.numeric(factor(top500_DF[[input$cat_var_option3]]))) - 0.5, 
                 label="Hover over vertical lines to see values!",hjust=1,size=3.5,fontface="italic",color="gray30")
      })
    curr_ggplot = reactive_data_option3_FUNC()
    #Use girafe to render the interactive plot
    girafe(ggobj=curr_ggplot,options=list(opts_hover(css="font-weight:bold;fill:#D55E00;"),
                                               opts_tooltip(css="background:#222;color:#fff;padding:5px;border-radius:4px;")))
  }), input$fifa_ed_option3, input$cont_var_option3, input$cat_var_option3)
  
  #Reset graph settings if Reset All Settings button is pressed
  observeEvent(input$reset_inputs_option3, {
    updateSelectInput(session,"cont_var_option3",selected="Overall")
    updateSelectInput(session,"cat_var_option3",selected="Age Group")
    updateSliderInput(session,"fifa_ed_option3",value=max_fifa_ed)
  })
  
  ################### Server Logic For Option 5- GT Summarization Table ##################
  #Observe inputs to make sure specific and any/all options are not selected together
  observe({
    selected=input$country_option5_LIST
    if ("Any Country" %in% selected && length(selected) > 1) {
      if ("Any Country" == selected[length(selected)]) {
        updateSelectInput(session,"country_option5_LIST",selected="Any Country")
      } else if ("Any Country" %in% selected) {
        updateSelectInput(session,"country_option5_LIST",selected=setdiff(selected,"Any Country"))
     }
    }
  })
  
  #Observe inputs to make sure specific and any/all options are not selected together
  observe({
    selected=input$parameters_to_display_option5_LIST
    if ("All Parameters" %in% selected && length(selected) > 1) {
      if ("All Parameters" == selected[length(selected)]) {
        updateSelectInput(session,"parameters_to_display_option5_LIST",selected="All Parameters")
      } else if ("All Parameters" %in% selected) {
        updateSelectInput(session,"parameters_to_display_option5_LIST",selected=setdiff(selected,"All Parameters"))
      }
    }
  })
  
  #Observe inputs to make sure specific and any/all options are not selected together
  observe({
    selected=input$club_option5_LIST
    if ("Any Club" %in% selected && length(selected) > 1) {
      if ("Any Club" == selected[length(selected)]) {
        updateSelectInput(session,"club_option5_LIST",selected="Any Club")
      } else if ("Any Club" %in% selected) {
        updateSelectInput(session,"club_option5_LIST",selected=setdiff(selected,"Any Club"))
      }
    }
  })
  
  #Reset graph settings if Reset All Settings button is pressed
  observeEvent(input$reset_inputs_option5, {
    updateSelectInput(session,"team_position_option5",selected="Any Position")
    updateSelectInput(session,"country_option5_LIST",selected="Any Country")
    updateSelectInput(session,"club_option5_LIST",selected="Any Club")
    updateSelectInput(session,"player_rating_option5",selected="Any Rating")
    updateSliderInput(session,"num_players_option5",value=10)
    updateSliderInput(session,"fifa_ed_option5",value=21)
    updateSelectInput(session,"parameters_to_display_option5_LIST",selected=parameter_option5_default_LIST)
  })
  
  #GT table function
  output$gt_summarization_table_option5_FUNC = bindCache(render_gt({
    reactive_data_option5_FUNC = reactive({
      country_option5_copy_LIST = copy(input$country_option5_LIST)
      club_option5_copy_LIST = copy(input$club_option5_LIST)
      parameters_to_display_option5_copy_LIST = copy(input$parameters_to_display_option5_LIST)
      
      #Copy lists
      if (is.null(country_option5_copy_LIST) || length(country_option5_copy_LIST) == 0) {
        country_option5_copy_LIST = c("Any Country")
      }
      if (is.null(club_option5_copy_LIST) || length(club_option5_copy_LIST) == 0) {
        club_option5_copy_LIST = c("Any Club")
      }
      if (is.null(parameters_to_display_option5_copy_LIST) || length(parameters_to_display_option5_copy_LIST) == 0) {
        parameters_to_display_option5_copy_LIST = parameter_option5_default_LIST
      } 
      if ("All Parameters" %in% parameters_to_display_option5_copy_LIST) {
        parameters_to_display_option5_copy_LIST = parameter_option5_LIST
      }
      
      full_parameters_to_display_LIST = c("Player",parameters_to_display_option5_copy_LIST)
      
      #Filter dataset by selected FIFA edition
      FIFA_option5_filt_DF = FIFA_option5_DF[FIFA_option5_DF$fifa_edition == input$fifa_ed_option5, ]
      
      if (nrow(FIFA_option5_filt_DF) == 0) {
        FIFA_option5_filt_DF = data.frame()
        no_data_message = "No data available for the selected options."
        return(no_data_message)
      } else if (input$team_position_option5 != "Any Position") {
          if (input$team_position_option5 %in% c("Defender","Defensive Midfielder","Midfielder","Attacking Midfielder","Forward")) {
            FIFA_option5_filt_DF = FIFA_option5_filt_DF[FIFA_option5_filt_DF$position_type == input$team_position_option5, ]  
          } else {
            FIFA_option5_filt_DF = FIFA_option5_filt_DF[FIFA_option5_filt_DF$`Position` == input$team_position_option5, ]
          }
      }
      
      if (nrow(FIFA_option5_filt_DF) == 0) {
        FIFA_option5_filt_DF = data.frame()
        no_data_message = "No data available for the selected options."
        return(no_data_message)
      } else if (!("Any Country" %in% country_option5_copy_LIST)) {
          FIFA_option5_filt_DF = FIFA_option5_filt_DF[FIFA_option5_filt_DF$`Country` %in% country_option5_copy_LIST, ]
      }
      
      
      if (nrow(FIFA_option5_filt_DF) == 0) {
        FIFA_option5_filt_DF = data.frame()
        no_data_message = "No data available for the selected options."
        return(no_data_message)
      } else if (!("Any Club" %in% club_option5_copy_LIST)) {
        FIFA_option5_filt_DF = FIFA_option5_filt_DF[FIFA_option5_filt_DF$`Club` %in% club_option5_copy_LIST, ]
      }
      
      
      if (nrow(FIFA_option5_filt_DF) == 0) {
        FIFA_option5_filt_DF = data.frame()
        no_data_message = "No data available for the selected options."
        return(no_data_message)
      } else if (input$player_rating_option5 != "Any Rating") {
        FIFA_option5_filt_DF = FIFA_option5_filt_DF[FIFA_option5_filt_DF$`player_rating` == input$player_rating_option5, ]
      }
      
      FIFA_option5_filt_DF = na.omit(FIFA_option5_filt_DF[, c(full_parameters_to_display_LIST,"country_url")])
      
      FIFA_option5_sorted_DF = arrange(FIFA_option5_filt_DF,desc(FIFA_option5_filt_DF$`Overall`))
      
      top_n = as.numeric(min(input$num_players_option5, nrow(FIFA_option5_sorted_DF)))
      if (top_n > 0) {
        FIFA_option5_top_n_DF = head(FIFA_option5_sorted_DF,top_n)  
        no_data_message = NULL
      } else {
        FIFA_option5_top_n_DF = data.frame()
        no_data_message = "No data available for the selected options."
        return(no_data_message)
      }
      
      attribute_cols_LIST = c("Country","Shooting","Passing","Dribbling","Defending","Pace","Physical")
      names(FIFA_option5_top_n_DF)[names(FIFA_option5_top_n_DF) %in% attribute_cols_LIST] = 
        paste0(attribute_cols_LIST,"_value")
      
      if (input$team_position_option5 == "Any Position") {
        position_title = "Players"
      } else if (input$team_position_option5 %in% c("Defender","Defensive Midfielder","Midfielder","Attacking Midfielder","Forward")) {
        position_title = paste0(input$team_position_option5,"s")
      } else {
        position_title = paste0("Players at ",input$team_position_option5)
      }
      
      if (input$player_rating_option5 == "Any Rating") {
        rating_subtitle = "Any Rating"
      } else {
        rating_subtitle = paste0(input$player_rating_option5," Rating")
      }
      
      extra_subtitle = ""
      if ("Country" %in% parameters_to_display_option5_copy_LIST && !("Any Country" %in% country_option5_copy_LIST)){
        if (length(country_option5_copy_LIST) == 2) {
          extra_subtitle = paste0(trimws(extra_subtitle),", Originating From ")
          extra_subtitle = paste0(extra_subtitle,country_option5_copy_LIST[1]," or ",country_option5_copy_LIST[2])
        } else if (length(country_option5_copy_LIST) == 1) {
          extra_subtitle = paste0(trimws(extra_subtitle),", Originating From ")
          extra_subtitle = paste0(extra_subtitle,country_option5_copy_LIST)
        }
      }
      
      if ("Club" %in% parameters_to_display_option5_copy_LIST && !("Any Club" %in% club_option5_copy_LIST)) {
        if (length(club_option5_copy_LIST) == 2) {
          if (length(extra_subtitle) == 0) {
            extra_subtitle = paste0(trimws(extra_subtitle),"Plays For ")
          } else {
            extra_subtitle = paste0(trimws(extra_subtitle),", Plays For ")
          }
          extra_subtitle = paste0(extra_subtitle,club_option5_copy_LIST[1]," or ",club_option5_copy_LIST[2])
        } else if (length(club_option5_copy_LIST) == 1) {
          if (length(extra_subtitle) == 0) {
            extra_subtitle = paste0(extra_subtitle,"Plays For ")
          } else {
            extra_subtitle = paste0(extra_subtitle,", Plays For ")
          }
          extra_subtitle = paste0(extra_subtitle,club_option5_copy_LIST)
        }
      }
      
      curr_title = paste("Top",input$num_players_option5,position_title)
      curr_subtitle = paste0(rating_subtitle,extra_subtitle)
      
      
      if (nrow(FIFA_option5_top_n_DF) == 0) {
        FIFA_option5_top_n_DF = data.frame()
        no_data_message = "No data available for the selected options."
        return(no_data_message)
      } else {
        for (col in attribute_cols_LIST[2:length(attribute_cols_LIST)]) {
          if (paste0(col,"_value") %in% names(FIFA_option5_top_n_DF)) {
            FIFA_option5_top_n_DF[[col]]=cut(FIFA_option5_top_n_DF[[paste0(col,"_value")]],breaks=c(0,45,50,55,60,65,70,75,78,81,84,Inf),
                                             labels=c("","","⭐","⭐☆","⭐⭐","⭐⭐☆","⭐⭐⭐","⭐⭐⭐☆","⭐⭐⭐⭐","⭐⭐⭐⭐☆","⭐⭐⭐⭐⭐"),
                                             right=TRUE)
          }
        }
      }
      
      if (nrow(FIFA_option5_top_n_DF) == 0) {
        FIFA_option5_top_n_DF = data.frame()
        no_data_message = "No data available for the selected options."
        return(no_data_message)
      } else if (paste0("Country","_value") %in% names(FIFA_option5_top_n_DF)) {
          colnames(FIFA_option5_top_n_DF)[colnames(FIFA_option5_top_n_DF) == "country_url"] = "Country"
      }
      
      FIFA_option5_gt_table_data_DF = FIFA_option5_top_n_DF[, full_parameters_to_display_LIST]

      if (nrow(FIFA_option5_gt_table_data_DF) == 0) {
        no_data_message = "No data available for the selected options."
        return(no_data_message)
      } else{
        FIFA_option5_GT_TABLE = gt(FIFA_option5_gt_table_data_DF)  
      }
      
      if ("Country" %in% names(FIFA_option5_gt_table_data_DF)) {
        FIFA_option5_GT_TABLE = fmt_image(FIFA_option5_GT_TABLE,columns="Country",width=30,height=20)
      }
      
      curr_gradient_palette_LIST = list("Age"=c("#deebf7","#41b6c4","#253494"),"Potential"=c("#e5f5e0","#66c2a4","#006d2c"),
                                        "Market Value (€)"=c("#fff7bc","#fec44f","#d95f0e"))
      for (col in names(curr_gradient_palette_LIST)) {
        if (col %in% names(FIFA_option5_gt_table_data_DF)) {
          FIFA_option5_gt_table_data_DF[[col]] = as.numeric(FIFA_option5_gt_table_data_DF[[col]])
          FIFA_option5_GT_TABLE=data_color(FIFA_option5_GT_TABLE,columns=all_of(col),fn=col_numeric(palette=curr_gradient_palette_LIST[[col]],
                                                                                                        domain=range(FIFA_option5_gt_table_data_DF[[col]],na.rm=TRUE)))
        }
      }
      
      if ("Position" %in% names(FIFA_option5_gt_table_data_DF)) {
        position_colors_LIST = list("Defender"="#1f77b4","Defensive Midfielder"="#2ca02c","Midfielder"="#FFFF99",
                                    "Attacking Midfielder"="#ff7f0e","Forward"="#d62728")
        position_groups_LIST = list("Defender"=c("LWB","LB","LCB","CB","RCB","RB","RWB"),"Defensive Midfielder"=c("LDM","CDM","RDM"),
                                    "Midfielder"=c("LCM","CM","RCM","LM","RM"),"Attacking Midfielder"=c("LAM","CAM","RAM"),
                                    "Forward"=c("LF","CF","RF","LS","ST","RS","LW","RW"))
        position_color_mapping_VECTOR = unlist(lapply(names(position_groups_LIST), function(group) {
          positions = position_groups_LIST[[group]]
          color = position_colors_LIST[[group]]
          setNames(rep(color, length(positions)), positions)
        }))
        FIFA_option5_GT_TABLE = data_color(FIFA_option5_GT_TABLE,columns=Position,
                                           fn=col_factor(palette=position_color_mapping_VECTOR,
                                                             domain=unique(FIFA_option5_gt_table_data_DF$Position)))
      }
      
      if ("Overall" %in% names(FIFA_option5_gt_table_data_DF)) {
        FIFA_option5_GT_TABLE = tab_style(FIFA_option5_GT_TABLE,style=cell_fill(color="#FFD700"),locations=cells_body(columns=c("Overall"),
                                                                                                                      rows=Overall >= 75))
        FIFA_option5_GT_TABLE = tab_style(FIFA_option5_GT_TABLE,style=cell_fill(color="#C0C0C0"),locations=cells_body(columns=c("Overall"),
                                                                                                                      rows=Overall>=65 & Overall<75))
        FIFA_option5_GT_TABLE = tab_style(FIFA_option5_GT_TABLE,style=cell_fill(color="#cd7f32"),locations=cells_body(columns=c("Overall"),
                                                                                                                      rows=Overall<65))
      }
      
      available_cols_LIST = c()
      for (col in c("Age","Country","Overall","Market Value (€)","Potential")) {
        if (col %in% names(FIFA_option5_gt_table_data_DF)) {
          available_cols_LIST = c(available_cols_LIST,col)
        }
      }
      FIFA_option5_GT_TABLE = cols_align(FIFA_option5_GT_TABLE,columns=available_cols_LIST,align="center")
      
      FIFA_option5_GT_TABLE = tab_header(FIFA_option5_GT_TABLE,title=md(curr_title),subtitle=md(curr_subtitle))
      FIFA_option5_GT_TABLE = opt_table_font(FIFA_option5_GT_TABLE,font=google_font("Roboto"))
      
      FIFA_option5_GT_TABLE = tab_options(FIFA_option5_GT_TABLE,heading.title.font.size=24,heading.title.font.weight="bold",
                                          heading.subtitle.font.size=18,table.font.size=16,table.border.top.color="black")
      
      return(FIFA_option5_GT_TABLE)
    })
    reactive_data_option5_FUNC()
  }), input$team_position_option5,input$country_option5_LIST,input$club_option5_LIST,input$player_rating_option5,
      input$num_players_option5,input$parameters_to_display_option5_LIST,input$fifa_ed_option5)
  
  output$rarity_description_table = renderTable({
    data.frame(
      Colour=c("Gold","Silver","Bronze"),
      `Overall Rating`=c("75+","65-74","Below 65"),
      stringsAsFactors=FALSE
    )
  })
  
  output$stars_description_table = renderTable({
    data.frame(
      Stars=c("","☆","⭐","⭐☆","⭐⭐","⭐⭐☆","⭐⭐⭐","⭐⭐⭐☆","⭐⭐⭐⭐","⭐⭐⭐⭐☆","⭐⭐⭐⭐⭐"),
      `Attribute Rating`=c("0-45","46-50","51-55","56-60","61-65","66-70","71-75","76-78","79-81","82-84","85+"),
      stringsAsFactors=FALSE
    )
  })
  ################### Server Logic For Option 6- X ##################
}

#################################### Run application #####################################
#Run the application
shinyApp(ui=ui,server=server)

