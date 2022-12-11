### SHINY UI ###
ui <- bootstrapPage(
  navbarPage(
    theme = shinytheme("flatly"), 
    collapsible = TRUE,
    
    
    HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">EMS tracker</a>'), id="nav",
    windowTitle = "EMS tracker",
    
    
    #EMS 
    tabPanel("EMS mapper",
             div(class="outer",
                 
                 leafletOutput("mymap", width="100%",height="1000px"),
                 
                 absolutePanel(id = "controls", class = "panel panel-default",
                               top = 75, left = 55, width = 250, fixed=TRUE,
                               draggable = TRUE, height = "auto",
                               
                               span(tags$i(h6("Calls for EMS are subject to significant variation in different time periods.")), style="color:#045a8d"),
                               h3(textOutput("reactive_case_count"), align = "right"),
                               h6(textOutput("clean_date_reactive"), align = "right"),
                               plotlyOutput("epi_curve", height="130px", width="100%"),
                               plotlyOutput("cumulative_plot", height="130px", width="100%"),
                               span(("Incase of an Emergency, please call 0711 264000."),align = "left", style = "font-size:80%"),#tags$br(),
                               span(("Kiambu Emergency Operations Center."),align = "left", style = "font-size:80%"),
                               
                               
                               
                               sliderTextInput("plot_date",
                                               label = h5("Select mapping date"),
                                               choices = format(unique(Cumulative$date), "%d %b %y"),
                                               selected = format(current_date, "%d %b %y"),
                                               grid = FALSE,
                                               animate=animationOptions(interval = 3000, loop = FALSE))
                 ),
                 
                 absolutePanel(id = "logo", class = "card", bottom = 20, left = 60, width = 80, fixed=TRUE, draggable = FALSE, height = "auto",
                               tags$a(href='https://kiambu.go.ke/health-services/', tags$img(src='H:\\4.2\\PROJECT\\objective 3\\rshine\\logo.png',height='40',width='80'))),
                 
                 absolutePanel(id = "logo", class = "card", bottom = 20, left = 20, width = 30, fixed=TRUE, draggable = FALSE, height = "auto",
                               actionButton("twitter_share", label = "", icon = icon("twitter"),style='padding:5px',
                                            onclick = sprintf("window.open('%s')", 
                                                              "https://twitter.com/intent/tweet?text=%20@LSHTM_Vaccines%20outbreak%20mapper&url=https://bit.ly/2uBvnds&hashtags=EmergencyMedicalSevices")))
                 
                 
             )
    ),
    
    tabPanel("Monthly plots",
             div(class="outer",
                 
                 leafletOutput("monthly_map", width="100%",height="1000px"),
                 
             )
    ),
    tabPanel("Weekly plots",
             div(class="outer",
                 
                 leafletOutput("weekly_map", width="100%",height="1000px"),
                 
             )
    ),
    
    tabPanel("Hourly plots",
             div(class="outer",
                 
                 leafletOutput("Hourly_map", width="100%",height="1000px"),
                 
             )
    ),
    
    
    tabPanel("Data",
             numericInput("maxrows", "Rows to show", 25),
             verbatimTextOutput("rawtable"),
             downloadButton("downloadCsv", "Download as CSV"),tags$br(),tags$br(),
             "Adapted from data provided by ", tags$a(href="https://kiambu.go.ke/health-services/", 
                                                      "Kiambu County Emergency Operations Center (EOC).")
    ),
    
    tabPanel("About this site",
             tags$div(
               tags$h4("Last update"), 
               
               "This site is will be updated once daily once fully operational.", 
               "The aim is to complement available mapping resources with several interactive features, including the timeline function and the ability to overlay past Emergencies.",
               "It is also meant to share information with the public on emerging emergencies and also serve as a decision support tool for the county government of Kiambu.", 
               "All information on Emergency Medical Services from Kiambu county will be provided on this site, this will help the county government to better mobilze their resouces when providing emergemcy services to the public.", 
               
               
               tags$br(),tags$br(),tags$h4("Background"), 
               "Emergency medical services (EMS), also known as ambulance services or paramedic services, are emergency services that provide urgent pre-hospital treatment and stabilisation for serious illness and injuries and transport to definitive care. 
                        They may also be known as a first aid squad.
                        In most places, the EMS can be summoned by members of the public (as well as medical facilities, other emergency services, businesses and authorities) via an emergency telephone number which puts them in contact with a control facility, which will then dispatch a suitable resource for the situation. 
                        Ambulances are the primary vehicles for delivering EMS, though some also use squad cars, motorcycles, aircraft, or boats.",
               tags$br(),tags$br(),
               "In isolation, these emergencies can be hard to interpret. 
                        How is demand for emergency medical services changing? Are efforts to reduce loss of life due to delayed emergencies working? Are there particular trends in occurance of these emergencies?
                        This site is updated daily based on data provided by Kiambu Emergency operations center. 
                        By looking beyond the headlines, we hope it is possible to get a deeper understanding of this unfolding Emergencies.",
               
               tags$br(),tags$br(),tags$h4("Code"),
               "Code and input data used to generate this Shiny mapping tool are available on ",tags$a(href="https://github.com/Madaga-L/AMBULANCE-LOCATION-PROJECT", "Github."),
               tags$br(),tags$br(),tags$h4("Sources"),
               tags$b("Kiambu Emergency Operations Center: "), tags$a(href="https://kiambu.go.ke/health-services/", "Kiambu county, department of Health services,"),
               
               tags$br(),tags$br(),tags$h4("Authors"),
               "Lavinia Mmbaga Madaga, Student, GEGIS, JKUAT",tags$br(),
               
               tags$br(),tags$br(),tags$h4("Contact"),
               "lavinia.mmbaga@students.jkuat.ac,ke",tags$br(),tags$br(),
               
               tags$br(),tags$br(),tags$h4("Kiambu county emergency contact number"),
               "+254 709 877 000",tags$br(),tags$br(),
             )
    )
  )
)