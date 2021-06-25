
tabPanel( includeHTML("joint_lab_separate/0.example_data/settings.html"),
          fluidRow(   column(12,h2("1.Select starting state and covariate patterns"))),
          uiOutput("pageinput1"),
          fluidRow(   column(12,h2("2.Naming states and transitions and extra options"))),
          uiOutput("pageinput2")
)

