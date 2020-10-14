library(tidyverse); library(ggplot2); library(Rmisc)

# -------------------------------------------------------------------- #

# -------------------------------------------------------------------- #
# Shiny app
library(shiny)

UI <- fluidPage(
  # App title ----
    titlePanel("School Reopening under COVID-19"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      selectInput(inputId = "Intervention", 
                  label = "Choose an Intervention:",
                  choices = c("No Intervention" = '1', 
                              "Intervention: Bubble System" = '2', 
                              "Intervention: Bubble System & Tutor Group" = '3',
                              "Intervention: Bubble System & Variation in School Opening Time" = '4',
                              "Intervention: Bubble System & Tutor Group & Variation in School Opening Time" = '5')
                  ),
      
      # Only show these panel if the checkbox input is a No Intervention:
      conditionalPanel(
        condition = "input.Intervention == '1'",
        sliderInput(inputId = "noStudents1",
                    label = "Number of Students:",
                    min = 0,
                    max = 1000,
                    value = 100),
        
        sliderInput(inputId = "noStaffs1",
                    label = "Number of Staffs:",
                    min = 0,
                    max = 500,
                    value = 60),
        
        sliderInput(inputId = "C.StudentStudent1",
                    label = "Number of Students a Student may contact with:",
                    min = 0,
                    max = 50,
                    value = 20),
        
        sliderInput(inputId = "C.StudentStaff1",
                    label = "Number of Staffs a Student may contact with:",
                    min = 0,
                    max = 50,
                    value = 3),
        
        sliderInput(inputId = "C.StaffStudent1",
                    label = "Number of Students a Staff may contact with:",
                    min = 0,
                    max = 50,
                    value = 20),
        
        sliderInput(inputId = "C.StaffStaff1",
                    label = "Number of Staffs a Staff may contact with:",
                    min = 0,
                    max = 50,
                    value = 20),
        
        sliderInput(inputId = "TransStudent1",
                    label = "Transmissibility of an infected Student:",
                    min = 0,
                    max = 1,
                    value = 0.64),
        
        sliderInput(inputId = "TransStaff1",
                    label = "Transmissibility of an infected Staff:",
                    min = 0,
                    max = 1,
                    value = 1),
        
        sliderInput(inputId = "SuscepStudent1",
                    label = "Susceptibility of a susceptible Student:",
                    min = 0,
                    max = 1,
                    value = 0.79),
        
        sliderInput(inputId = "SuscepStaff1",
                    label = "Susceptibility of a susceptible Staff:",
                    min = 0,
                    max = 1,
                    value = 1),
        
        sliderInput(inputId = "PercentSympStudent1",
                    label = "If a Student is infected, the Percentage of being Symptomatic:",
                    min = 0,
                    max = 1,
                    value = 0.1),
        
        sliderInput(inputId = "PercentSympStaff1",
                    label = "If a Staff is infected, the Percentage of being Symptomatic:",
                    min = 0,
                    max = 1,
                    value = 0.3),
        sliderInput(inputId = "LatenttoInfection1",
                    label = "The rate of from Latent to Infection per day:",
                    min = 0,
                    max = 1,
                    value = 0.2),
        
        sliderInput(inputId = "TransmissionAsymptoSymp1",
                    label = "The Relative Transmission for Asymptomatic compared to Symptomatic:",
                    min = 0,
                    max = 1,
                    value = 0.138),
        
        sliderInput(inputId = "InfectiontoQuanrantine1",
                    label = "The Rate of an Infection getting Quanrantined per day:",
                    min = 0,
                    max = 1,
                    value = 0.75),
        
        sliderInput(inputId = "Recovery1",
                    label = "The Rate of Recovery per day:",
                    min = 0,
                    max = 1,
                    value = 0.1)
        ),
      
      conditionalPanel(
        condition = "input.Intervention == '2' | input.Intervention == '3'",
        sliderInput(inputId = "BubbleStudents2",
                    label = "Number of Student in each Bubble:",
                    min = 0,
                    max = 20,
                    value = 10),
        
        sliderInput(inputId = "BubbleStaff2",
                    label = "Number of Staff in each Bubble:",
                    min = 0,
                    max = 20,
                    value = 3),
        
        sliderInput(inputId = "NoBubbles2",
                    label = "Number of Bubbles:",
                    min = 0,
                    max = 20,
                    value = 10),
        
        sliderInput(inputId = "NoOutStudentsContact2",
                    label = "Number of outside bubble Students a Student may contact with:",
                    min = 0,
                    max = 20,
                    value =3),
        
        sliderInput(inputId = "NoOutStaffsContact2",
                    label = "Number of outside bubble Staffs a Staff may contact with:",
                    min = 0,
                    max = 20,
                    value = 3),
        
        sliderInput(inputId = "TransStudent2",
                    label = "Transmissibility of an infected Student:",
                    min = 0,
                    max = 1,
                    value = 0.64),
        
        sliderInput(inputId = "TransStaff2",
                    label = "Transmissibility of an infected Staff:",
                    min = 0,
                    max = 1,
                    value = 1),
        
        sliderInput(inputId = "SuscepStudent2",
                    label = "Susceptibility of a susceptible Student:",
                    min = 0,
                    max = 1,
                    value = 0.79),
        
        sliderInput(inputId = "SuscepStaff2",
                    label = "Susceptibility of a susceptible Staff:",
                    min = 0,
                    max = 1,
                    value = 1),
        
        sliderInput(inputId = "TransIn2",
                    label = "Transmissibility Inside a Bubble:",
                    min = 0,
                    max = 1,
                    value = 0.54),
        
        sliderInput(inputId = "TransOut2",
                    label = "Transmissibility Outside a Bubble:",
                    min = 0,
                    max = 1,
                    value = 0.54/2),
        
        sliderInput(inputId = "PercentSympStudent2",
                    label = "If a Student is infected, the Percentage of being Symptomatic:",
                    min = 0,
                    max = 1,
                    value = 0.1),
        
        sliderInput(inputId = "PercentSympStaff2",
                    label = "If a Staff is infected, the Percentage of being Symptomatic:",
                    min = 0,
                    max = 1,
                    value = 0.3),
        
        sliderInput(inputId = "LatenttoInfection2",
                    label = "The rate of from Latent to Infection per day:",
                    min = 0,
                    max = 1,
                    value = 0.2),
        
        sliderInput(inputId = "TransmissionAsymptoSymp2",
                    label = "The Relative Transmission for Asymptomatic compared to Symptomatic:",
                    min = 0,
                    max = 1,
                    value = 0.138),
        
        sliderInput(inputId = "InfectiontoQuanrantine2",
                    label = "The Rate of an Infection  getting Quanrantined per day:",
                    min = 0,
                    max = 1,
                    value = 0.75),
        
        sliderInput(inputId = "Recovery2",
                    label = "The Rate of Recovery per day:",
                    min = 0,
                    max = 1,
                    value = 0.1)
        ),
      
      conditionalPanel(
        condition = "input.Intervention == '4' | input.Intervention == '5'",
        
        selectInput(inputId = "Openingdays", 
                    label = "School Opening Day of Week:",
                    choices = c("All Weekdays: Monday to Friday" = '1', 
                                "Long Weekend: Monday to Thurday" = '2', 
                                "1 day on, 1 day off: Monday, Wednesday and Friday" = '3',
                                "2 days on, 2 days off: Monday, Tuesday and Friday" = '4')
                    ),
        
        sliderInput(inputId = "BubbleStudents4",
                    label = "Number of Student in each Bubble:",
                    min = 0,
                    max = 20,
                    value = 10),
        
        sliderInput(inputId = "BubbleStaff4",
                    label = "Number of Staff in each Bubble:",
                    min = 0,
                    max = 20,
                    value = 3),
        
        sliderInput(inputId = "NoBubbles4",
                    label = "Number of Bubbles:",
                    min = 0,
                    max = 20,
                    value = 10),
        
        sliderInput(inputId = "NoOutStudentsContact4",
                    label = "Number of outside bubble Students a Student may contact with:",
                    min = 0,
                    max = 20,
                    value =3),
        
        sliderInput(inputId = "NoOutStaffsContact4",
                    label = "Number of outside bubble Staffs a Staff may contact with:",
                    min = 0,
                    max = 20,
                    value = 3),
        
        sliderInput(inputId = "TransStudent4",
                    label = "Transmissibility of an infected Student:",
                    min = 0,
                    max = 1,
                    value = 0.64),
        
        sliderInput(inputId = "TransStaff4",
                    label = "Transmissibility of an infected Staff:",
                    min = 0,
                    max = 1,
                    value = 1),
        
        sliderInput(inputId = "SuscepStudent4",
                    label = "Susceptibility of a susceptible Student:",
                    min = 0,
                    max = 1,
                    value = 0.79),
        
        sliderInput(inputId = "SuscepStaff4",
                    label = "Susceptibility of a susceptible Staff:",
                    min = 0,
                    max = 1,
                    value = 1),
        
        sliderInput(inputId = "TransIn4",
                    label = "Transmissibility Inside a Bubble:",
                    min = 0,
                    max = 1,
                    value = 0.54),
        
        sliderInput(inputId = "TransOut4",
                    label = "Transmissibility Outside a Bubble:",
                    min = 0,
                    max = 1,
                    value = 0.54/2),
        
        sliderInput(inputId = "PercentSympStudent4",
                    label = "If a Student is infected, the Percentage of being Symptomatic:",
                    min = 0,
                    max = 1,
                    value = 0.1),
        
        sliderInput(inputId = "PercentSympStaff4",
                    label = "If a Staff is infected, the Percentage of being Symptomatic:",
                    min = 0,
                    max = 1,
                    value = 0.3),
        
        sliderInput(inputId = "LatenttoInfection4",
                    label = "The rate of from Latent to Infection per day:",
                    min = 0,
                    max = 1,
                    value = 0.2),
        
        sliderInput(inputId = "TransmissionAsymptoSymp4",
                    label = "The Relative Transmission for Asymptomatic compared to Symptomatic:",
                    min = 0,
                    max = 1,
                    value = 0.138),
        
        sliderInput(inputId = "InfectiontoQuanrantine4",
                    label = "The Rate of an Infection  getting Quanrantined per day:",
                    min = 0,
                    max = 1,
                    value = 0.75),
        
        sliderInput(inputId = "Recovery4",
                    label = "The Rate of Recovery per day:",
                    min = 0,
                    max = 1,
                    value = 0.1)
      ),
      helpText("Note: ")
    ),
    
    mainPanel(
      h4("SEDUQR Plots - Students and Staffs"),
      fluidRow(
        column(6,plotOutput(outputId = "p_Student", width="400px", height = "500px")),
        column(6,plotOutput(outputId = "p_Staff", width="400px", height = "500px"))),
        
      h4("Rt plots - Students and Staffs"),
      fluidRow(
        column(10,plotOutput(outputId = "p_Rt")))
      )
    )
  )


# Define server logic required to summarize and view the selected dataset
SERVER = function(input, output) {
  
  output$p_Student <-renderPlot({
    
    if (input$Intervention == '1') {
      
      myfun1(N.Staff = input$noStaffs1, N.Student = input$noStudents1, 
                      C.StudentStudent = input$C.StudentStudent1, C.StudentStaff = input$C.StudentStaff1, 
                      C.StaffStudent = input$C.StaffStudent1, C.StaffStaff = input$C.StaffStaff1,
                      Trans.Staff = input$TransStaff1, Trans.Student = input$TransStudent1, # transmissibility of student / staff
                      Suscep.Staff = input$SuscepStaff1, Suscep.Student = input$SuscepStudent1, # susceptibility of student / staff
                      detect.Staff = input$PercentSympStaff1, detect.Student = input$PercentSympStudent1, # if a student / staff is infected, the probablity that he is symptomatic
                      a = input$LatenttoInfection1, e = input$TransmissionAsymptoSymp1, o = input$InfectiontoQuanrantine1, y = input$Recovery1)[1]
    
      }
    
    else if (input$Intervention == '2') {
      
      myfun2(B.Staff = input$BubbleStaff2, B.Student = input$BubbleStudents2, bubble.no = input$NoBubbles2, 
                         BBC.Staff = input$NoOutStaffsContact2, BBC.Student = input$NoOutStudentsContact2, # Between bubbles contact number for each student / staff
                         Trans.Staff = input$TransStaff2, Trans.Student = input$TransStudent2, # transmissibility of student / staff
                         Suscep.Staff = input$SuscepStaff2, Suscep.Student = input$SuscepStudent2, # susceptibility of student / staff
                         Trans.In = input$TransIn2, Trans.Out = input$TransOut2, # transmission rate inside / outside the bubble
                         detect.Staff = input$PercentSympStaff2, detect.Student = input$PercentSympStudent2, # if a student / staff is infected, the probablity that he is symptomatic
                         a = input$LatenttoInfection2, e = input$TransmissionAsymptoSymp2, o = input$InfectiontoQuanrantine2, y = input$Recovery2)[1]
      
      }
    
    else if (input$Intervention == '3') {
      
      myfun3(B.Staff = input$BubbleStaff2, B.Student = input$BubbleStudents2, bubble.no = input$NoBubbles2, 
                      BBC.Staff = input$NoOutStaffsContact2, BBC.Student = input$NoOutStudentsContact2, # Between bubbles contact number for each student / staff
                      Trans.Staff = input$TransStaff2, Trans.Student = input$TransStudent2, # transmissibility of student / staff
                      Suscep.Staff = input$SuscepStaff2, Suscep.Student = input$SuscepStudent2, # susceptibility of student / staff
                      Trans.In = input$TransIn2, Trans.Out = input$TransOut2, # transmission rate inside / outside the bubble
                      detect.Staff = input$PercentSympStaff2, detect.Student = input$PercentSympStudent2, # if a student / staff is infected, the probablity that he is symptomatic
                      a = input$LatenttoInfection2, e = input$TransmissionAsymptoSymp2, o = input$InfectiontoQuanrantine2, y = input$Recovery2)[1]
      
    }
    
    else if (input$Intervention == '4' & input$Openingdays == '1') {
      
      myfun4.1(B.Staff = input$BubbleStaff4, B.Student = input$BubbleStudents4, bubble.no = input$NoBubbles4, 
                      BBC.Staff = input$NoOutStaffsContact4, BBC.Student = input$NoOutStudentsContact4, # Between bubbles contact number for each student / staff
                      Trans.Staff = input$TransStaff4, Trans.Student = input$TransStudent4, # transmissibility of student / staff
                      Suscep.Staff = input$SuscepStaff4, Suscep.Student = input$SuscepStudent4, # susceptibility of student / staff
                      Trans.In = input$TransIn4, Trans.Out = input$TransOut4, # transmission rate inside / outside the bubble
                      detect.Staff = input$PercentSympStaff4, detect.Student = input$PercentSympStudent4, # if a student / staff is infected, the probablity that he is symptomatic
                      a = input$LatenttoInfection4, e = input$TransmissionAsymptoSymp4, o = input$InfectiontoQuanrantine4, y = input$Recovery4)[1]
      
    }
    
    else if (input$Intervention == '4' & input$Openingdays == '2') {
      
      myfun4.2(B.Staff = input$BubbleStaff4, B.Student = input$BubbleStudents4, bubble.no = input$NoBubbles4, 
                        BBC.Staff = input$NoOutStaffsContact4, BBC.Student = input$NoOutStudentsContact4, # Between bubbles contact number for each student / staff
                        Trans.Staff = input$TransStaff4, Trans.Student = input$TransStudent4, # transmissibility of student / staff
                        Suscep.Staff = input$SuscepStaff4, Suscep.Student = input$SuscepStudent4, # susceptibility of student / staff
                        Trans.In = input$TransIn4, Trans.Out = input$TransOut4, # transmission rate inside / outside the bubble
                        detect.Staff = input$PercentSympStaff4, detect.Student = input$PercentSympStudent4, # if a student / staff is infected, the probablity that he is symptomatic
                        a = input$LatenttoInfection4, e = input$TransmissionAsymptoSymp4, o = input$InfectiontoQuanrantine4, y = input$Recovery4)[1]
      
    }

    else if (input$Intervention == '4' & input$Openingdays == '3') {
      
      myfun4.3(B.Staff = input$BubbleStaff4, B.Student = input$BubbleStudents4, bubble.no = input$NoBubbles4, 
                        BBC.Staff = input$NoOutStaffsContact4, BBC.Student = input$NoOutStudentsContact4, # Between bubbles contact number for each student / staff
                        Trans.Staff = input$TransStaff4, Trans.Student = input$TransStudent4, # transmissibility of student / staff
                        Suscep.Staff = input$SuscepStaff4, Suscep.Student = input$SuscepStudent4, # susceptibility of student / staff
                        Trans.In = input$TransIn4, Trans.Out = input$TransOut4, # transmission rate inside / outside the bubble
                        detect.Staff = input$PercentSympStaff4, detect.Student = input$PercentSympStudent4, # if a student / staff is infected, the probablity that he is symptomatic
                        a = input$LatenttoInfection4, e = input$TransmissionAsymptoSymp4, o = input$InfectiontoQuanrantine4, y = input$Recovery4)[1]
      
    }
    
    else if (input$Intervention == '4' & input$Openingdays == '4') {
      
      myfun4.4(B.Staff = input$BubbleStaff4, B.Student = input$BubbleStudents4, bubble.no = input$NoBubbles4, 
                        BBC.Staff = input$NoOutStaffsContact4, BBC.Student = input$NoOutStudentsContact4, # Between bubbles contact number for each student / staff
                        Trans.Staff = input$TransStaff4, Trans.Student = input$TransStudent4, # transmissibility of student / staff
                        Suscep.Staff = input$SuscepStaff4, Suscep.Student = input$SuscepStudent4, # susceptibility of student / staff
                        Trans.In = input$TransIn4, Trans.Out = input$TransOut4, # transmission rate inside / outside the bubble
                        detect.Staff = input$PercentSympStaff4, detect.Student = input$PercentSympStudent4, # if a student / staff is infected, the probablity that he is symptomatic
                        a = input$LatenttoInfection4, e = input$TransmissionAsymptoSymp4, o = input$InfectiontoQuanrantine4, y = input$Recovery4)[1]
      
    }
    
    else if (input$Intervention == '5' & input$Openingdays == '1') {
      
      myfun5.1(B.Staff = input$BubbleStaff4, B.Student = input$BubbleStudents4, bubble.no = input$NoBubbles4, 
                        BBC.Staff = input$NoOutStaffsContact4, BBC.Student = input$NoOutStudentsContact4, # Between bubbles contact number for each student / staff
                        Trans.Staff = input$TransStaff4, Trans.Student = input$TransStudent4, # transmissibility of student / staff
                        Suscep.Staff = input$SuscepStaff4, Suscep.Student = input$SuscepStudent4, # susceptibility of student / staff
                        Trans.In = input$TransIn4, Trans.Out = input$TransOut4, # transmission rate inside / outside the bubble
                        detect.Staff = input$PercentSympStaff4, detect.Student = input$PercentSympStudent4, # if a student / staff is infected, the probablity that he is symptomatic
                        a = input$LatenttoInfection4, e = input$TransmissionAsymptoSymp4, o = input$InfectiontoQuanrantine4, y = input$Recovery4)[1]
      
    }
    
    else if (input$Intervention == '5' & input$Openingdays == '2') {
      
      myfun5.2(B.Staff = input$BubbleStaff4, B.Student = input$BubbleStudents4, bubble.no = input$NoBubbles4, 
                        BBC.Staff = input$NoOutStaffsContact4, BBC.Student = input$NoOutStudentsContact4, # Between bubbles contact number for each student / staff
                        Trans.Staff = input$TransStaff4, Trans.Student = input$TransStudent4, # transmissibility of student / staff
                        Suscep.Staff = input$SuscepStaff4, Suscep.Student = input$SuscepStudent4, # susceptibility of student / staff
                        Trans.In = input$TransIn4, Trans.Out = input$TransOut4, # transmission rate inside / outside the bubble
                        detect.Staff = input$PercentSympStaff4, detect.Student = input$PercentSympStudent4, # if a student / staff is infected, the probablity that he is symptomatic
                        a = input$LatenttoInfection4, e = input$TransmissionAsymptoSymp4, o = input$InfectiontoQuanrantine4, y = input$Recovery4)[1]
      
    }
    
    else if (input$Intervention == '5' & input$Openingdays == '3') {
      
      myfun5.3(B.Staff = input$BubbleStaff4, B.Student = input$BubbleStudents4, bubble.no = input$NoBubbles4, 
                        BBC.Staff = input$NoOutStaffsContact4, BBC.Student = input$NoOutStudentsContact4, # Between bubbles contact number for each student / staff
                        Trans.Staff = input$TransStaff4, Trans.Student = input$TransStudent4, # transmissibility of student / staff
                        Suscep.Staff = input$SuscepStaff4, Suscep.Student = input$SuscepStudent4, # susceptibility of student / staff
                        Trans.In = input$TransIn4, Trans.Out = input$TransOut4, # transmission rate inside / outside the bubble
                        detect.Staff = input$PercentSympStaff4, detect.Student = input$PercentSympStudent4, # if a student / staff is infected, the probablity that he is symptomatic
                        a = input$LatenttoInfection4, e = input$TransmissionAsymptoSymp4, o = input$InfectiontoQuanrantine4, y = input$Recovery4)[1]
      
    }
    
    else {
      
      myfun5.4(B.Staff = input$BubbleStaff4, B.Student = input$BubbleStudents4, bubble.no = input$NoBubbles4, 
                        BBC.Staff = input$NoOutStaffsContact4, BBC.Student = input$NoOutStudentsContact4, # Between bubbles contact number for each student / staff
                        Trans.Staff = input$TransStaff4, Trans.Student = input$TransStudent4, # transmissibility of student / staff
                        Suscep.Staff = input$SuscepStaff4, Suscep.Student = input$SuscepStudent4, # susceptibility of student / staff
                        Trans.In = input$TransIn4, Trans.Out = input$TransOut4, # transmission rate inside / outside the bubble
                        detect.Staff = input$PercentSympStaff4, detect.Student = input$PercentSympStudent4, # if a student / staff is infected, the probablity that he is symptomatic
                        a = input$LatenttoInfection4, e = input$TransmissionAsymptoSymp4, o = input$InfectiontoQuanrantine4, y = input$Recovery4)[1]
      
    }
    
  })
  
  output$p_Staff <-renderPlot({
    
    if (input$Intervention == '1') {
      
      myfun1(N.Staff = input$noStaffs1, N.Student = input$noStudents1, 
                      C.StudentStudent = input$C.StudentStudent1, C.StudentStaff = input$C.StudentStaff1, 
                      C.StaffStudent = input$C.StaffStudent1, C.StaffStaff = input$C.StaffStaff1,
                      Trans.Staff = input$TransStaff1, Trans.Student = input$TransStudent1, # transmissibility of student / staff
                      Suscep.Staff = input$SuscepStaff1, Suscep.Student = input$SuscepStudent1, # susceptibility of student / staff
                      detect.Staff = input$PercentSympStaff1, detect.Student = input$PercentSympStudent1, # if a student / staff is infected, the probablity that he is symptomatic
                      a = input$LatenttoInfection1, e = input$TransmissionAsymptoSymp1, o = input$InfectiontoQuanrantine1, y = input$Recovery1)[2]
      
    }
    
    else if (input$Intervention == '2') {
      
      myfun2(B.Staff = input$BubbleStaff2, B.Student = input$BubbleStudents2, bubble.no = input$NoBubbles2, 
                         BBC.Staff = input$NoOutStaffsContact2, BBC.Student = input$NoOutStudentsContact2, # Between bubbles contact number for each student / staff
                         Trans.Staff = input$TransStaff2, Trans.Student = input$TransStudent2, # transmissibility of student / staff
                         Suscep.Staff = input$SuscepStaff2, Suscep.Student = input$SuscepStudent2, # susceptibility of student / staff
                         Trans.In = input$TransIn2, Trans.Out = input$TransOut2, # transmission rate inside / outside the bubble
                         detect.Staff = input$PercentSympStaff2, detect.Student = input$PercentSympStudent2, # if a student / staff is infected, the probablity that he is symptomatic
                         a = input$LatenttoInfection2, e = input$TransmissionAsymptoSymp2, o = input$InfectiontoQuanrantine2, y = input$Recovery2)[2]
      
    }
    
    else if (input$Intervention == '3') {
      
      myfun3(B.Staff = input$BubbleStaff2, B.Student = input$BubbleStudents2, bubble.no = input$NoBubbles2, 
                      BBC.Staff = input$NoOutStaffsContact2, BBC.Student = input$NoOutStudentsContact2, # Between bubbles contact number for each student / staff
                      Trans.Staff = input$TransStaff2, Trans.Student = input$TransStudent2, # transmissibility of student / staff
                      Suscep.Staff = input$SuscepStaff2, Suscep.Student = input$SuscepStudent2, # susceptibility of student / staff
                      Trans.In = input$TransIn2, Trans.Out = input$TransOut2, # transmission rate inside / outside the bubble
                      detect.Staff = input$PercentSympStaff2, detect.Student = input$PercentSympStudent2, # if a student / staff is infected, the probablity that he is symptomatic
                      a = input$LatenttoInfection2, e = input$TransmissionAsymptoSymp2, o = input$InfectiontoQuanrantine2, y = input$Recovery2)[2]
      
    }
    
    else if (input$Intervention == '4' & input$Openingdays == '1') {
      
      myfun4.1(B.Staff = input$BubbleStaff4, B.Student = input$BubbleStudents4, bubble.no = input$NoBubbles4, 
                        BBC.Staff = input$NoOutStaffsContact4, BBC.Student = input$NoOutStudentsContact4, # Between bubbles contact number for each student / staff
                        Trans.Staff = input$TransStaff4, Trans.Student = input$TransStudent4, # transmissibility of student / staff
                        Suscep.Staff = input$SuscepStaff4, Suscep.Student = input$SuscepStudent4, # susceptibility of student / staff
                        Trans.In = input$TransIn4, Trans.Out = input$TransOut4, # transmission rate inside / outside the bubble
                        detect.Staff = input$PercentSympStaff4, detect.Student = input$PercentSympStudent4, # if a student / staff is infected, the probablity that he is symptomatic
                        a = input$LatenttoInfection4, e = input$TransmissionAsymptoSymp4, o = input$InfectiontoQuanrantine4, y = input$Recovery4)[2]
      
    }
    
    else if (input$Intervention == '4' & input$Openingdays == '2') {
      
      myfun4.2(B.Staff = input$BubbleStaff4, B.Student = input$BubbleStudents4, bubble.no = input$NoBubbles4, 
                        BBC.Staff = input$NoOutStaffsContact4, BBC.Student = input$NoOutStudentsContact4, # Between bubbles contact number for each student / staff
                        Trans.Staff = input$TransStaff4, Trans.Student = input$TransStudent4, # transmissibility of student / staff
                        Suscep.Staff = input$SuscepStaff4, Suscep.Student = input$SuscepStudent4, # susceptibility of student / staff
                        Trans.In = input$TransIn4, Trans.Out = input$TransOut4, # transmission rate inside / outside the bubble
                        detect.Staff = input$PercentSympStaff4, detect.Student = input$PercentSympStudent4, # if a student / staff is infected, the probablity that he is symptomatic
                        a = input$LatenttoInfection4, e = input$TransmissionAsymptoSymp4, o = input$InfectiontoQuanrantine4, y = input$Recovery4)[2]
      
    }
    
    else if (input$Intervention == '4' & input$Openingdays == '3') {
      
      myfun4.3(B.Staff = input$BubbleStaff4, B.Student = input$BubbleStudents4, bubble.no = input$NoBubbles4, 
                        BBC.Staff = input$NoOutStaffsContact4, BBC.Student = input$NoOutStudentsContact4, # Between bubbles contact number for each student / staff
                        Trans.Staff = input$TransStaff4, Trans.Student = input$TransStudent4, # transmissibility of student / staff
                        Suscep.Staff = input$SuscepStaff4, Suscep.Student = input$SuscepStudent4, # susceptibility of student / staff
                        Trans.In = input$TransIn4, Trans.Out = input$TransOut4, # transmission rate inside / outside the bubble
                        detect.Staff = input$PercentSympStaff4, detect.Student = input$PercentSympStudent4, # if a student / staff is infected, the probablity that he is symptomatic
                        a = input$LatenttoInfection4, e = input$TransmissionAsymptoSymp4, o = input$InfectiontoQuanrantine4, y = input$Recovery4)[2]
      
    }
    
    else if (input$Intervention == '4' & input$Openingdays == '4') {
      
      myfun4.4(B.Staff = input$BubbleStaff4, B.Student = input$BubbleStudents4, bubble.no = input$NoBubbles4, 
                        BBC.Staff = input$NoOutStaffsContact4, BBC.Student = input$NoOutStudentsContact4, # Between bubbles contact number for each student / staff
                        Trans.Staff = input$TransStaff4, Trans.Student = input$TransStudent4, # transmissibility of student / staff
                        Suscep.Staff = input$SuscepStaff4, Suscep.Student = input$SuscepStudent4, # susceptibility of student / staff
                        Trans.In = input$TransIn4, Trans.Out = input$TransOut4, # transmission rate inside / outside the bubble
                        detect.Staff = input$PercentSympStaff4, detect.Student = input$PercentSympStudent4, # if a student / staff is infected, the probablity that he is symptomatic
                        a = input$LatenttoInfection4, e = input$TransmissionAsymptoSymp4, o = input$InfectiontoQuanrantine4, y = input$Recovery4)[2]
      
    }
    
    else if (input$Intervention == '5' & input$Openingdays == '1') {
      
      myfun5.1(B.Staff = input$BubbleStaff4, B.Student = input$BubbleStudents4, bubble.no = input$NoBubbles4, 
                        BBC.Staff = input$NoOutStaffsContact4, BBC.Student = input$NoOutStudentsContact4, # Between bubbles contact number for each student / staff
                        Trans.Staff = input$TransStaff4, Trans.Student = input$TransStudent4, # transmissibility of student / staff
                        Suscep.Staff = input$SuscepStaff4, Suscep.Student = input$SuscepStudent4, # susceptibility of student / staff
                        Trans.In = input$TransIn4, Trans.Out = input$TransOut4, # transmission rate inside / outside the bubble
                        detect.Staff = input$PercentSympStaff4, detect.Student = input$PercentSympStudent4, # if a student / staff is infected, the probablity that he is symptomatic
                        a = input$LatenttoInfection4, e = input$TransmissionAsymptoSymp4, o = input$InfectiontoQuanrantine4, y = input$Recovery4)[2]
      
    }
    
    else if (input$Intervention == '5' & input$Openingdays == '2') {
      
      myfun5.2(B.Staff = input$BubbleStaff4, B.Student = input$BubbleStudents4, bubble.no = input$NoBubbles4, 
                        BBC.Staff = input$NoOutStaffsContact4, BBC.Student = input$NoOutStudentsContact4, # Between bubbles contact number for each student / staff
                        Trans.Staff = input$TransStaff4, Trans.Student = input$TransStudent4, # transmissibility of student / staff
                        Suscep.Staff = input$SuscepStaff4, Suscep.Student = input$SuscepStudent4, # susceptibility of student / staff
                        Trans.In = input$TransIn4, Trans.Out = input$TransOut4, # transmission rate inside / outside the bubble
                        detect.Staff = input$PercentSympStaff4, detect.Student = input$PercentSympStudent4, # if a student / staff is infected, the probablity that he is symptomatic
                        a = input$LatenttoInfection4, e = input$TransmissionAsymptoSymp4, o = input$InfectiontoQuanrantine4, y = input$Recovery4)[2]
      
    }
    
    else if (input$Intervention == '5' & input$Openingdays == '3') {
      
      myfun5.3(B.Staff = input$BubbleStaff4, B.Student = input$BubbleStudents4, bubble.no = input$NoBubbles4, 
                        BBC.Staff = input$NoOutStaffsContact4, BBC.Student = input$NoOutStudentsContact4, # Between bubbles contact number for each student / staff
                        Trans.Staff = input$TransStaff4, Trans.Student = input$TransStudent4, # transmissibility of student / staff
                        Suscep.Staff = input$SuscepStaff4, Suscep.Student = input$SuscepStudent4, # susceptibility of student / staff
                        Trans.In = input$TransIn4, Trans.Out = input$TransOut4, # transmission rate inside / outside the bubble
                        detect.Staff = input$PercentSympStaff4, detect.Student = input$PercentSympStudent4, # if a student / staff is infected, the probablity that he is symptomatic
                        a = input$LatenttoInfection4, e = input$TransmissionAsymptoSymp4, o = input$InfectiontoQuanrantine4, y = input$Recovery4)[2]
      
    }
    
    else {
      
      myfun5.4(B.Staff = input$BubbleStaff4, B.Student = input$BubbleStudents4, bubble.no = input$NoBubbles4, 
                        BBC.Staff = input$NoOutStaffsContact4, BBC.Student = input$NoOutStudentsContact4, # Between bubbles contact number for each student / staff
                        Trans.Staff = input$TransStaff4, Trans.Student = input$TransStudent4, # transmissibility of student / staff
                        Suscep.Staff = input$SuscepStaff4, Suscep.Student = input$SuscepStudent4, # susceptibility of student / staff
                        Trans.In = input$TransIn4, Trans.Out = input$TransOut4, # transmission rate inside / outside the bubble
                        detect.Staff = input$PercentSympStaff4, detect.Student = input$PercentSympStudent4, # if a student / staff is infected, the probablity that he is symptomatic
                        a = input$LatenttoInfection4, e = input$TransmissionAsymptoSymp4, o = input$InfectiontoQuanrantine4, y = input$Recovery4)[2]
      
    }
    
  })
  
  output$p_Rt <-renderPlot({
    
    if (input$Intervention == '1') {
      
      myfun1(N.Staff = input$noStaffs1, N.Student = input$noStudents1, 
                      C.StudentStudent = input$C.StudentStudent1, C.StudentStaff = input$C.StudentStaff1, 
                      C.StaffStudent = input$C.StaffStudent1, C.StaffStaff = input$C.StaffStaff1,
                      Trans.Staff = input$TransStaff1, Trans.Student = input$TransStudent1, # transmissibility of student / staff
                      Suscep.Staff = input$SuscepStaff1, Suscep.Student = input$SuscepStudent1, # susceptibility of student / staff
                      detect.Staff = input$PercentSympStaff1, detect.Student = input$PercentSympStudent1, # if a student / staff is infected, the probablity that he is symptomatic
                      a = input$LatenttoInfection1, e = input$TransmissionAsymptoSymp1, o = input$InfectiontoQuanrantine1, y = input$Recovery1)[3]
      
    }
    
    else  if (input$Intervention == '2') {
      
      myfun2(B.Staff = input$BubbleStaff2, B.Student = input$BubbleStudents2, bubble.no = input$NoBubbles2, 
                         BBC.Staff = input$NoOutStaffsContact2, BBC.Student = input$NoOutStudentsContact2, # Between bubbles contact number for each student / staff
                         Trans.Staff = input$TransStaff2, Trans.Student = input$TransStudent2, # transmissibility of student / staff
                         Suscep.Staff = input$SuscepStaff2, Suscep.Student = input$SuscepStudent2, # susceptibility of student / staff
                         Trans.In = input$TransIn2, Trans.Out = input$TransOut2, # transmission rate inside / outside the bubble
                         detect.Staff = input$PercentSympStaff2, detect.Student = input$PercentSympStudent2, # if a student / staff is infected, the probablity that he is symptomatic
                         a = input$LatenttoInfection2, e = input$TransmissionAsymptoSymp2, o = input$InfectiontoQuanrantine2, y = input$Recovery2)[3]
      
    }
    
    else if (input$Intervention == '3') {
      
      myfun3(B.Staff = input$BubbleStaff2, B.Student = input$BubbleStudents2, bubble.no = input$NoBubbles2, 
                      BBC.Staff = input$NoOutStaffsContact2, BBC.Student = input$NoOutStudentsContact2, # Between bubbles contact number for each student / staff
                      Trans.Staff = input$TransStaff2, Trans.Student = input$TransStudent2, # transmissibility of student / staff
                      Suscep.Staff = input$SuscepStaff2, Suscep.Student = input$SuscepStudent2, # susceptibility of student / staff
                      Trans.In = input$TransIn2, Trans.Out = input$TransOut2, # transmission rate inside / outside the bubble
                      detect.Staff = input$PercentSympStaff2, detect.Student = input$PercentSympStudent2, # if a student / staff is infected, the probablity that he is symptomatic
                      a = input$LatenttoInfection2, e = input$TransmissionAsymptoSymp2, o = input$InfectiontoQuanrantine2, y = input$Recovery2)[3]
      
    }
    
    else if (input$Intervention == '4' & input$Openingdays == '1') {
      
      myfun4.1(B.Staff = input$BubbleStaff4, B.Student = input$BubbleStudents4, bubble.no = input$NoBubbles4, 
                        BBC.Staff = input$NoOutStaffsContact4, BBC.Student = input$NoOutStudentsContact4, # Between bubbles contact number for each student / staff
                        Trans.Staff = input$TransStaff4, Trans.Student = input$TransStudent4, # transmissibility of student / staff
                        Suscep.Staff = input$SuscepStaff4, Suscep.Student = input$SuscepStudent4, # susceptibility of student / staff
                        Trans.In = input$TransIn4, Trans.Out = input$TransOut4, # transmission rate inside / outside the bubble
                        detect.Staff = input$PercentSympStaff4, detect.Student = input$PercentSympStudent4, # if a student / staff is infected, the probablity that he is symptomatic
                        a = input$LatenttoInfection4, e = input$TransmissionAsymptoSymp4, o = input$InfectiontoQuanrantine4, y = input$Recovery4)[3]
      
    }
    
    else if (input$Intervention == '4' & input$Openingdays == '2') {
      
      myfun4.2(B.Staff = input$BubbleStaff4, B.Student = input$BubbleStudents4, bubble.no = input$NoBubbles4, 
                        BBC.Staff = input$NoOutStaffsContact4, BBC.Student = input$NoOutStudentsContact4, # Between bubbles contact number for each student / staff
                        Trans.Staff = input$TransStaff4, Trans.Student = input$TransStudent4, # transmissibility of student / staff
                        Suscep.Staff = input$SuscepStaff4, Suscep.Student = input$SuscepStudent4, # susceptibility of student / staff
                        Trans.In = input$TransIn4, Trans.Out = input$TransOut4, # transmission rate inside / outside the bubble
                        detect.Staff = input$PercentSympStaff4, detect.Student = input$PercentSympStudent4, # if a student / staff is infected, the probablity that he is symptomatic
                        a = input$LatenttoInfection4, e = input$TransmissionAsymptoSymp4, o = input$InfectiontoQuanrantine4, y = input$Recovery4)[3]
      
    }
    
    else if (input$Intervention == '4' & input$Openingdays == '3') {
      
      myfun4.3(B.Staff = input$BubbleStaff4, B.Student = input$BubbleStudents4, bubble.no = input$NoBubbles4, 
                        BBC.Staff = input$NoOutStaffsContact4, BBC.Student = input$NoOutStudentsContact4, # Between bubbles contact number for each student / staff
                        Trans.Staff = input$TransStaff4, Trans.Student = input$TransStudent4, # transmissibility of student / staff
                        Suscep.Staff = input$SuscepStaff4, Suscep.Student = input$SuscepStudent4, # susceptibility of student / staff
                        Trans.In = input$TransIn4, Trans.Out = input$TransOut4, # transmission rate inside / outside the bubble
                        detect.Staff = input$PercentSympStaff4, detect.Student = input$PercentSympStudent4, # if a student / staff is infected, the probablity that he is symptomatic
                        a = input$LatenttoInfection4, e = input$TransmissionAsymptoSymp4, o = input$InfectiontoQuanrantine4, y = input$Recovery4)[3]
      
    }
    
    else if (input$Intervention == '4' & input$Openingdays == '4') {
      
      myfun4.4(B.Staff = input$BubbleStaff4, B.Student = input$BubbleStudents4, bubble.no = input$NoBubbles4, 
                        BBC.Staff = input$NoOutStaffsContact4, BBC.Student = input$NoOutStudentsContact4, # Between bubbles contact number for each student / staff
                        Trans.Staff = input$TransStaff4, Trans.Student = input$TransStudent4, # transmissibility of student / staff
                        Suscep.Staff = input$SuscepStaff4, Suscep.Student = input$SuscepStudent4, # susceptibility of student / staff
                        Trans.In = input$TransIn4, Trans.Out = input$TransOut4, # transmission rate inside / outside the bubble
                        detect.Staff = input$PercentSympStaff4, detect.Student = input$PercentSympStudent4, # if a student / staff is infected, the probablity that he is symptomatic
                        a = input$LatenttoInfection4, e = input$TransmissionAsymptoSymp4, o = input$InfectiontoQuanrantine4, y = input$Recovery4)[3]
      
    }
    
    else if (input$Intervention == '5' & input$Openingdays == '1') {
      
      myfun5.1(B.Staff = input$BubbleStaff4, B.Student = input$BubbleStudents4, bubble.no = input$NoBubbles4, 
                        BBC.Staff = input$NoOutStaffsContact4, BBC.Student = input$NoOutStudentsContact4, # Between bubbles contact number for each student / staff
                        Trans.Staff = input$TransStaff4, Trans.Student = input$TransStudent4, # transmissibility of student / staff
                        Suscep.Staff = input$SuscepStaff4, Suscep.Student = input$SuscepStudent4, # susceptibility of student / staff
                        Trans.In = input$TransIn4, Trans.Out = input$TransOut4, # transmission rate inside / outside the bubble
                        detect.Staff = input$PercentSympStaff4, detect.Student = input$PercentSympStudent4, # if a student / staff is infected, the probablity that he is symptomatic
                        a = input$LatenttoInfection4, e = input$TransmissionAsymptoSymp4, o = input$InfectiontoQuanrantine4, y = input$Recovery4)[3]
      
    }
    
    else if (input$Intervention == '5' & input$Openingdays == '2') {
      
      myfun5.2(B.Staff = input$BubbleStaff4, B.Student = input$BubbleStudents4, bubble.no = input$NoBubbles4, 
                        BBC.Staff = input$NoOutStaffsContact4, BBC.Student = input$NoOutStudentsContact4, # Between bubbles contact number for each student / staff
                        Trans.Staff = input$TransStaff4, Trans.Student = input$TransStudent4, # transmissibility of student / staff
                        Suscep.Staff = input$SuscepStaff4, Suscep.Student = input$SuscepStudent4, # susceptibility of student / staff
                        Trans.In = input$TransIn4, Trans.Out = input$TransOut4, # transmission rate inside / outside the bubble
                        detect.Staff = input$PercentSympStaff4, detect.Student = input$PercentSympStudent4, # if a student / staff is infected, the probablity that he is symptomatic
                        a = input$LatenttoInfection4, e = input$TransmissionAsymptoSymp4, o = input$InfectiontoQuanrantine4, y = input$Recovery4)[3]
      
    }
    
    else if (input$Intervention == '5' & input$Openingdays == '3') {
      
      myfun5.3(B.Staff = input$BubbleStaff4, B.Student = input$BubbleStudents4, bubble.no = input$NoBubbles4, 
                        BBC.Staff = input$NoOutStaffsContact4, BBC.Student = input$NoOutStudentsContact4, # Between bubbles contact number for each student / staff
                        Trans.Staff = input$TransStaff4, Trans.Student = input$TransStudent4, # transmissibility of student / staff
                        Suscep.Staff = input$SuscepStaff4, Suscep.Student = input$SuscepStudent4, # susceptibility of student / staff
                        Trans.In = input$TransIn4, Trans.Out = input$TransOut4, # transmission rate inside / outside the bubble
                        detect.Staff = input$PercentSympStaff4, detect.Student = input$PercentSympStudent4, # if a student / staff is infected, the probablity that he is symptomatic
                        a = input$LatenttoInfection4, e = input$TransmissionAsymptoSymp4, o = input$InfectiontoQuanrantine4, y = input$Recovery4)[3]
      
    }
    
    else {
      
      myfun5.4(B.Staff = input$BubbleStaff4, B.Student = input$BubbleStudents4, bubble.no = input$NoBubbles4, 
                        BBC.Staff = input$NoOutStaffsContact4, BBC.Student = input$NoOutStudentsContact4, # Between bubbles contact number for each student / staff
                        Trans.Staff = input$TransStaff4, Trans.Student = input$TransStudent4, # transmissibility of student / staff
                        Suscep.Staff = input$SuscepStaff4, Suscep.Student = input$SuscepStudent4, # susceptibility of student / staff
                        Trans.In = input$TransIn4, Trans.Out = input$TransOut4, # transmission rate inside / outside the bubble
                        detect.Staff = input$PercentSympStaff4, detect.Student = input$PercentSympStudent4, # if a student / staff is infected, the probablity that he is symptomatic
                        a = input$LatenttoInfection4, e = input$TransmissionAsymptoSymp4, o = input$InfectiontoQuanrantine4, y = input$Recovery4)[3]
      
    }
    
  })
}



app <- shinyApp(ui = UI, server = SERVER)
runApp(app)


# submitButton("Update Plots")  #放在ui里的update按键
