library(ggplot2)
###################################################################################################################
T = 100
###################################################################################################################
######### input #########
myfun1 <- function(N.Staff, # total staffs at school
                  N.Student, # total students at school
                  Trans.Staff, # transmissibility of staff
                  Trans.Student, # transmissibility of student
                  Suscep.Staff, # susceptibility of staff
                  Suscep.Student, # susceptibility of student
                  C.StudentStudent, # number of contact between students
                  C.StudentStaff, # number of staffs a student contact with
                  C.StaffStudent, # number of students a staff contact with
                  C.StaffStaff, # number of contact between staffs
                  a, # ALPHA: rate from latent to infection
                  detect.Staff, # if a staff is infected, the probablity that he is symptomatic
                  detect.Student, # if a student is infected, the probablity that he is symptomatic
                  e, # the percentage of transmissibility for an asymptomatic case compare to a symptomatic case
                  o, # the rate for a detectable case being quarantined (depends on the number of tests and accuracy of tests)
                  y # the recovery rate
                  ){
  
  ######### Data frame #########
  B <- matrix(rep(0), T, 1)
  A <- array(rep(B), dim = c(T, 6))
  
  A[1,] <- c(N.Student-1,1,0,0,0,0)   # initials
  
  dimnames(A) <- list(1:T, c("S","E","D","U","Q","R"))  # name the array
  
  head(A)
  df <- as.data.frame(A)
  #########
  
  ######### Set initials #########
  R = Q = U = D = E = S=matrix(rep(0), T+1)
  
  S.Staff=S1=S2=S3=S4=S
  E.Staff=E1=E2=E3=E4=E
  D.Staff=D1=D2=D3=D4=D
  U.Staff=U1=U2=U3=U4=U
  Q.Staff=Q1=Q2=Q3=Q4=Q
  R.Staff=R1=R2=R3=R4=R
  #########
  
  ######### Parameters #########
  Ta = matrix(c(Trans.Student,Trans.Staff), nrow = 2) # transmissibility of student and staff
  Ca = matrix(c(Suscep.Student,Suscep.Staff), nrow = 1)  # susceptibility of student and staff
  tau = mean(0.54, 0.54/2)  # transmission rate 
  # set p: the transmission rate: 
  p1 = Ta %*% Ca
  # p = (student-student, staff-student, student-staff, staff-staff )
  p = c(p1[1,1]*tau, p1[2,1]*tau, p1[1,2]*tau, p1[2,2]*tau) #RHO
  d = c(detect.Student,detect.Staff)  # d:probability of an infection is detectable/symptomatic for student and staff
 
  #########
  
  ######### Stimulation #########
  for (i in 1:T){
    ######### For STUDENTS #########
    ######### initals #########
    S[1] =  N.Student-1
    E[1] =  1
    D[1] =  0
    U[1] =  0
    Q[1] =  0
    R[1] =  0
    #########
    
    ######### STUDENT to STUDENT #########
    S1[1] =  N.Student-1
    E1[1] =  1
    D1[1] =  0
    U1[1] =  0
    Q1[1] =  0
    R1[1] =  0
  
    # This formula comes from the dynamic equations (in powerpoint slide 4)
    S1[i+1] = S[i] - C.StudentStudent*p[1]*(D[i]+e*U[i])
    E1[i+1] = E[i] + C.StudentStudent*p[1]*(D[i]+e*U[i]) - a*E[i]
    D1[i+1] = D[i] + a*d[1]*E[i] - o*D[i]
    U1[i+1] = U[i] + a*(1-d[1])*E[i] - y*U[i]
    Q1[i+1] = Q[i] + o*D[i] - y*Q[i]
    R1[i+1] = R[i] + y*(Q[i]+U[i])
    #########
    
    ######### STAFF to STUDENT #########
    # initial - there is no change of S and E initially
    S2[1] = 0
    E2[1] = 0
    
    # The change in S and E for bubble j depends on the number of D and U of staffs
    S2[i+1] = - C.StaffStudent*p[2]*(D.Staff[i]+e*U.Staff[i])   
    E2[i+1] = C.StaffStudent*p[2]*(D.Staff[i]+e*U.Staff[i]) 
    #########
    
    
    ######### TOTAL on STUDNETS #########
    if (S1[i+1] + S2[i+1] >= 0){   # The total number of S has to be >= 0
      S[i+1] = S1[i+1] + S2[i+1]
      E[i+1] = E1[i+1] + E2[i+1]
    }
    else {  # If not, S = 0, 
      # E = previous (time step) amout of E - the same amount of students reduced in S compartment - those move to the next compartments
      S[i+1] = 0
      E[i+1] = E[i] - (S[i+1]-S[i]) - a*E[i]
    }
    if (E[i+1] >= 0){
      
    }
    else {
      E[i+1] = 0
    }
    
    D[i+1] = D1[i+1] 
    U[i+1] = U1[i+1]
    Q[i+1] = Q1[i+1]
    R[i+1] = R1[i+1]
  
    
    ######### For STAFF ######### 
    
    ######### initals #########
    S.Staff[1] =  N.Staff
    E.Staff[1] =  0
    D.Staff[1] =  0
    U.Staff[1] =  0
    Q.Staff[1] =  0
    R.Staff[1] =  0
    #########
  
    ######### STUDENT to STAFF #########
    # initials
    S3[1] = 0
    
    # Only S and E compartments will change directly by the infection cause by studnets (E3 = -S3)
    S3[i+1] = - C.StudentStaff*p[3]*(D[i]+e*U[i])
    
    ######### STAFF to STAFF #########
    # initials
    S4[1] = N.Staff
    
    S4[i+1] = S.Staff[i] - C.StaffStaff*p[4]*(D.Staff[i]+e*U.Staff[i])
    E4[i+1] = E.Staff[i] + C.StaffStaff*p[4]*(D.Staff[i]+e*U.Staff[i]) - a*E.Staff[i]
    D4[i+1] = D.Staff[i] + a*d[2]*E.Staff[i] - o*D.Staff[i]
    U4[i+1] = U.Staff[i] + a*(1-d[2])*E.Staff[i] - y*U.Staff[i]
    Q4[i+1] = Q.Staff[i] + o*D.Staff[i] - y*Q.Staff[i]
    R4[i+1] = R.Staff[i] + y*(Q.Staff[i]+U.Staff[i])
    #########
    
    ######### TOTAL on STAFF #########
    if (S4[i+1] + S3[i+1] >= 0){   # The total number of S has to be >= 0
      S.Staff[i+1] = S4[i+1] + S3[i+1]
      E.Staff[i+1] = E4[i+1] - S3[i+1] 
    }
    else {  # If not, S = 0, 
      #         E = previous (time step) amout of E - the same amount of students reduced in S compartment - those move to the next compartments
      S.Staff[i+1] = 0
      E.Staff[i+1]= E.Staff[i] + S.Staff[i] - a*E.Staff[i]
    }
    
    D.Staff[i+1] = D4[i+1] 
    U.Staff[i+1] = U4[i+1]
    Q.Staff[i+1] = Q4[i+1] 
    R.Staff[i+1] = R4[i+1] 
    #########
    
    
    ######### Add the results to the dataframe da #########
    df$S[i] =S[i]
    df$E[i] =E[i]
    df$D[i] =D[i]
    df$U[i] =U[i]
    df$Q[i] =Q[i]
    df$R[i] =R[i]
    
    df$S.Staff[i] =S.Staff[i]
    df$E.Staff[i] =E.Staff[i]
    df$D.Staff[i] =D.Staff[i]
    df$U.Staff[i] =U.Staff[i]
    df$Q.Staff[i] =Q.Staff[i]
    df$R.Staff[i] =R.Staff[i]
    #########
    
    ######### The change on Weekends  #########
    # There will not be new infections from 0 A.M. on Saturday till 0 A.M. on the following Monday
    
    offschool = c(seq(0,T,7)-1,seq(0,T,7)-0)
    
    if (!is.na(match(i, setdiff(0:T,offschool))) == TRUE){  #if time i is weekday
    }
    
    else{  #if time i is weekend
      S[i+1] = S[i]
      E[i+1] = E[i] - a*E[i]
      
      S.Staff[i+1] = S.Staff[i]
      E.Staff[i+1] = E.Staff[i] - a*E.Staff[i]
      
    }
  }
    #########
    
    ######### Add extra column #########
    for (i in 1:T){
      
      # reproduction rate Rt
      df$Rt.Student[i] <- (df$E[i+1] + df$D[i+1] + df$U[i+1] + df$Q[i+1]) / 
        (df$E[i] + df$D[i] + df$U[i] + df$Q[i])
      
      df$Rt.Staff[i] <- (df$E.Staff[i+1] + df$D.Staff[i+1] + df$U.Staff[i+1] + df$Q.Staff[i+1]) / 
        (df$E.Staff[i] + df$D.Staff[i] + df$U.Staff[i] + df$Q.Staff[i])
    }
  
  ######### Plots  #########
  
  p_NoBubble1 <- ggplot(df, aes(x = X_lim, y = S)) + 
    geom_line(aes(x = X_lim, y = S, color="pink")) + 
    geom_line(aes(x = X_lim, y = E, color="orange")) + 
    geom_line(aes(x = X_lim, y = D, color="brown")) + 
    geom_line(aes(x = X_lim, y = U, color="green")) + 
    geom_line(aes(x = X_lim, y = Q, color="blue")) + 
    geom_line(aes(x = X_lim, y = R, color="purple")) + 
    geom_vline(xintercept = 90, colour = "grey", linetype = "dashed") + 
    labs(title="SEDUQR Model for Student - No Bubble", x="Time (day)", y="Number of Students") +
    scale_color_identity(name = "",
                         breaks = c("pink", "orange", "brown", "green", "blue", "purple"),
                         labels = c("S","E","D", "U", "Q", "R"),
                         guide = "legend") +
    annotate("text", x=90, y=-2, label= "End of Term") 
  
  #Staff
  p_NoBubble2 <- ggplot(df, aes(x = X_lim, y = S.Staff)) + 
    geom_line(aes(x = X_lim, y = S.Staff, color="pink")) + 
    geom_line(aes(x = X_lim, y = E.Staff, color="orange")) + 
    geom_line(aes(x = X_lim, y = D.Staff, color="brown")) + 
    geom_line(aes(x = X_lim, y = U.Staff, color="green")) + 
    geom_line(aes(x = X_lim, y = Q.Staff, color="blue")) + 
    geom_line(aes(x = X_lim, y = R.Staff, color="purple")) + 
    geom_vline(xintercept = 90, colour = "grey", linetype = "dashed") + 
    labs(title="SEDUQR Model for Staff - No Bubble", x="Time (day)", y="Number of Staffs") +
    scale_color_identity(name = "",
                         breaks = c("pink", "orange", "brown", "green", "blue", "purple"),
                         labels = c("S","E","D", "U", "Q", "R"),
                         guide = "legend") +
    annotate("text", x=90, y=-2, label= "End of Term") 
  
  #Rt
  df_Rt <- matrix(rep(0), T)
  df_Rt[1:nrow(df)-1] <- c(1,1, (df$Rt.Student[3:(nrow(df)-1)] + df$Rt.Staff[3:(nrow(df)-1)])/2)
  df_Rt[nrow(df)] = df_Rt[nrow(df)-1]
  
  p_NoBubbleRt <- ggplot(df, aes(x = X_lim, y = df_Rt)) + 
    geom_line() +
    labs(title="Rt - No Bubble", x="Time (day)", y="Number of Students and Staffs")
  
  #########
  return(list(p_NoBubble1, p_NoBubble2, p_NoBubbleRt))
}
    #########
    
# myfun1(N.Staff = 50, N.Student = 100, 
#        C.StudentStudent = 20, C.StudentStaff = 3, 
#        C.StaffStudent = 20, C.StaffStaff =  5,
#        Trans.Staff = 1, Trans.Student = 0.64, # transmissibility of student / staff
#      Suscep.Staff = 1, Suscep.Student = 0.79, # susceptibility of student / staff
#      detect.Staff = 0.3, detect.Student = 0.1, # if a student / staff is infected, the probablity that he is symptomatic
#        a = 0.2, e = 0.138, o = 0.75, y = 0.1)


