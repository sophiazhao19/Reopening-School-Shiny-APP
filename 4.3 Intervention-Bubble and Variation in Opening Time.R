###################################################################################################################
T = 100 
###################################################################################################################
######### input #########
myfun4.3 <- function(# N.Staff, # total staffs at school
  # N.Student, # total students at school
  B.Staff, # staffs in each bubble
  B.Student, # students in each bubble
  bubble.no, # number of bubbles
  BBC.Staff, # Between bubbles contact number for each staff
  BBC.Student, # Between bubbles contact number for each student
  Trans.Staff, # transmissibility of staff
  Trans.Student, # transmissibility of student
  Suscep.Staff, # susceptibility of staff
  Suscep.Student, # susceptibility of student
  Trans.In, # transmission rate inside the bubble
  Trans.Out, # transmission rate outside the bubble
  a, # ALPHA: rate from latent to infection
  detect.Staff, # if a staff is infected, the probablity that he is symptomatic
  detect.Student, # if a student is infected, the probablity that he is symptomatic
  e, # the percentage of transmissibility for an asymptomatic case compare to a symptomatic case
  o, # the rate for a detectable case being quarantined (depends on the number of tests and accuracy of tests)
  y # the recovery rate
){
  #########  
  
  ######### Data frame #########
  B <- matrix(rep(0), T, 7)
  A <- array(rep(B), dim = c(T, 7, bubble.no+1))
  
  A[,1,] <- B.Student #  S
  A[,7,] <- B.Student #  N
  A[,1,11] <- B.Staff   #  staffs are S initially
  A[,7,11] <- B.Staff * bubble.no#  staffs in total 
  A[1,,1] <- c(B.Student-1,1,0,0,0,0,B.Student)   # initials
  
  dimnames(A) <- list(1:T, c("S","E","D","U","Q","R","N"), c(sprintf("B%d", 1:bubble.no),"Staff"))  # name the array
  
  head(A)
  da <- as.data.frame(A)
  #########
  
  ######### Set initials #########
  S <- matrix(rep(0), T+1, bubble.no)
  E <- matrix(rep(0), T+1, bubble.no)
  D <- matrix(rep(0), T+1, bubble.no)
  U <- matrix(rep(0), T+1, bubble.no)
  Q <- matrix(rep(0), T+1, bubble.no)
  R <- matrix(rep(0), T+1, bubble.no)
  r <- matrix(rep(0), T+1, bubble.no)
  
  S.Staff=S1=S2=S3=S4=S5=S6=S
  E.Staff=E1=E2=E3=E4=E5=E6=E
  D.Staff=D1=D2=D3=D4=D5=D6=D
  U.Staff=U1=U2=U3=U4=U5=U6=U
  Q.Staff=Q1=Q2=Q3=Q4=Q5=Q6=Q
  R.Staff=R1=R2=R3=R4=R5=R6=R
  r1=r2=r3=r4=r5=r
  
  S5.diff=S2.diff=S
  E5.diff=E2.diff=E
  D5.diff=D2.diff=D
  U5.diff=U2.diff=U
  R5.diff=R2.diff=R
  r5.diff=r2.diff=r
  
  S.Staff.change = S.Staff
  
  # Staff replacement
  Staff.School = S.Staff  
  Staff.rp = S.Staff  
  
  #########
  
  ######### Parameters #########
  Ta = matrix(c(Trans.Student,Trans.Staff), nrow = 2) # transmissibility of student and staff
  Ca = matrix(c(Suscep.Student,Suscep.Staff), nrow = 1)  # susceptibility of student and staff
  tau = c(Trans.In, Trans.Out)  # individual transmission rate within and outside the bubble
  # set p: the transmission rate: 
  p1 = Ta %*% Ca
  # p = (student-student same bubble, student-student different, staff-student, student-staff, staff-staff both teaching same or different bubbles)
  p = c(p1[1,1]*tau[1], p1[1,1]*tau[2], p1[2,1]*tau[2], p1[1,2]*tau[2], p1[2,2]*tau[2]) #RHO
  d = c(detect.Student,detect.Staff) # d:probability of an infection is detectable/symptomatic for student and staff
  #########
  
  ######### Stimulation #########
  for (i in 1:T){
    for (j in 1:bubble.no){  
      
      ######### For STUDENTS #########
      ######### initals #########
      S[1,j] =  da[1,paste("S.B", j, sep = "")]
      E[1,j] =  da[1,paste("E.B", j, sep = "")]
      D[1,j] =  da[1,paste("D.B", j, sep = "")]
      U[1,j] =  da[1,paste("U.B", j, sep = "")]
      Q[1,j] =  da[1,paste("Q.B", j, sep = "")]
      R[1,j] =  da[1,paste("R.B", j, sep = "")]
      #########
      
      ######### STUDENT to STUDENT in the SAME BUBBLE #########
      S1[1,j] =  da[1,paste("S.B", j, sep = "")]
      E1[1,j] =  da[1,paste("E.B", j, sep = "")]
      D1[1,j] =  da[1,paste("D.B", j, sep = "")]
      U1[1,j] =  da[1,paste("U.B", j, sep = "")]
      Q1[1,j] =  da[1,paste("Q.B", j, sep = "")]
      R1[1,j] =  da[1,paste("R.B", j, sep = "")]
      r1[1,j] =  S[1,j]  # number of contact depends on how many S in the bubble
      
      # This formula comes from the dynamic equations (in powerpoint slide 4)
      S1[i+1,j] = S[i,j] - r1[i,j]*p[1]*(D[i,j]+e*U[i,j])
      E1[i+1,j] = E[i,j] + r1[i,j]*p[1]*(D[i,j]+e*U[i,j]) - a*E[i,j]
      D1[i+1,j] = D[i,j] + a*d[1]*E[i,j] - o*D[i,j]
      U1[i+1,j] = U[i,j] + a*(1-d[1])*E[i,j] - y*U[i,j]
      Q1[i+1,j] = Q[i,j] + o*D[i,j] - y*Q[i,j]
      R1[i+1,j] = R[i,j] + y*(Q[i,j]+U[i,j])
      r1[i+1,j] = S[i,j] 
      #########
      
      ######### STUDENT to STUDENT in DIFFERENT BUBBLES #########
      # number of contact for each individual in bubble j to students in other bubbles
      r2.diff[1,j] =  BBC.Student  # each student in j contact a few students in other bubbles
      r2.diff[i+1,j] = r2.diff[i,j]    
      # the total number of contacts of the students in bubble j
      m = round(r2.diff[i,j]*(S[i,j]))  # only Susceptible individuals in j might be infected by students in other bubbles
      
      # inital values of total number of students at school not in bubble j
      # (only S,E,D,U are at school, currently not considered recovery come back to school yet)
      S2.diff[1,j] = B.Student * bubble.no
      E2.diff[1,j] = 0
      D2.diff[1,j] = 0
      U2.diff[1,j] = 0
      R2.diff[1,j] = 0
      
      # Subset for S,E,D,U compartments of all bubbles 
      all.S = (c(1:ncol(da)) %% 7) == 1
      all.E = (c(1:ncol(da)) %% 7) == 2
      all.D = (c(1:ncol(da)) %% 7) == 3
      all.U = (c(1:ncol(da)) %% 7) == 4
      all.R = (c(1:ncol(da)) %% 7) == 6
      
      #total number of students at school not in bubble j (and not staff: the reason of removing the last column)
      # The number of S, E, D, U at time i, different to bubble j
      S2.diff[i,j] = round(sum((da[i,(all.S)][,-j])[,c(1:ncol(da[i,(all.S)][,-j])-1)]))
      E2.diff[i,j] = round(sum((da[i,(all.E)][,-j])[,c(1:ncol(da[i,(all.E)][,-j])-1)]))
      D2.diff[i,j] = round(sum((da[i,(all.D)][,-j])[,c(1:ncol(da[i,(all.D)][,-j])-1)]))
      U2.diff[i,j] = round(sum((da[i,(all.U)][,-j])[,c(1:ncol(da[i,(all.U)][,-j])-1)]))
      R2.diff[i,j] = round(sum((da[i,(all.R)][,-j])[,c(1:ncol(da[i,(all.R)][,-j])-1)]))
      
      # make a list of S, E, D, U not in bubble j (the times of repeat is the number of students in each compartment)
      list = c(rep("S", S2.diff[i,j]), rep("E", E2.diff[i,j]), rep("D", D2.diff[i,j]), rep("U", U2.diff[i,j]), rep("R", R2.diff[i,j]))
      
      #count the number of S,E,D,U that students in j contacted to
      # " sample(list, m, replace = TRUE) ": Randomly select m from list
      set.seed(123)
      countS = sum((sample(list, m, replace = TRUE))=="S") # for students in bubble j: the number of contact outside bubble j are S
      countE = sum((sample(list, m, replace = TRUE))=="E") # for students in bubble j: the number of contact outside bubble j are E
      countD = sum((sample(list, m, replace = TRUE))=="D") # for students in bubble j: the number of contact outside bubble j are D
      countU = sum((sample(list, m, replace = TRUE))=="U") # for students in bubble j: the number of contact outside bubble j are U
      countR = sum((sample(list, m, replace = TRUE))=="R") # for students in bubble j: the number of contact outside bubble j are R
      
      # The number of change in S and E compartments by interactions from other bubbles to bubbles j (only the partial effect)
      # initial (no intractions between bubbles at time 1)
      S2[1,j] = 0
      E2[1,j] = 0
      
      # The change in S and E for bubble j only depends on the number of:
      # 1. (   susceptible students in bubble j: r1[i,j] = S[i,j]   ) and 
      # 2. (   number of detectable/symtomatic and undetectable/asymtomatic that are contacted in other bubbles: countD+e*countU   
      #                                                        -- e is because the effectiveness on D and U in infection rate are different)
      S2[i+1,j] = -p[2]*(countD+e*countU)  # Only the change because of other bubbles
      E2[i+1,j] = p[2]*(countD+e*countU)  # Only the change because of other bubbles, so those Es goes into D/U compartments are included in E1
      #########
      
      ######### STAFF to STUDENTs they teach #########
      ######### initial - there is no change of S and E initially
      S3[1,j] = 0
      E3[1,j] = 0
      r3[1,j] = B.Student
      
      # The change in S and E for bubble j depends on the number of D and U of staffs
      S3[i+1,j] = - r1[i,j]*p[3]*(D.Staff[i,j]+e*U.Staff[i,j])   
      E3[i+1,j] = r1[i,j]*p[3]*(D.Staff[i,j]+e*U.Staff[i,j]) 
      r3[i+1,j] =  S[i,j]
      
      
      
      #########
      
      ######### TOTAL on STUDNETS #########
      if (S1[i+1,j] + S2[i+1,j] + S3[i+1,j] >= 0){   # The total number of S has to be >= 0
        S[i+1,j] = S1[i+1,j] + S2[i+1,j] + S3[i+1,j] 
        E[i+1,j] = E1[i+1,j] + E2[i+1,j] + E3[i+1,j] 
      }
      else {  # If not, S = 0, 
        #         E = previous (time step) amout of E - the same amount of students reduced in S compartment - those move to the next compartments
        S[i+1,j] = 0
        E[i+1,j] = E[i,j] - (S[i+1,j]-S[i,j]) - a*E[i,j]
      }
      
      D[i+1,j] = D1[i+1,j]
      U[i+1,j] = U1[i+1,j]
      Q[i+1,j] = Q1[i+1,j]
      R[i+1,j] = R1[i+1,j] 
      #########
      
      
      ######### For STAFF #########
      
      # The overall effect on STAFF (student -> staff  &   staff -> staff in same / different bubble)
      
      # initials
      S.Staff[1,] =  B.Staff
      E.Staff[1,] =  0
      D.Staff[1,] =  0
      U.Staff[1,] =  0
      Q.Staff[1,] =  0
      R.Staff[1,] =  0
      
      ######### STUDENT to STAFF #########
      # initials
      S4[1,j] = 0
      r4[1,j] = B.Staff
      
      # Only S and E compartments will change directly by the infection cause by studnets (E3 = -S3)
      S4[i+1,j] = - r4[i]*p[4]*(D[i,j]+e*U[i,j])
      r4[i+1,j] = S.Staff[i,j]
      #########
      
      ######### STAFF to STAFF (teach same bubble) #########
      # initials
      S6[1,j] = 0
      
      # Only S and E compartments will change directly by the infection cause by staffs teach same bubble (E6 = -S6)
      S6[i+1,j] = - r4[i]*p[5]*(D.Staff[i,j]+e*U.Staff[i,j])
      #########
      
      ######### STAFF to STAFF (teach different bubbles) #########
      # number of contact for each individual staff teach bubble j to the staffs teach other bubbles
      r5.diff[1,j] = BBC.Staff  # each staff teach j contact 3 staffs teach other bubbles
      r5.diff[i+1,j] = r5.diff[i,j]    
      # the total number of contacts of the students in bubble j
      m.Staff = round(r5.diff[i,j]*(S.Staff[i,j]))  # only Susceptible staffs in j might be infected 
      
      # inital values of total number of staff at school not teach bubble j
      # (only S,E,D,U are at school, currently not considered recovery come back to school yet)
      S5.diff[1,] = (bubble.no-1) * B.Staff
      E5.diff[1,] = 0
      D5.diff[1,] = 0
      U5.diff[1,] = 0
      R5.diff[1,] = 0
      
      # The number of S, E, D, U at time i, not teaching to bubble j
      S5.diff[i,j] = round(sum(S.Staff[i,])-S.Staff[i,j])
      E5.diff[i,j] = round(sum(E.Staff[i,])-E.Staff[i,j])
      D5.diff[i,j] = round(sum(D.Staff[i,])-D.Staff[i,j])
      U5.diff[i,j] = round(sum(U.Staff[i,])-U.Staff[i,j])
      R5.diff[i,j] = round(sum(R.Staff[i,])-R.Staff[i,j])
      
      # make a list of S, E, D, U not teach bubble j (the times of repeat is the number of staff in each compartment)
      list.Staff = c(rep("S", S5.diff[i,j]), rep("E", E5.diff[i,j]), rep("D", D5.diff[i,j]), rep("U", U5.diff[i,j]), rep("R", R5.diff[i,j]))
      
      #count the number of S,E,D,U that staffs teaching bubble j contacted to
      # " sample(list.Staff, m.Staff, replace = TRUE) ": Randomly select m.Staff from list
      set.seed(123)
      count.StaffS = sum((sample(list.Staff, m.Staff, replace = TRUE))=="S") # for students in bubble j: the number of contact outside bubble j are S
      count.StaffE = sum((sample(list.Staff, m.Staff, replace = TRUE))=="E") # for students in bubble j: the number of contact outside bubble j are E
      count.StaffD = sum((sample(list.Staff, m.Staff, replace = TRUE))=="D") # for students in bubble j: the number of contact outside bubble j are D
      count.StaffU = sum((sample(list.Staff, m.Staff, replace = TRUE))=="U") # for students in bubble j: the number of contact outside bubble j are U
      count.StaffR = sum((sample(list.Staff, m.Staff, replace = TRUE))=="R") # for students in bubble j: the number of contact outside bubble j are R
      
      # The number of change in S and E compartments by staffs teach j and other staffs contacts
      # initial (no transmission at time 1)
      S5[1,j] = 0
      
      # The change in S and E for staffs teach bubble j only depends on the number of:
      # 1. (   susceptible staffs teach bubble j: r5[i,j] = S.Staff[i,j]   ) and 
      # 2. (   number of detectable/symtomatic and undetectable/asymtomatic staffs teach other bubbles: count.StaffD+e*count.StaffU
      #                                                        -- e is because the effectiveness on D and U in infection rate are different)
      S5[i+1,j] = - p[5]*(count.StaffD+e*count.StaffU)  # Only the change because of staff teach other bubbles
      # E5 = -S5
      #########
      
      ######### TOTAL on STAFF #########
      # the total change of staff
      
      S.Staff.change[1,j] = 0
      S.Staff.change[i+1,j] = -(S4[i+1,j] + S5[i+1,j] + S6[i+1,j])    
      
      # With larger number in each group, remove the overlapping cases of new infection by:
      # (There might be chance an individual staff is infected by both students he teach and other staff he contact with)
      # Here, use randomly choosing to reduce the overlapping change effect
      # # # count.StaffS_overlap = - round(S3[i+1,j] + S5[i+1,j] + S6[i+1,j]) # the infected cases/the change in S (with overlap)
      
      # This formula comes from the dynamic equations (in powerpoint slide 4)
      S.Staff[i+1,j] = S.Staff[i,j] - S.Staff.change[i+1,j]
      E.Staff[i+1,j] = E.Staff[i,j] + S.Staff.change[i+1,j] - a*E.Staff[i,j]
      D.Staff[i+1,j] = D.Staff[i,j] + a*d[2]*E.Staff[i,j] - o*D.Staff[i,j]
      U.Staff[i+1,j] = U.Staff[i,j] + a*(1-d[2])*E.Staff[i,j] - y*U.Staff[i,j]
      Q.Staff[i+1,j] = Q.Staff[i,j] + o*D.Staff[i,j] - y*Q.Staff[i,j]
      R.Staff[i+1,j] = R.Staff[i,j] + y*(Q.Staff[i,j]+U.Staff[i,j])
      #########
      
      ########## 1 day on 1 day off : School day Monday, Wednesday and Friday #########
      onoff1 = c(seq(0,T,7)-5, seq(0,T,7)-3, seq(0,T,7)-1, seq(0,T,7))
      
      if (!is.na(match(i, setdiff(0:T,onoff1))) == TRUE){  #if time i is weekday
      }
      
      else{  #if time i is weekend
        S[i+1,j] = S[i,j]
        E[i+1,j] = E[i,j] - a*E[i,j]
        
        S.Staff[i+1,j] = S.Staff[i,j]
        E.Staff[i+1,j] = E.Staff[i,j] - a*E.Staff[i,j]
        
      } 
      #########
      
      
      ######### Add the results of each time step i and each bubble j of each compartments to the dataframe da #########
      da[i,paste("S.B", j, sep = "")] <- S[i,j]
      da[i,paste("E.B", j, sep = "")] <- E[i,j]
      da[i,paste("D.B", j, sep = "")] <- D[i,j]
      da[i,paste("U.B", j, sep = "")] <- U[i,j]
      da[i,paste("Q.B", j, sep = "")] <- Q[i,j]
      da[i,paste("R.B", j, sep = "")] <- R[i,j]
      
      da[i,paste("S.", "Staff", sep = "")] <- sum(S.Staff[i,]) 
      da[i,paste("E.", "Staff", sep = "")] <- sum(E.Staff[i,]) 
      da[i,paste("D.", "Staff", sep = "")] <- sum(D.Staff[i,]) 
      da[i,paste("U.", "Staff", sep = "")] <- sum(U.Staff[i,]) 
      da[i,paste("Q.", "Staff", sep = "")] <- sum(Q.Staff[i,]) 
      da[i,paste("R.", "Staff", sep = "")] <- sum(R.Staff[i,]) 
      
      # Sum up the total dynamic of students for all bubbles
      da$S.Student[i] <- sum(S[i,])
      da$E.Student[i] <- sum(E[i,])
      da$D.Student[i] <- sum(D[i,])
      da$U.Student[i] <- sum(U[i,])
      da$Q.Student[i] <- sum(Q[i,])
      da$R.Student[i] <- sum(R[i,])
      da$N.Student[i] <- bubble.no * B.Student
      #########
    }
  }
  
  ######### Add extra columns: reproduction rate Rt #########
  for (i in 1:T){
    da$Rt.Student[i] <- (da$E.Student[i+1] + da$D.Student[i+1] + da$U.Student[i+1] + da$Q.Student[i+1]) / 
      (da$E.Student[i] + da$D.Student[i] + da$U.Student[i] + da$Q.Student[i])
    
    da$Rt.Staff[i] <- (da$E.Staff[i+1] + da$D.Staff[i+1] + da$U.Staff[i+1] + da$Q.Staff[i+1]) / 
      (da$E.Staff[i] + da$D.Staff[i] + da$U.Staff[i] + da$Q.Staff[i])
  }
  #########
  
  ######### Plot SEDUQR graphs  #########
  par(mfrow=c(2, 1), lwd=1)
  
  # Students
  X_lim <- seq(1,T,by=1)
  p_1 <- ggplot(da, aes(x = X_lim, y = S.Student)) + 
    geom_line(aes(x = X_lim, y = S.Student, color="pink")) + 
    geom_line(aes(x = X_lim, y = E.Student, color="orange")) + 
    geom_line(aes(x = X_lim, y = D.Student, color="brown")) + 
    geom_line(aes(x = X_lim, y = U.Student, color="green")) + 
    geom_line(aes(x = X_lim, y = Q.Student, color="blue")) + 
    geom_line(aes(x = X_lim, y = R.Student, color="purple")) + 
    geom_vline(xintercept = 90, colour = "grey", linetype = "dashed") + 
    labs(title="SEDUQR Model for Student", x="Time (day)", y="Number of Students") +
    scale_color_identity(name = "",
                         breaks = c("pink", "orange", "brown", "green", "blue", "purple"),
                         labels = c("S","E","D", "U", "Q", "R"),
                         guide = "legend") +
    annotate("text", x=90, y=-2, label= "End of Term") 
  
  # Staffs
  X_lim <- seq(1,T,by=1)
  p_2 <- ggplot(da, aes(x = X_lim, y = S.Staff)) + 
    geom_line(aes(x = X_lim, y = S.Staff, color="pink")) + 
    geom_line(aes(x = X_lim, y = E.Staff, color="orange")) + 
    geom_line(aes(x = X_lim, y = D.Staff, color="brown")) + 
    geom_line(aes(x = X_lim, y = U.Staff, color="green")) + 
    geom_line(aes(x = X_lim, y = Q.Staff, color="blue")) + 
    geom_line(aes(x = X_lim, y = R.Staff, color="purple")) + 
    geom_vline(xintercept = 90, colour = "grey", linetype = "dashed") + 
    labs(title="SEDUQR Model for Staff", x="Time (day)", y="Number of Staffs") +
    scale_color_identity(name = "",
                         breaks = c("pink", "orange", "brown", "green", "blue", "purple"),
                         labels = c("S","E","D", "U", "Q", "R"),
                         guide = "legend") +
    annotate("text", x=90, y=-2, label= "End of Term") 
  
  # Rt
  da_Rt <- matrix(rep(0), T)
  da_Rt[1:nrow(da)-1] <- c(1,1, (da$Rt.Student[3:(nrow(da)-1)] + da$Rt.Staff[3:(nrow(da)-1)])/2)
  da_Rt[nrow(da)] = da_Rt[nrow(da)-1]
  
  p_Rt <- ggplot(da, aes(x = X_lim, y = da_Rt)) + 
    geom_line() +
    labs(title="Rt", x="Time (day)", y="Number of Students and Staffs")
  
  #########
  return(list(p_1, p_2, p_Rt))
  return(length(da))
  
}
###################################################################################################################

###################################################################################################################
# plots <- myfun4.3(B.Staff = 3, B.Student = 10, bubble.no = 10, 
#       BBC.Staff = 3, BBC.Student = 3, # Between bubbles contact number for each student / staff
#     Trans.Staff = 1, Trans.Student = 0.64, # transmissibility of student / staff
#     Suscep.Staff = 1, Suscep.Student = 0.79, # susceptibility of student / staff
#       Trans.In = 0.54, Trans.Out = 1/2 * 0.54, # transmission rate inside / outside the bubble
#     detect.Staff = 0.3, detect.Student = 0.1, # if a student / staff is infected, the probablity that he is symptomatic
# N    a = 0.2, e = 0.138, o = 0.75, y = 0.1)

###################################################################################################################
