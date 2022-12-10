
#explore relationship between Attrition and Daily Rate
EmployeeData%>%
  ggplot(aes(x = Attrition, y = DailyRate))+
  geom_point(color="orange", position="jitter", alpha=.5) +
  geom_boxplot(aes(fill = Attrition)) +
  labs(title="Attrition vs Daily Rate")+
  theme(plot.title = element_text(hjust = 0.5))



#explore relationship between Attrition and Education
Education_df<-EmployeeData%>%
  group_by(Attrition, Education)%>%
  summarise(totalnum=n())

Education_df

Education_df%>%
  ggplot(aes(x=Education, y=totalnum, fill=Attrition))+
  geom_bar(stat = "identity",position = "dodge")+
  theme_bw()+
  labs(title="Attrition vs Education")+
  theme(plot.title = element_text(hjust = 0.5))


#explore relationship among Education , Attrition and DailyRate
EmployeeData%>%
  ggplot(aes(x = as.factor(Education), y = DailyRate, fill=Attrition))+ 
  geom_boxplot()+
  xlab("Education")+
  ylab("Daily Rate")+
  geom_point(color="orange", position="jitter", alpha=.1) +
  labs(title="Education vs Daily Rate")+
  theme(plot.title = element_text(hjust = 0.5))


#explore relationship between Job Satisfaction and Employee Attrition
Job_Stf_df<-EmployeeData%>%
  group_by(Attrition, JobSatisfaction)%>%
  summarise(totalnum=n())

Job_Stf_df

Job_Stf_df%>%
  ggplot(aes(x=JobSatisfaction, y=totalnum, fill=Attrition))+
  geom_bar(stat = "identity",position = "dodge")+
  theme_bw()+
  labs(title="Attrition vs Job Satisfaction")+
  xlab("Job Satisfaction")+
  ylab("Total number")+
  theme(plot.title = element_text(hjust = 0.5))


#explore relationship between Employee Hourly Salary (HourlyRate) and Department
EmployeeData%>%
  ggplot(aes(x=HourlyRate,  fill=Department))+
  geom_density(alpha=.3)+
  theme_bw()+
  labs(title="Hourly Rate vs Department")+
  xlab("Hourly Rate")+
  theme(plot.title = element_text(hjust = 0.5))



#explore relationship among gender, Attrition and MonthlyRate
EmployeeData%>%
  ggplot(aes(x = Gender, y = MonthlyRate, fill=Attrition))+ 
  geom_boxplot()+
  xlab("Gender")+
  ylab("Monthly Rate")+
  geom_point(color="orange", position="jitter", alpha=.1) +
  labs(title="Gender vs Monthly Rate")+
  theme(plot.title = element_text(hjust = 0.5))










