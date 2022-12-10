#Feature Extraction

Subset_numeric<-select_if(EmployeeData,is.numeric)

BusinessTravel_df<-model.matrix(~BusinessTravel-1,EmployeeData) %>% as.data.frame()
Department_df<-model.matrix(~Department-1,EmployeeData) %>% as.data.frame()
EducationField_df<-model.matrix(~EducationField-1,EmployeeData) %>%as.data.frame()
Gender_df<-model.matrix(~Gender-1,EmployeeData) %>% as.data.frame()
JobRole_df<-model.matrix(~JobRole-1,EmployeeData) %>% as.data.frame()
MaritalStatus_df<-model.matrix(~MaritalStatus-1,EmployeeData) %>% as.data.frame()
OverTime_df<-model.matrix(~OverTime-1,EmployeeData) %>% as.data.frame()

EmployeeNew<-cbind(Subset_numeric, BusinessTravel_df, Department_df,
                   EducationField_df, Gender_df, JobRole_df, 
                   MaritalStatus_df, OverTime_df)

EmployeeNew$Attrition<-EmployeeData$Attrition

dim(EmployeeNew)
head(EmployeeNew)