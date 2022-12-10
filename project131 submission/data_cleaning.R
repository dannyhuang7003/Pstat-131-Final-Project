

apply(is.na(EmployeeData), MARGIN = 2, sum)

colnames(select_if(EmployeeData,is.character))

EmployeeData$BusinessTravel<-factor(EmployeeData$BusinessTravel)
EmployeeData$Department<-factor(EmployeeData$Department)
EmployeeData$EducationField<-factor(EmployeeData$EducationField)
EmployeeData$Gender<-factor(EmployeeData$Gender)
EmployeeData$JobRole<-factor(EmployeeData$JobRole)
EmployeeData$MaritalStatus<-factor(EmployeeData$MaritalStatus)
EmployeeData$Over18<-factor(EmployeeData$Over18)
EmployeeData$OverTime<-factor(EmployeeData$OverTime)

str(EmployeeData)
