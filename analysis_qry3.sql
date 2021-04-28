USE Reporting_System;
GO

SELECT Response_Date,
    DATEPART(YEAR, Response_Date) AS [Year],
    DATENAME(MONTH, Response_Date) as [Month],
    CAST(DATEPART(WEEK, Response_Date)AS NVARCHAR(2)) AS [WeekNo],
    FORMAT(Response_Date, 'ddd') as [DOW],
    CAST(DATEPART(Hour, Response_Date) AS NVARCHAR(2)) AS [Hour],
    CASE
        WHEN DATEPART(HOUR, Response_Date) BETWEEN 6 AND 18 THEN 'DAY'
        ELSE 'NIGHT'
    END AS [Shift],
    Priority_Number,
    Problem,
     CASE
        WHEN Agency_Type = 'LAW' THEN 'POLICE'
		WHEN Agency_Type = 'FIRE' AND (Problem LIKE ('%ALS%') OR Problem LIKE ('%BLS%') OR Problem IN ('CONSTRUCTION SITE INJURY','PSYCHIATRIC EMERGENCY VIOLENT','PUBLIC SERVICE EMS')) THEN 'EMS'
        WHEN Agency_Type = 'FIRE' AND NOT (Problem LIKE ('%ALS%') OR Problem LIKE ('%BLS%') OR Problem IN ('CONSTRUCTION SITE INJURY','PSYCHIATRIC EMERGENCY VIOLENT','PUBLIC SERVICE EMS')) THEN 'FIRE'
		ELSE 'DECC'
	END AS [Agency],
    CASE
		WHEN Problem LIKE 'MUTUAL%' THEN 'MUTUAL AID'
		WHEN MethodofCallRcvd IS NULL AND Problem IN ('TRAFFIC STOP', 'OCCUPIED VEHICLE CHECK', 'SUBJECT STOP', 'FLAG DOWN','OFF DUTY DETAIL','ASSIST CITIZEN DELAY','DISABLED MOTORIST','ON VIEW') THEN 'OFFICER'
		WHEN MethodOfCallRcvd = '' THEN 'Not Reported'
		WHEN MethodOfCallRcvd IS NULL AND Problem NOT IN ('TRAFFIC STOP', 'OCCUPIED VEHICLE CHECK', 'SUBJECT STOP', 'FLAG DOWN','OFF DUTY DETAIL','ASSIST CITIZEN DELAY','DISABLED MOTORIST','ON VIEW') THEN 'Not Reported'
		ELSE MethodOfCallRcvd
	END AS [Call Received],
    CASE
		WHEN Problem LIKE 'MUTUAL%' THEN ClockStartTime
		WHEN Fixed_Time_PhonePickUp IS NULL THEN Time_PhonePickUp
		ELSE Fixed_Time_PhonePickUp
	END AS [PhonePickUp],
    Fixed_Time_CallEnteredQueue,
    Time_First_Unit_Assigned,
    Fixed_Time_CallTakingComplete,
    DATEDIFF(SECOND, Fixed_Time_PhonePickUp, Fixed_Time_CallEnteredQueue) AS [T2Q],
    DATEDIFF(SECOND, Fixed_Time_CallEnteredQueue, Time_First_Unit_Assigned) AS [T2Disp],
    DATEDIFF(SECOND, Fixed_Time_PhonePickUp, Fixed_Time_CallTakingComplete) AS [ProcTime]
FROM dbo.Response_Master_Incident
WHERE Response_Date BETWEEN '2019-01-01' AND '2020-01-01'
AND CallTaking_Performed_By IN (SELECT Emp_Name FROM Personnel WHERE Emp_ID BETWEEN '4000' AND '5000')
AND (Time_First_Unit_Assigned != '' OR Time_First_Unit_Assigned IS NOT NULL)
AND Fixed_Time_PhonePickUp IS NOT NULL
AND (Call_Disposition NOT IN ('TEST1-TEST CALL', 'TST-Test Call', 'TEST-Test Call','DUPPD-Duplicate Police','DUPFD-Duplicate Call Fire','IS-SYSTEM BACK IN SERVICE','SYSTEM BACK IN SERVICE') OR Call_Disposition IS NULL)
ORDER BY Response_Date
;