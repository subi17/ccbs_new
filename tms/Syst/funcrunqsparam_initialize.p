/* ----------------------------------------------------------------------
  MODULE .......: FuncRunQSParam_initialize.p
  TASK .........: Initialize parameters for scheduling
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 06.05.10
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

{Syst/commali.i} 
{Func/timestamp.i}
{Syst/funcrunqsparam_initialize.i}

DEF INPUT PARAMETER iiFRQScheduleID AS INT NO-UNDO.
   
DEF VAR ldaRunDate AS DATE NO-UNDO.
DEF VAR liRunTime  AS INT  NO-UNDO.
DEF VAR ldaPrevEnd AS DATE NO-UNDO.
DEF VAR lcDefaultValue AS CHAR NO-UNDO.

DEF BUFFER bQueueSched FOR FuncRunQSchedule.


FIND FIRST bQueueSched WHERE bQueueSched.FRQScheduleID = iiFRQScheduleID
   NO-LOCK NO-ERROR.
IF NOT AVAILABLE bQueueSched OR 
   LOOKUP(bQueueSched.RunState,"Scheduled,Initialized,Running") = 0 THEN 
   RETURN ERROR.
   
fSplitTS(bQueueSched.StartTS,
         OUTPUT ldaRunDate,
         OUTPUT liRunTime).
     
ldaPrevEnd = DATE(MONTH(ldaRunDate),1,YEAR(ldaRunDate)) - 1.
            
FOR EACH FuncRunQRow NO-LOCK WHERE
         FuncRunQRow.FRQueueID = bQueueSched.FRQueueID,
    EACH FuncRunParam NO-LOCK WHERE
         FuncRunParam.FRConfigID = FuncRunQRow.FRConfigID:
             
   FIND FIRST FuncRunQSParam WHERE
         FuncRunQSParam.FRQScheduleID = bQueueSched.FRQScheduleID AND
         FuncRunQSParam.FRQRowSeq = FuncRunQRow.FRQRowSeq AND
         FuncRunQSParam.ParamSeq = FuncRunParam.ParamSeq NO-LOCK NO-ERROR.
   IF AVAILABLE FuncRunQSParam THEN DO:
      IF NOT (LOOKUP(FuncRunQSParam.ParamType,"Date,Integer") > 0 AND 
              INDEX(FuncRunQSParam.CharParam,"#") > 0)
      THEN NEXT.

      FIND CURRENT FuncRunQSParam EXCLUSIVE-LOCK.
      ASSIGN 
         lcDefaultValue = FuncRunQSParam.CharParam
         FuncRunQSParam.CharParam = "".
   END.
      
   IF NOT AVAILABLE FuncRunQSParam THEN DO:    
      CREATE FuncRunQSParam.
      ASSIGN
         FuncRunQSParam.FRQueueID     = bQueueSched.FRQueueID
         FuncRunQSParam.FRQScheduleID = bQueueSched.FRQScheduleID
         FuncRunQSParam.FRQRowSeq     = FuncRunQRow.FRQRowSeq
         FuncRunQSParam.FRConfigID    = FuncRunParam.FRConfigID
         FuncRunQSParam.ParamSeq      = FuncRunParam.ParamSeq
         FuncRunQSParam.ParamType     = FuncRunParam.ParamType
         lcDefaultValue               = FuncRunParam.DefaultValue.
   END.
   
   IF lcDefaultValue > "" THEN 
   CASE FuncRunQSParam.ParamType:
   WHEN "Date"      THEN DO:
      FuncRunQSParam.DateParam = fDateValue(lcDefaultValue,
                                            ldaRunDate).
   END.
   WHEN "Decimal"   THEN DO:
      FuncRunQSParam.DecParam = DEC(lcDefaultValue) NO-ERROR.
   END.
   WHEN "Character" THEN DO:
      FuncRunQSParam.CharParam = lcDefaultValue.
   END.
   WHEN "Logical"   THEN DO:
      FuncRunQSParam.LogParam = 
         (LOOKUP(lcDefaultValue,"yes,1,true") > 0).
   END.
   WHEN "Integer"   THEN DO:
      FuncRunQSParam.IntParam = fIntegerValue(lcDefaultValue,
                                              ldaRunDate).
   END.
   END CASE. 

END.

