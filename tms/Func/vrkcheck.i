/* ----------------------------------------------------------------------
  MODULE .......: vrkcheck.i
  TASK .........: check address info 
  APPLICATION ..: tms
  AUTHOR .......: tk
  CREATED ......: 26.04.04
  CHANGED ......: 21.02.06/aam separated from vrkcheck.p
                               (called from msreqvrk.p also)
  Version ......: SHARK
  ---------------------------------------------------------------------- */

{timestamp.i}

DEF STREAM sErrLog.

PROCEDURE pVRKCheck:
   
   DEF INPUT  PARAMETER icPersonID AS CHAR NO-UNDO.
   DEF INPUT  PARAMETER iiKey      AS INT  NO-UNDO.
   DEF OUTPUT PARAMETER oiError    AS INT  NO-UNDO.
   
   def var cMessage  as char no-undo.
   def var cResponse as char no-undo.
   def var cdate1    as char no-undo.
   def var cdate2    as char no-undo.

   IF icPersonID = "" THEN DO:
      oiError = 2.
      RETURN.
   END.
   
   /* VRK */
   cMessage = "AT4GET2L " + icPersonID.
   
   run atpigw(cMessage, output cResponse).

   oiError = 0.
        
   /* erroneous response */
   if cResponse begins "ERR" or 
      cResponse begins "AT4GET2L ERROR" THEN DO:
   
      /* write to log */
      OUTPUT STREAM sErrLog TO 
         VALUE("/apps/tms/eventlog/vrk_" +
               STRING(YEAR(TODAY),"9999") +
               STRING(MONTH(TODAY),"99")  +
               STRING(DAY(TODAY),"99") + ".log") APPEND.   
      PUT STREAM sErrLog UNFORMATTED
         iiKey                   CHR(9)
         TODAY                   CHR(9)
         STRING(TIME,"hh:mm:ss") CHR(9)
         cMessage                CHR(9)
         cResponse               SKIP.
      OUTPUT STREAM sErrLog CLOSE.
 
      if cResponse begins "AT4GET2L ERROR|020" OR
         /* invalid personid */
         cResponse begins "AT4GET2L ERROR|070"            
      THEN oiError = 2.
      ELSE oiError = 1.
   
      IF not session:batch then message "Check failed" skip cResponse
      view-as alert-box.    

      RETURN. 
   end.

   cResponse = entry(2,cResponse,"|").

   CREATE VRKQuery.

   assign 
      VRKQuery.CrStamp      = fMakeTS()
      VRKQuery.PersonId     = trim(substr(cResponse,1,11))
      VRKQuery.Lastname     = trim(substr(cResponse,12,50))
      VRKQuery.Firstname    = trim(substr(cResponse,62,50)) 
      VRKQuery.HomeCode     = trim(substr(cResponse,112,3)) 
      VRKQuery.HomeName     = trim(substr(cResponse,115,100)) 
      VRKQuery.Address      = trim(substr(cResponse,215,50)) 
      VRKQuery.Zipcode      = trim(substr(cResponse,265,5)) 
      VRKQuery.Postoffice   = trim(substr(cResponse,270,50))
      VRKQuery.TempAddress  = trim(substr(cResponse,328,50)) 
      VRKQuery.TempZip      = trim(substr(cResponse,378,5)) 
      VRKQuery.TempPOffice  = trim(substr(cResponse,383,50))
      VRKQuery.Language     = trim(substr(cResponse,449,2)) 
      VRKQuery.LangName     = trim(substr(cResponse,451,30)) 
      VRKQuery.Trusteeship  = trim(substr(cResponse,489,1))
      VRKQuery.CompLimit    = trim(substr(cResponse,490,1))
      VRKQuery.TshipTxt     = trim(substr(cResponse,491,100))
      VRKQuery.CompLimitTxt = trim(substr(cResponse,591,100)).
   
   assign
      cdate1 = trim(substr(cResponse,433,8)) /* tempfrom */
      cdate2 = trim(substr(cResponse,441,8)) /* tempto */
      VRKQuery.TempFrom = ?
      VRKQuery.TempTo   = ?.

   if cdate1 ne "" then do:
      VRKQuery.TempFrom = date(int(substr(cdate1,3,2)), 
                               int(substr(cdate1,1,2)),
                               int(substr(cdate1,5,4))) no-error.
      if cdate2 ne "" then do:             
                
         VRKQuery.TempTo = date(int(substr(cdate2,3,2)), 
                                int(substr(cdate2,1,2)),
                                int(substr(cdate2,5,4))) no-error. 
      end.
   end.

   ASSIGN   
      cdate1 = trim(substr(cResponse,320,8))  /* movingdate */
      cdate2 = trim(substr(cResponse,481,8)). /* deathday */

   if cdate1 ne "" then 
      VRKQuery.MovingDate = date(int(substr(cdate1,3,2)), 
                                 int(substr(cdate1,1,2)),
                                 int(substr(cdate1,5,4))) no-error.
   if cdate2 ne "" then              
      VRKQuery.DeathDay = date(int(substr(cdate2,3,2)), 
                               int(substr(cdate2,1,2)),
                               int(substr(cdate2,5,4))) no-error. 

END PROCEDURE.

 
