/* ----------------------------------------------------------------------
  MODULE .......: msreqvrk.p
  TASK .........: check address info for owner change
  APPLICATION ..: tms
  AUTHOR .......: aam (from vrkcheck.p)
  CREATED ......: 21.02.06
  CHANGED ......: 29.03.06/aam use old query only from last 24h
                               (previously 180 days)
  Version ......: TF
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Func/cparam2.i}
{Func/timestamp.i}
{Func/vrkcheck.i}

DEF INPUT  PARAMETER iiRequest AS INT  NO-UNDO.
DEF OUTPUT PARAMETER ocError   AS CHAR NO-UNDO.

DEF VAR liError    AS INT  NO-UNDO.
DEF VAR ldStamp    AS DEC  NO-UNDO.
DEF VAR lcPersonID AS CHAR NO-UNDO.
DEF VAR lcName     AS CHAR NO-UNDO.
DEF VAR lcOldAddr  AS CHAR NO-UNDO.
DEF VAR lcAddress  AS CHAR NO-UNDO.
DEF VAR lcOthers   AS CHAR NO-UNDO.
DEF VAR liCnt      AS INT  NO-UNDO.
DEF VAR lcCOName   AS CHAR NO-UNDO.


FIND MsRequest WHERE MsRequest.MsRequest = iiRequest NO-LOCK NO-ERROR.
IF NOT AVAILABLE MsRequest THEN DO:
   ocError = "Request was not found".
   RETURN.
END.

IF MsRequest.ReqStat < 12 OR MsRequest.ReqStat > 13 THEN DO:
   ocError = "Request is not in a valid state for SAT check".
   RETURN.
END. 

ASSIGN ldStamp    = fHMS2TS(TODAY - 1,STRING(TIME,"hh:mm:ss"))
       lcPersonID = ENTRY(11,MsRequest.ReqCParam1,";").
   
IF lcPersonID = "" THEN DO:
   ocError = "PersonID has not been given".
   RETURN.
END. 

FIND FIRST VRKQuery WHERE 
           VRKQuery.PersonId = lcPersonId AND
           VRKQuery.CrStamp > ldStamp NO-LOCK NO-ERROR.

IF NOT AVAIL VRKQuery THEN DO:

   RUN pVRKCheck(lcPersonID,
                 -1 * MsRequest.MsRequest,
                 OUTPUT liError).
                 
   /* error occurred */
   IF liError > 0 THEN DO:
       
       IF liError = 2 THEN DO:
          ocError = "Invalid person ID".
          
          DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                     "MsRequest",
                     STRING(MsRequest.MsRequest),
                     0,
                     "VRK Failed",
                     "Invalid person ID").
          
          FIND CURRENT MsRequest EXCLUSIVE-LOCK.
          ASSIGN MsRequest.ReqDParam1 = 2
                 MsRequest.ReqStatus  = 13.
          RELEASE MsRequest.
       END.
       ELSE ocError = "VRK check failed".
       
       RETURN.
   END. 
       
END.                    

/* update checked values to request */

ASSIGN lcName    = VrkQuery.Lastname  + ";" +
                   VrkQuery.FirstName 
       lcCOName  = ENTRY(3,MsRequest.ReqCParam1,";")
       lcOldAddr = ENTRY(4,MsRequest.ReqCParam1,";") + ";" + 
                   ENTRY(5,MsRequest.ReqCParam1,";") + ";" +
                   ENTRY(6,MsRequest.ReqCParam1,";") + ";" +
                   ENTRY(7,MsRequest.ReqCParam1,";").
         
/* temporary address */
IF VRKQuery.TempFrom NE ?     AND
   VRKQuery.TempFrom <= TODAY AND 
  (VRKQuery.TempTo = ? OR
   VRKQuery.TempTo > TODAY) 
THEN lcAddress = VrkQuery.TempAddress + ";" + 
                 VrkQuery.TempZip     + ";" + 
                 VrkQuery.TempPoffice.

/* permanent address */
ELSE lcAddress = (IF VrkQuery.Address > "" 
                  THEN VrkQuery.Address
                  ELSE ENTRY(4,MsRequest.ReqCParam1,";")) + ";" + 
                 (IF VrkQuery.Zipcode > ""
                  THEN VrkQuery.Zipcode
                  ELSE ENTRY(5,MsRequest.ReqCParam1,";")) + ";" +
                 (IF VrkQuery.PostOffice > ""
                  THEN VrkQuery.PostOffice
                  ELSE ENTRY(6,MsRequest.ReqCParam1,";")).
                  
/* country from request */                  
lcAddress = lcAddress + ";" + ENTRY(7,MsRequest.ReqCParam1,";").

/* if address has changed then empty old co-name */
IF lcAddress NE lcOldAddr THEN lcCOName = "".

DO liCnt = 8 TO NUM-ENTRIES(MsRequest.ReqCParam1,";"):
    lcOthers = lcOthers + ";" + ENTRY(liCnt,MsRequest.ReqCParam1,";").
END.

FIND CURRENT MsRequest EXCLUSIVE-LOCK.
ASSIGN MsRequest.ReqCParam1 = lcName + ";" + lcCOName + ";" + 
                              lcAddress + lcOthers
       MsRequest.ReqDParam1 = 1.
       
/* deceased */
IF VRKQuery.DeathDay NE ? THEN DO:
   ASSIGN
     ocError = "Person has died on " + STRING(VRKQuery.DeathDay,"99.99.9999")
     MsRequest.ReqDParam1 = 3. 

   DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
              "MsRequest",
              STRING(MsRequest.MsRequest),
              0,
              "VRK",
              "Person has died on " + STRING(VRKQuery.DeathDay,"99.99.9999")).
END.

/* check failed */
IF MsRequest.ReqDParam1 > 1 THEN MsRequest.ReqStat = 13.

/* both vrk and sat have been succesful */
ELSE IF MsRequest.ReqDParam2 <= 1 THEN MsRequest.ReqStat = 12.


RELEASE MsRequest.


