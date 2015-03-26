/* ----------------------------------------------------------------------
  MODULE .......: postfilec
  TASK .........: Create a file to Post from new/deleted customers
                  (for address updates)
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 25.01.05
  CHANGED ......: 
  Version ......: M15
  ---------------------------------------------------------------------- */

{commali.i}
{cparam2.i}
{ftransdir.i}
{timestamp.i}

DEF OUTPUT PARAMETER oiDoneAdd AS INT  NO-UNDO. 
DEF OUTPUT PARAMETER oiDoneDel AS INT  NO-UNDO. 
DEF OUTPUT PARAMETER ocInfo    AS CHAR NO-UNDO. 

DEF VAR lcAddFile   AS CHAR NO-UNDO.
DEF VAR lcDelFile   AS CHAR NO-UNDO.
DEF VAR lcTransDir  AS CHAR NO-UNDO. 
DEF VAR liCnt       AS INT  NO-UNDO. 
DEF VAR lcFileExt   AS CHAR NO-UNDO INIT ".txt".
DEF VAR lcNewLine   AS CHAR NO-UNDO. 
DEF VAR ldtSent     AS DATE NO-UNDO. 
DEF VAR ldStamp     AS DEC  NO-UNDO. 
DEF VAR ldtTo       AS DATE NO-UNDO. 
DEF VAR lcSocSecID  AS CHAR NO-UNDO.
DEF VAR liCust      AS INT  NO-UNDO. 
 
DEF TEMP-TABLE ttCust NO-UNDO
   FIELD CustNum  AS INT
   FIELD ActType  AS INT
   FIELD ActDate  AS DATE
   FIELD ActStamp AS DEC
   INDEX CustNum CustNum
   INDEX ActType ActType CustNum.
   
DEF STREAM sFile.

FUNCTION fDateConv RETURNS CHARACTER
   (idtDate AS DATE).

   RETURN STRING(YEAR(idtDate),"9999")  +
          STRING(MONTH(idtDate),"99") +
          STRING(DAY(idtDate),"99").
END FUNCTION.

FUNCTION fSocialSecID RETURN CHARACTER.

   DEF VAR lcSocID    AS CHAR NO-UNDO. 
   DEF VAR ldtSocDate AS DATE NO-UNDO.
   
   lcSocID = "".
   
   IF Customer.OrgId > "" AND
      Customer.Category NE "2" 
   THEN DO:
      /* simple check */
      ASSIGN lcSocID    = RIGHT-TRIM(Customer.OrgId)
             ldtSocDate = fHetu2Date(lcSocID).
      
      IF ldtSocDate = ? THEN lcSocID = "".
   END.
 
   RETURN lcSocID.
   
END FUNCTION.


FUNCTION fLocalTransDir RETURNS LOGICAL
   (icFile AS CHAR).

   DEF VAR lcExtens AS CHAR NO-UNDO.
   
   /* move the new file to the actual transfer directory */
   IF lcTransDir = "" THEN RETURN FALSE.
   
   IF SUBSTRING(icFile,LENGTH(icFile) + 1 - LENGTH(lcFileExt)) = lcFileExt
   THEN lcExtens = lcFileExt.
   ELSE lcExtens = "".
   
   IF NOT fTransDir(icFile,
                    lcExtens,
                    lcTransDir)
   THEN DO:
      ocInfo = ocInfo + 
               "File could not be moved to transfer directory (" +
               lcTransDir + ") ".
      RETURN FALSE.         
   END. 
               
   ELSE RETURN TRUE. 
   
END FUNCTION.

FUNCTION fNewCustomer RETURNS LOGICAL
   (idtCreated AS DATE).

   DEF VAR lcLastName  AS CHAR NO-UNDO.
   DEF VAR lcFirstName AS CHAR NO-UNDO.
   DEF VAR liCnt       AS INT  NO-UNDO.
   
   /* skip some customers */
   IF Customer.CustName MATCHES("*testaaja*") THEN RETURN FALSE.
   
   lcLastName = Customer.CustName.
   IF Customer.FirstName = "" THEN DO:
      IF Customer.COName > "" THEN 
         lcLastName  = lcLastName + " " + Customer.COName.
      lcFirstName = "".
   END.
   ELSE lcFirstName = Customer.FirstName.
      
   lcSocSecId = fSocialSecID().

   /* when customer was created */
   IF idtCreated = ? THEN DO:

      idtCreated = 5/1/2004.

      FOR FIRST EventLog NO-LOCK WHERE
             EventLog.TableName = "Customer" AND
             EventLog.Key       = STRING(Customer.CustNum) AND
             EventLog.Action    = "Create":
         idtCreated = EventLog.EventDate.
      END.
   END.
   
   PUT STREAM sFile 
      (lcSocSecID > "")        FORMAT "H/"
      lcSocSecID               FORMAT "X(11)"
      SPACE(12) 
      STRING(Customer.CustNum) FORMAT "X(25)"
      SPACE(10)
      lcLastName               FORMAT "X(30)"
      lcFirstName              FORMAT "X(30)"
      SPACE(20)
      Customer.Phone           FORMAT "X(20)"
      fDateConv(idtCreated)    FORMAT "X(8)" 
      Customer.ZipCode         FORMAT "X(5)"
      Customer.PostOffice      FORMAT "X(30)"
      Customer.Address         FORMAT "X(50)"
      SPACE(68)
      SKIP.

   RETURN TRUE.
   
END FUNCTION.

ASSIGN lcAddFile  = fCParamC("PostFileCustA") 
       lcDelFile  = fCParamC("PostFileCustD")
       lcTransDir = fCParamC("PostDirCust")
       /* date of last send */
       ldtSent    = fCParamDA("PostFileSent")
       lcNewLine  = CHR(13) + CHR(10).
       
/* gather events from last full day */
ldtTo = TODAY - 1.

IF lcAddFile = "" OR lcAddFile = ? OR
   lcDelFile = "" OR lcDelFile = ? OR
   lcTransDir = "" OR lcTransDir = ?
THEN DO:
   ocInfo = "File definitions incomplete".
   RETURN.
END.

liCnt = INDEX(lcAddFile,lcFileExt).
IF liCnt > 0 
THEN lcAddFile = SUBSTRING(lcAddFile,1,liCnt - 1) +
                 "_" + fDateConv(TODAY) + 
                 SUBSTRING(lcAddFile,liCnt).
ELSE lcAddFile = lcAddFile + "_" + fDateConv(TODAY).              

liCnt = INDEX(lcDelFile,lcFileExt).
IF liCnt > 0 
THEN lcDelFile = SUBSTRING(lcDelFile,1,liCnt - 1) +
                 "_" + fDateConv(TODAY) + 
                 SUBSTRING(lcDelFile,liCnt).
ELSE lcDelFile = lcDelFile + "_" + fDateConv(TODAY).              


/* if last send date is unknown, then send all customers as new ones */
IF ldtSent = ? THEN DO:

   OUTPUT STREAM sFile TO VALUE(lcAddFile).
   
   FOR EACH Customer NO-LOCK WHERE
            Customer.Brand    = gcBrand AND
            Customer.CustNum >= 10006 
   BY Customer.CustNum:
   
      IF fNewCustomer(?) THEN oiDoneAdd = oiDoneAdd + 1.
   END.         

   OUTPUT STREAM sFile CLOSE.
    
   /* move the new file to the actual transfer directory */
   fLocalTransDir(lcAddFile).

END.

ELSE DO:

   /* gather add/delete events from eventlog */
   FOR EACH EventLog NO-LOCK USE-INDEX EventDate WHERE
            /* better to get all events from given time period than
               all customer events throughout the history */
            EventLog.EventDate  > ldtSent    AND
            EventLog.TableName  = "Customer":

      liCust = INTEGER(EventLog.Key) NO-ERROR.
      IF ERROR-STATUS:ERROR OR liCust < 10006 THEN NEXT.
      
      IF LOOKUP(EventLog.Action,"Create,Delete") = 0 THEN NEXT.
      
      ldStamp = fHMS2TS(EventLog.EventDate,
                        EventLog.EventTime).
                      
      IF EventLog.EventDate > ldtTo THEN NEXT. 
                        
      FIND FIRST ttCust WHERE ttCust.CustNum = INTEGER(EventLog.Key) NO-ERROR.
      IF AVAILABLE ttCust THEN DO:

         IF ttCust.ActType = 9 THEN NEXT.
         
         /* customer may have been added and deleted during this period */
         IF EventLog.Action = "Create" AND 
            ttCust.ActType = 0 
         THEN DO:
            IF ldStamp < ttCust.ActStamp 
            THEN ttCust.ActType = 9. 
            ELSE ASSIGN ttCust.ActType  = 1
                        ttCust.ActStamp = ldStamp
                        ttCust.ActDate  = EventLog.EventDate.
         END.

         /* or vice versa */
         ELSE IF EventLog.Action = "Delete" AND
                 ttCust.ActType = 1
         THEN DO:
            IF ldStamp > ttCust.ActStamp
            THEN ttCust.ActType = 9.
         END.
         
         /* this should not be possible */
         ELSE IF ldStamp > ttCust.ActStamp
         THEN ASSIGN ttCust.ActStamp = ldStamp
                     ttCust.ActDate  = EventLog.EventDate.
         
      END.
      
      ELSE DO:
        
         CREATE ttCust.
         ASSIGN ttCust.CustNum  = INTEGER(EventLog.Key)
                ttCust.ActStamp = ldStamp
                ttCust.ActType  = INTEGER(EventLog.Action = "Create")
                ttCust.ActDate  = EventLog.EventDate.
      END.
      
   END.

   /* add events */
   IF CAN-FIND(FIRST ttCust WHERE ttCust.ActType = 1) THEN DO:
   
      OUTPUT STREAM sFile TO VALUE(lcAddFile).
       
      FOR EACH ttCust WHERE
               ttCust.ActType = 1,
         FIRST Customer NO-LOCK WHERE
               Customer.CustNum = ttCust.CustNum:
   
         IF fNewCustomer(ttCust.ActDate) THEN oiDoneAdd = oiDoneAdd + 1.
      END.

      OUTPUT STREAM sFile CLOSE.
    
      /* move the new file to the actual transfer directory */
      fLocalTransDir(lcAddFile).
   END.
   
   /* delete events */
   IF CAN-FIND(FIRST ttCust WHERE ttCust.ActType = 0) THEN DO:
   
      OUTPUT STREAM sFile TO VALUE(lcDelFile).
       
      FOR EACH ttCust WHERE
               ttCust.ActType = 0:
   
         PUT STREAM sFile
            lcSocSecID               FORMAT "X(11)"
            SPACE(2) 
            STRING(Customer.CustNum) FORMAT "X(25)"
            "P"
            SKIP.
            
         oiDoneDel = oiDoneDel + 1.
      END.

      OUTPUT STREAM sFile CLOSE.
    
      /* move the new file to the actual transfer directory */
      fLocalTransDir(lcDelFile).
   END.
 
END. 

/* mark last day of which events have been handled */
DO TRANS:
   FIND FIRST TMSParam EXCLUSIVE-LOCK WHERE
              TMSParam.Brand     = gcBrand AND
              TMSParam.ParamCode = "PostFileSent" NO-ERROR.
   IF NOT AVAILABLE TMSParam THEN DO:
      CREATE TMSParam.
      ASSIGN TMSParam.Brand      = gcBrand
             TMSParam.ParamCode  = "PostFileSent"
             TMSParam.ParamGroup = "Post"
             TMSParam.ParamName  = "Latest day when file was sent"
             TMSParam.ParamType  = "DA".
   END.
   TMSParam.DateVal = ldtTo.
   
   RELEASE TMSParam.
END.




  
   
   
   
   
