/*
   CHANGED: 30.07.07. kl Sums per errorcode
            01.10.09  mk PAYTYPE= added
*/

{Syst/commpaa.i}
ASSIGN
   gcBrand = "1"
   katun   = "cron".
   
{Func/timestamp.i}
{Func/email.i}
{Func/cparam2.i}
{Func/ftransdir.i}
{Func/multitenantfunc.i}

DEFINE VARIABLE lcTemp            AS CHARACTER NO-UNDO.
DEFINE VARIABLE liCount           AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcCliType         AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcAction          AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liTotal           AS INTEGER   NO-UNDO. 
DEFINE VARIABLE liOk              AS INTEGER   NO-UNDO.
DEFINE VARIABLE liErr             AS INTEGER   NO-UNDO. 
DEFINE VARIABLE lcLoop            AS CHARACTER NO-UNDO EXTENT 2.
DEFINE VARIABLE liLoop            AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcICC             AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcImsi            AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcRepFile         AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcAmtFile         AS CHARACTER NO-UNDO. 
DEFINE VARIABLE ldFrom            AS DECIMAL   NO-UNDO.
DEFINE VARIABLE ldTo              AS DECIMAL   NO-UNDO. 
DEFINE VARIABLE lcAddressListFile AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcMailMessageDest AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcAttachment      AS CHARACTER NO-UNDO.
DEFINE VARIABLE liIdx1            AS INTEGER   NO-UNDO.
DEFINE VARIABLE liIdx2            AS INTEGER   NO-UNDO.
DEFINE VARIABLE liErrorNum        AS INTEGER   NO-UNDO.
DEFINE VARIABLE liLoop1           AS INTEGER   NO-UNDO.
DEFINE VARIABLE liLoop2           AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcMailDir         AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcRepDir          AS CHARACTER NO-UNDO.
DEFINE VARIABLE liStatus          AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcPentahoFile     AS CHARACTER NO-UNDO. 
DEFINE VARIABLE ldActivDate       AS DATE      NO-UNDO.
DEFINE VARIABLE liActivTime       AS INTEGER   NO-UNDO. 
DEFINE VARIABLE lcPentahoSpool    AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcPentahoRep      AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcTenant          AS CHARACTER NO-UNDO.

ASSIGN
   lcMailDir = fCParamC("RepConfDir")
   lcRepDir = fCParamC("SologRepDir")
   lcPentahoSpool = fCParamC("PentahoSpool")
   lcPentahoRep = fCParamC("PentahoSolog")
   lcTenant = CAPS(fgetBrandNamebyTenantId(TENANT-ID(LDBNAME(1)))).
lcAddressListFile = lcMailDir + "/sologrep.email".

GetRecipients(lcAddressListFile).
xMailSubj = lcTenant + "_Solog_report_" + STRING(DAY(TODAY))   + 
                              STRING(MONTH(TODAY)) +
                              STRING(YEAR(TODAY)).

DEFINE TEMP-TABLE ttSolog NO-UNDO
   FIELD SologId      AS INTEGER
   FIELD Action       AS CHARACTER
   FIELD ActivTs      AS DECIMAL
   FIELD CliType      AS CHARACTER
   /* Solog.Response OK = TRUE, ERROR = FALSE */
   FIELD Response     AS LOGICAL
   FIELD RespLine     AS CHAR
   FIELD ErrorNum     AS INTEGER

   INDEX SologId  SologId
   INDEX ErrorNum ErrorNum.


DEFINE TEMP-TABLE ttSologSum NO-UNDO
   FIELD ErrorNum     AS INTEGER
   FIELD CliType      AS CHARACTER
   FIELD Action       AS CHARACTER
   FIELD ActivDate      AS DATE
   FIELD Amount       AS INTEGER
   INDEX SologSum ErrorNum CLIType Action ActivDate .


IF lcRepDir = "" OR lcRepDir = ? THEN lcRepDir = "/tmp".

ldFrom =  fHMS2TS(TODAY - 1,STRING(25119,"HH:MM:SS")).
ldTo =  fHMS2TS(TODAY ,STRING(25119,"HH:MM:SS")).


lcAmtFile = lcRepDir + "/" + lctenant + "_Solog_report_" + 
                                          STRING(DAY(TODAY))   +
                                          STRING(MONTH(TODAY)) +
                                          STRING(YEAR(TODAY))  +
            ".txt".

lcRepFile = lcRepDir + "/" + lctenant + "_Failed_Sologs_" + 
                                          STRING(DAY(TODAY))   + 
                                          STRING(MONTH(TODAY)) +
                                          STRING(YEAR(TODAY))  +
            ".txt". 

lcPentahoFile = lcPentahoSpool + "/" + lctenant + "_Pentaho_Sologs_" + 
                                                    STRING(DAY(TODAY))   + 
                                                    STRING(MONTH(TODAY)) +
                                                    STRING(YEAR(TODAY))  +
            ".txt". 

DEFINE STREAM osDumpAmount.
DEFINE STREAM osDumpReport.
DEFINE STREAM osDumpPent.

DO liStatus = 1 TO 10:

FOR EACH Solog NO-LOCK USE-INDEX Stat WHERE 
         Solog.Brand = gcBrand  AND
         Solog.Stat  = liStatus AND
         Solog.ActivationTs       >= ldFrom AND
         Solog.ActivationTs       < ldTo    AND
  (INDEX(Solog.CommLine,"CREATE") > 0      OR
   INDEX(Solog.CommLine,"DELETE") > 0):

   IF Solog.Response = "" THEN NEXT.

   /* Set CliType according to Solog Command line */
   IF      INDEX(Solog.CommLine,"PAYTYPE=PREPAID")  > 0 THEN lcCliType = "Prepaid".
   ELSE IF INDEX(Solog.CommLine,"PAYTYPE=POSTPAID") > 0 THEN lcCliType = "Postpaid".
   
   /* Set Action according to Solog Command line */
   IF      INDEX(Solog.CommLine,"CREATE") > 0 THEN lcAction = "CREATE".
   ELSE IF INDEX(Solog.CommLine,"DELETE") > 0 THEN lcAction = "DELETE".
   
   FIND FIRST ttSolog WHERE 
              ttSolog.SologId = Solog.Solog
   NO-ERROR.
   
   IF NOT AVAILABLE ttSolog THEN DO:
      
      /* Create solog information */
      
      CREATE ttSolog.
      ASSIGN
         ttSolog.SologId  = Solog.Solog
         ttSolog.Action   = lcAction.
             
      ASSIGN
         ttSolog.ActivTS  = Solog.ActivationTs
         ttSolog.CliType  = lcCliType.
      
      IF Solog.Response NE "OK" THEN DO:
         
         liLoop2 = NUM-ENTRIES(SoLog.Response," ").

         DO liLoop1 = 1 TO liLoop2:

            lIErrorNum = INTEGER(ENTRY(liLoop1,SoLog.Response," ")) NO-ERROR.
     
            IF NOT ERROR-STATUS:ERROR THEN liLoop1 = liLoop2 + 1.
     
         END.
         
         ASSIGN
            ttSolog.Response = FALSE
            ttSolog.RespLine = Solog.Response
            ttSolog.ErrorNum = liErrorNum
            liIdx1           = INDEX(ttSolog.RespLine,"KI=").
      
         IF liIdx1 > 0 THEN DO:
            
            liIdx2 = liIdx1.
         
            DO WHILE SUBSTR(ttSoLog.RespLine,liIdx2,1) NE "":
               liIdx2 = liIdx2 + 1.
            END.

            SUBSTR(ttSoLog.RespLine,liIdx1,liIdx2 - liIdx1) = "".

         END.
         
      END.
      
      ELSE IF INDEX(Solog.Response, "OK") > 0 THEN ttSolog.Response = TRUE.

      /* update summary */
      fSplitTS(ttSolog.ActivTS,
               OUTPUT ldActivDate,
               OUTPUT liActivTime).

      FIND ttSologSum WHERE
           ttSologSum.ErrorNum = ttSolog.ErrorNum AND
           ttSologSum.CliType  = ttSolog.CLiType AND
           ttSologSum.Action   = ttSolog.Action AND
           ttSologSum.ActivDate  = ldActivDate  USE-INDEX SologSum NO-LOCK NO-ERROR.
      If AVAIL ttSologSum THEN 
           FIND CURRENT ttSologSum EXCLUSIVE-LOCK NO-ERROR.
      ELSE DO:
          CREATE ttSologSum. 
          ASSIGN ttSologSum.ErrorNum = ttSolog.ErrorNum 
                 ttSologSum.CLIType  = ttSolog.CLIType 
                 ttSologSum.Action   = ttSolog.Action
                 ttSologSum.ActivDate = ldActivDate .
      END.
          ASSIGN ttSologSum.Amount = ttSologSum.Amount + 1 . 
          RELEASE ttSologSum. 
   END.

   
END.

END.

/* Create report of sologs */

OUTPUT STREAM osDumpAmount TO VALUE(lcAmtFile).

liCount = 0. 

/* Total Amount of activations */
  
FOR EACH ttSolog NO-LOCK WHERE
         ttSolog.Action = "CREATE":
 
   IF ttSolog.Response = TRUE  THEN liOk  = liOk  + 1.
   IF ttSolog.Response = FALSE THEN liErr = liErr + 1.

END.

PUT STREAM osDumpAmount UNFORMATTED 
   "Total amount of activations: " liOk + liErr SKIP.

PUT STREAM osDumpAmount UNFORMATTED
   "Amount of activations OK: " liOk SKIP.

PUT STREAM osDumpAmount UNFORMATTED 
   "Amount of activations Failed: " liErr SKIP.

IF liErr > 0 THEN DO:
   
   FOR EACH ttSolog NO-LOCK WHERE
            ttSolog.Action = "CREATE" AND
            ttSolog.ErrorNum NE 0
   BREAK BY ttSolog.ErrorNum:

      ACCUMULATE ttSolog.ErrorNum (SUB-COUNT BY ttSolog.ErrorNum).

      IF LAST-OF(ttSolog.ErrorNum) THEN PUT STREAM osDumpAmount UNFORMATTED
         "Code " + STRING(ttSolog.ErrorNum) + ":" +
                   STRING (ACCUM SUB-COUNT BY ttSolog.ErrorNum ttSolog.ErrorNum) CHR(10).
   END.

END.

/* List activations by CliType */

ASSIGN
   lcLoop[1] = "PREPAID"
   lcLoop[2] = "POSTPAID"
   liOk      = 0
   liErr     = 0.

DO liLoop = 1 TO 2:

   FOR EACH ttSolog NO-LOCK WHERE
            ttSolog.CliType = lcLoop[liLoop] AND
            ttSolog.Action  = "CREATE":

      IF ttSolog.Response = TRUE  THEN liOk  = liOk  + 1.
      IF ttSolog.Response = FALSE THEN liErr = liErr + 1.

   END.

   PUT STREAM osDumpAmount UNFORMATTED 
      "Amount of activations OK " lcLoop[liLoop] " " liOk SKIP.

   PUT STREAM osDumpAmount UNFORMATTED 
      "Amount of activations Failed " lcLoop[liLoop] " " liErr SKIP.

   IF liErr > 0 THEN DO:
   
      FOR EACH ttSolog NO-LOCK WHERE
               ttSolog.CliType = lcLoop[liLoop] AND
               ttSolog.Action  = "CREATE"       AND 
               ttSolog.ErrorNum NE 0
      BREAK BY ttSolog.ErrorNum:

         ACCUMULATE ttSolog.ErrorNum (SUB-COUNT BY ttSolog.ErrorNum).

         IF LAST-OF(ttSolog.ErrorNum) THEN PUT STREAM osDumpAmount UNFORMATTED
            "Code " + STRING(ttSolog.ErrorNum) + ":" +
                      STRING (ACCUM SUB-COUNT BY ttSolog.ErrorNum ttSolog.ErrorNum) CHR(10).
      END.

   END.

   ASSIGN
      liOk  = 0
      liErr = 0.

END.

PUT STREAM osDumpAmount UNFORMATTED SKIP(1).

ASSIGN
   liOk  = 0
   liErr = 0.

FOR EACH ttSolog NO-LOCK WHERE
         ttSolog.Action = "DELETE":
   
   IF ttSolog.Response = TRUE  THEN liOk  = liOk + 1.
   IF ttSolog.Response = FALSE THEN liErr = liErr + 1.

END.

PUT STREAM osDumpAmount UNFORMATTED
   "Total amount of de-activations:" liOk + liErr SKIP.

PUT STREAM osDumpAmount UNFORMATTED
   "Amount of de-activations OK: " liOk SKIP.

PUT STREAM osDumpAmount UNFORMATTED
   "Amount of de-activations Failed: " liErr SKIP.


IF liErr > 0 THEN DO:
   
   FOR EACH ttSolog NO-LOCK WHERE
            ttSolog.Action = "DELETE" AND
            ttSolog.ErrorNum NE 0
   BREAK BY ttSolog.ErrorNum:

      ACCUMULATE ttSolog.ErrorNum (SUB-COUNT BY ttSolog.ErrorNum).

      IF LAST-OF(ttSolog.ErrorNum) THEN PUT STREAM osDumpAmount UNFORMATTED
         "Code " + STRING(ttSolog.ErrorNum) + ":" +
                   STRING (ACCUM SUB-COUNT BY ttSolog.ErrorNum ttSolog.ErrorNum) CHR(10).
   END.

END.

ASSIGN
   liOk  = 0
   liErr = 0.

/* List activations by CliType */
DO liLoop = 1 TO 2:

   FOR EACH ttSolog NO-LOCK WHERE
            ttSolog.CliType = lcLoop[liLoop] AND
            ttSolog.Action  = "DELETE":
   
      IF ttSolog.Response = TRUE  THEN liOk  = liOk  + 1.
      IF ttSolog.Response = FALSE THEN liErr = liErr + 1.

   END.

   PUT STREAM osDumpAmount UNFORMATTED
              "Amount of de-activations OK " lcLoop[liLoop] " " liOk SKIP.

   PUT STREAM osDumpAmount UNFORMATTED
              "Amount of de-activations Failed " lcLoop[liLoop] " " liErr SKIP.

   IF liErr > 0 THEN DO:
   
      FOR EACH ttSolog NO-LOCK WHERE
               ttSolog.CliType = lcLoop[liLoop] AND
               ttSolog.Action  = "DELETE"       AND 
               ttSolog.ErrorNum NE 0
      BREAK BY ttSolog.ErrorNum:

         ACCUMULATE ttSolog.ErrorNum (SUB-COUNT BY ttSolog.ErrorNum).

         IF LAST-OF(ttSolog.ErrorNum) THEN PUT STREAM osDumpAmount UNFORMATTED
            "Code " + STRING(ttSolog.ErrorNum) + ":" +
                      STRING (ACCUM SUB-COUNT BY ttSolog.ErrorNum ttSolog.ErrorNum) CHR(10).
      END.

   END.

   ASSIGN
      liOk  = 0
      liErr = 0.

END. 

OUTPUT STREAM osDumpAmount CLOSE.

/* Create report of failed sologs */
OUTPUT STREAM osDumpReport TO VALUE(lcRepFile).

FOR EACH ttSolog NO-LOCK WHERE
         ttSolog.Response = FALSE
BY ttSolog.Action:
   
   FIND FIRST Solog WHERE 
              Solog.Solog = ttSolog.Solog AND 
              Solog.Brand = "1"         
   NO-LOCK NO-ERROR.
   
   IF AVAILABLE Solog THEN FIND FIRST Order WHERE 
                                Order.MsSeq = Solog.MsSeq AND
                                Order.OrderType < 2
                           NO-LOCK NO-ERROR.
  
   IF AVAILABLE Order THEN DO:
      FIND FIRST Imsi WHERE 
                 Imsi.ICC = Order.ICC
      NO-LOCK NO-ERROR.
      lcICC = Order.ICC.
      IF AVAILABLE Imsi THEN lcImsi = Imsi.Imsi.
      ELSE                   lcImsi = "Not Available".
   END.
   ELSE lcICC = "Not Available".

   /* Hae solog.msseq --> order.msseq -> ICC --> IMSI.ICC */
   PUT STREAM osDumpReport UNFORMATTED 
      ttSolog.SologId             CHR(9)
      ttSolog.Action              CHR(9)
      fTs2HMS(Solog.ActivationTs) CHR(9)
      ttSolog.CliType             CHR(9)
      Solog.Cli                   CHR(9)
      lcImsi                      CHR(9)
      lcICC                       CHR(9)
      ttSolog.RespLine            SKIP.

END.

OUTPUT STREAM osDumpReport CLOSE.

/* create summary report used by pentaho */
OUTPUT STREAM osDumpPent TO VALUE(lcPentahoFile).
FOR EACH ttSologSum NO-LOCK :
    PUT STREAM osDumpPent UNFORMATTED 
        STRING(ttSologSum.ActivDate) CHR(9)
        ttSologSum.ErrorNum CHR(9)
        ttSologSum.CLIType CHR(9)
        ttSologSum.Action  CHR(9)
        ttSologSum.Amount  SKIP.
END.
OUTPUT STREAM osDumpPent CLOSE.

 
lcMailMessageDest = lcAmtFile.

lcAttachment = lcRepFile.

SendMail(lcMailMessageDest,lcAttachment).

/* transfer pentaho file */
fMove2TransDir(lcPentahoFile,"",lcPentahoRep).
