{Syst/commali.i}
{Func/timestamp.i}
{Func/filltemptable.i}
{Func/cparam2.i}

DEF TEMP-TABLE ttDB NO-UNDO
   FIELD ConnName  AS CHAR
   FIELD TableName AS CHAR
   FIELD ConnParam AS CHAR
   FIELD Connect   AS LOG
   FIELD DBOrder   AS INT
   INDEX ConnName ConnName TableName.

DEF TEMP-TABLE ttInvCust
   FIELD CustNum LIKE Customer.InvCust
   INDEX CustNum CustNum.

DEF TEMP-TABLE ttCLI
   FIELD ttCli AS CHAR FORMAT "X(18)" 
   INDEX ttCli ttCli.

DEFINE STREAM sFile.
DEFINE STREAM sLine.
                                 
DEF VAR lcQueryCase       AS CHAR NO-UNDO.
DEF VAR ldaActiveMobCDR   AS DATE NO-UNDO.
DEF VAR ldaActiveMobDtl   AS DATE NO-UNDO.
DEF VAR ldaActivePPCDR    AS DATE NO-UNDO.
DEF VAR ldaActivePPDtl    AS DATE NO-UNDO.
DEF VAR ldaActiveErrorCDR AS DATE NO-UNDO.
DEF VAR ldaActiveErrorDtl AS DATE NO-UNDO.
DEF VAR ldaActiveFraudDtl AS DATE NO-UNDO.
DEF VAR ldaActiveEdrDtl   AS DATE NO-UNDO.
DEF VAR ldaEndMobCDR      AS DATE NO-UNDO.
DEF VAR ldaEndMobDtl      AS DATE NO-UNDO.
DEF VAR ldaEndFraudDtl    AS DATE NO-UNDO.
DEF VAR ldaEndEdrDtl    AS DATE NO-UNDO.
DEF VAR ldaEndPPCDR       AS DATE NO-UNDO.
DEF VAR ldaEndPPDtl       AS DATE NO-UNDO.
DEF VAR ldaEndErrorCDR    AS DATE NO-UNDO.
DEF VAR ldaEndErrorDtl    AS DATE NO-UNDO.



FUNCTION fCallQuery RETURNS CHARACTER 
  (INPUT  idtStartDate  AS DATE,
   INPUT  idtEndDate    AS DATE,
   INPUT  iiCustNum     AS INTEGER,
   INPUT  icCustRole    AS CHARACTER,
   INPUT  icCLI         AS CHARACTER,
   INPUT  iiInvSeq      AS INTEGER,
   INPUT  icBillCode    AS CHARACTER,
   INPUT  icReasonCode  AS CHARACTER,
   INPUT  iiErrorCode   AS INTEGER,
   OUTPUT oiErrorCode   AS INTEGER,
   OUTPUT ocErrorQuery  AS CHARACTER).

   DEFINE VARIABLE ldaDate       AS DATE      NO-UNDO.
   DEFINE VARIABLE ldaXDate      AS DATE      NO-UNDO.
   DEFINE VARIABLE liInvSeq      AS INTEGER   NO-UNDO.
   DEFINE VARIABLE liCustNum     AS INTEGER   NO-UNDO.
   DEFINE VARIABLE liLoop1       AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lcStart       AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcEnd         AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcQuery       AS CHARACTER NO-UNDO INIT "ERROR".
   
   DEF BUFFER bQInvSeq FOR InvSeq.
   
   /****************************
   *        E R R O R S        *
   *  1 = Invalid StartDate    *
   *  2 = Invalid ToDate       *
   *  5 = Wrong search rule    *
   *  9 = Missing Invseq       *
   ****************************/

   /* SEARCH ANALYSE */

   ASSIGN
      oiErrorCode = 0
      ocErrorQuery = "".

   IF      idtStartDate = ? THEN oiErrorCode = 1.
   ELSE IF idtEndDate   = ? THEN oiErrorCode = 2.   
 
   IF iiInvSeq > 0 THEN DO:

      FIND FIRST bQInvSeq WHERE
                 bQInvSeq.Invseq = iiInvSeq
      NO-LOCK NO-ERROR.
      
      IF NOT Avail bQInvSeq THEN DO:
         oiErrorCode = 9.
      END.
      ELSE IF icCli NE ""  AND 
              icBillCode = "" THEN lcQueryCase = "CLI_AND_InvSeq".     
      ELSE IF icCli NE ""  AND
              icBillCode NE "" THEN lcQueryCase = "InvSeq_BillCode".
      ELSE                          lcQueryCase = "InvSeq".

   END.
   ELSE IF iiErrorcode > 0  THEN lcQueryCase = "ErrorCalls".
   ELSE IF iiCustNum   > 0  THEN DO:
      CASE icCustRole:
      WHEN "inv" THEN lcQueryCase = "InvCust".
      OTHERWISE lcQueryCase = "CustNum".
      END CASE.
   END.
   ELSE IF icCli       > "" THEN lcQueryCase = "CLI".
   ELSE oiErrorCode                          = 20.

   IF oiErrorCode = 0 THEN DO:

      ASSIGN 
         lcStart   = STRING(DAY(idtStartDate))   + "/" +
                     STRING(MONTH(idtStartDate)) + "/" +
                     STRING(YEAR(idtStartDate))
  
        lcEnd      = STRING(DAY(idtEndDate))   + "/"  +
                     STRING(MONTH(idtEndDate)) + "/"  +
                     STRING(YEAR(idtEndDate))
                             
        iccli      = "'" + iccli + "'"
        icBillCode = "'" + icBillcode + "'".

      CASE lcQueryCase:

         WHEN "CLI" THEN lcQuery =
            "#TBL.cli     = " + icCLI   +    " AND " +
            "#TBL.DateST >= " + lcStart +    " AND " +
            "#TBL.DateST <= " + lcEnd .

         WHEN "InvSeq" THEN lcQuery =
            "#TBL.InvCust    = " + STRING(bQInvSeq.CustNum) +  " AND " +
            "#TBL.InvSeq     = " + STRING(iiInvseq)         +  " AND " +
            "#TBL.DateST    >= " + lcStart                  +  " AND " +
            "#TBL.DateST    <= " + lcEnd.

         WHEN "CLI_AND_InvSeq" THEN lcQuery =
           "#TBL.InvCust    = " + STRING(bQInvSeq.CustNum) +  " AND " +
           "#TBL.InvSeq     = " + STRING(bQInvSeq.Invseq)  +  " AND " +
           "#TBL.DateST    >= " + lcStart                  +  " AND " +
           "#TBL.DateST    <= " + lcEnd                    +  " AND " +
           "#TBL.CLI        = " + icCLI .                

         WHEN "InvSeq_BillCode" THEN lcQuery =
            "#TBL.InvCust    = " + STRING(bQInvSeq.CustNum) +  " AND " +
            "#TBL.InvSeq     = " + STRING(iiInvseq)         +  " AND " +
            "#TBL.DateST    >= " + lcStart                  +  " AND " +
            "#TBL.DateST    <= " + lcEnd                    +  " AND " +
            "#TBL.BillCode   = " + icBillCode.

         WHEN "Invoice" THEN lcQuery =          
            "#TBL.InvCust    = " + STRING(bQInvSeq.CustNum) +  " AND " +
            "#TBL.InvSeq     = " + STRING(bQInvSeq.Invseq)  +  " AND " +
            "#TBL.DateST    >= " + lcStart                  +  " AND " +
            "#TBL.DateST    <= " + lcEnd                    +  " AND " +
            "#TBL.BillCode   = " + icBillCode.
                
         WHEN "CustNum" THEN lcQuery =
            "#TBL.CustNum = " + STRING(iiCustNum) + " AND " +
            "#TBL.DateST >= " + lcStart +           " AND " +
            "#TBL.DateST <= " + lcEnd.
 
         WHEN "InvCust" THEN lcQuery =
            "#TBL.InvCust = " + STRING(iiCustNum) + " AND " +
            "#TBL.DateST >= " + lcStart +           " AND " +
            "#TBL.DateST <= " + lcEnd.
       
         WHEN "ErrorCalls" THEN DO:
            lcQuery = 
            "#TBL.ErrorCode = " + STRING(iiErrorCode) + " AND " + 
            "#TBL.DateST >= " + lcStart +               " AND " +
            "#TBL.DateST <= " + lcEnd.

            IF idtStartDate = idtEndDate THEN DO:
               /* if not in separate errorcdrs and a short period -> use
                  date index */
               IF NOT CAN-FIND(FIRST CDRError WHERE 
                                  CDRError.CDRError = iiErrorCode) THEN 
                  lcQuery = lcQuery + " USE-INDEX Date".
            END.
            
            IF CONNECTED("roamcdr") THEN DO:
               ocErrorQuery = lcQuery. 
               /* utilize date index over errorcode if given period is short */
               IF idtStartDate = idtEndDate AND 
                  INDEX(ocErrorQuery,"USE-INDEX") = 0 THEN 
                  ocErrorQuery = ocErrorQuery + " USE-INDEX Date".
            END.       
         END.   
      END.

   END.
   
   RETURN lcQuery.
END.

FUNCTION fGetArchiveDBs RETURNS LOGIC
   (icTable     AS CHAR,
    idaFromDate AS DATE,
    idaToDate   AS DATE):
    
   FOR EACH DBConfig NO-LOCK WHERE
            DBConfig.Brand = gcBrand AND
            DBConfig.TableName = icTable AND
            DBConfig.DBState   = 1 AND 
            DBConfig.ToDate   >= idaFromDate AND
            DBConfig.FromDate <= idaToDate:
             
      CREATE ttDB.
      ASSIGN 
         ttDB.ConnName  = DBConfig.DBConnName
         ttDB.TableName = icTable
         ttDB.ConnParam = "-H " + DBConfig.Host + " -S " + DBConfig.Service
         ttDb.Connect   = TRUE
         ttDB.DBOrder   = 1.
   END.
   
END FUNCTION.

FUNCTION fSetCollectionDBs RETURNS LOGIC
   (icDBName    AS CHAR,
    icTableName AS CHAR,
    idaFromDate AS DATE,
    idaToDate   AS DATE,
    INPUT-OUTPUT idaActive AS DATE,
    INPUT-OUTPUT idaEnd AS DATE):
    
   CREATE ttDB.
   ASSIGN
      ttDB.ConnName  = icDbName
      ttDB.TableName = icTableName
      ttDB.Connect = FALSE.

   IF idaActive = ? THEN 
   FOR FIRST DBConfig NO-LOCK WHERE
             DBConfig.Brand = gcBrand AND
             DBConfig.TableName = ttDB.TableName AND
             DBConfig.DBState = 0 AND 
             DBConfig.ToDate >= TODAY AND
             DBConfig.FromDate <= TODAY:
      ASSIGN        
         idaActive = DBConfig.FromDate
         idaEnd    = DBConfig.ToDate.
   END.   
   IF idaActive = ? THEN 
   FOR FIRST DBConfig NO-LOCK WHERE
             DBConfig.Brand = gcBrand AND
             DBConfig.TableName = ttDB.TableName AND
             DBConfig.DBState = 0:
      ASSIGN 
         idaActive = DBConfig.FromDate
         idaEnd    = DBConfig.ToDate.
   END.   

   /* if old cdrs (or newer than those in current active db) requested then
      connect to archived dbs,
      search always from active though also because there may be old unbilled 
      tickets in active db */
   IF (idaActive NE ? AND idaFromDate < idaActive) OR
      (idaEnd NE ? AND idaToDate > idaEnd) THEN
         fGetArchiveDBs(ttDB.TableName,
                        idaFromDate,
                        idaToDate).

END FUNCTION.

/* note; details for postpaid and prepaid cdrs are in the same table */
FUNCTION fGetCDRDtl RETURNS LOGICAL
   (icCDRTable AS CHAR,
    idaDate    AS DATE,
    iiDtlSeq   AS INT,
    INPUT-OUTPUT ihDtl AS HANDLE):

   DEF VAR lcDbName   AS CHAR NO-UNDO.      
   DEF VAR lcDtlQuery AS CHAR NO-UNDO.      
   DEF VAR lcDtlTable AS CHAR NO-UNDO.
   DEF VAR lcDtlDate  AS CHAR NO-UNDO.
   DEF VAR liDtlFound AS INT  NO-UNDO.
   DEF VAR ldaActive  AS DATE NO-UNDO.
   DEF VAR ldaEnd     AS DATE NO-UNDO.

   IF idaDate = ? OR iiDtlSeq = 0 THEN RETURN FALSE. 
   
   EMPTY TEMP-TABLE ttDB.
   
   CREATE ttDB.
   ASSIGN 
      ttDB.Connect = FALSE
      ttDB.DBOrder   = 2
      lcDtlDate      = STRING(DAY(idaDate))   + "/" +
                       STRING(MONTH(idaDate)) + "/" +
                       STRING(YEAR(idaDate)).

   CASE icCDRTable:
   WHEN "MobCDR" OR 
   WHEN "PrepCDR"
   THEN ASSIGN
      ttDB.ConnName  = "mcdrdtl"
      ttDB.TableName = "McdrDtl2"
      ldaActive      = ldaActiveMobDtl
      ldaEnd         = ldaEndMobDtl.
   WHEN "PrepEDR"
   THEN ASSIGN
      ttDB.ConnName  = "prepedr"
      ttDB.TableName = "EDRDtl"
      ldaActive      = ldaActiveEdrDtl
      ldaEnd         = ldaEndErrorDtl.
   WHEN "ErrorCDR" THEN ASSIGN
      ttDB.ConnName  = "roamcdr"
      ttDB.TableName = "ErrorDtl"
      ldaActive      = ldaActiveErrorDtl
      ldaEnd         = ldaEndErrorDtl.
   WHEN "FraudCDR" THEN ASSIGN
      ttDB.ConnName  = "fraudcdr"
      ttDB.TableName = "frauddtl"
      ldaActive      = ldaActiveFraudDtl
      ldaEnd         = ldaEndFraudDtl.
   END CASE.
   
   IF ldaActive = ? THEN DO:
      FOR FIRST DBConfig NO-LOCK WHERE
                DBConfig.Brand = gcBrand AND
                DBConfig.TableName = ttDB.TableName AND
                DBConfig.DBState = 0:
         ASSIGN      
            ldaActive = DBConfig.FromDate
            ldaEnd    = DBConfig.ToDate.
      END.   
      IF ldaActive = ? THEN ASSIGN 
         ldaActive = ldaActiveMobCDR
         ldaEnd    = ldaEndMobCDR.
      
      CASE ttDB.TableName:
      WHEN "McdrDtl2" THEN ASSIGN
         ldaActiveMobDtl = ldaActive
         ldaEndMobDtl    = ldaEnd.
      WHEN "ErrorDtl" THEN ASSIGN
         ldaActiveErrorDtl = ldaActive
         ldaEndErrorDtl = ldaEnd.
      WHEN "FraudDtl" THEN ASSIGN
         ldaActiveFraudDtl = ldaActive
         ldaEndFraudDtl = ldaEnd.
      WHEN "EdrDtl" THEN ASSIGN
         ldaActiveEdrDtl = ldaActive
         ldaEndEdrDtl = ldaEnd.
      END CASE.
   END.
   
   /* if details of an old cdr requested then connect to archived dbs,
      search from active though also because there may be old unbilled tickets
      in active db */
   IF (ldaActive NE ? AND idaDate < ldaActive) OR 
      (ldaEnd NE ? AND idaDate > ldaEnd) THEN
      fGetArchiveDBs(ttDB.TableName,
                     idaDate,
                     idaDate).
  
   liDtlFound = 0.
   
   FindDetails:
   FOR EACH ttDB
   BY ttDB.DBOrder:
      
      IF ttDB.Connect THEN DO:
         lcDBName = "dtlQuery".
         CONNECT VALUE (ttDB.ConnName + " " +
                        ttDB.ConnParam + " -ld " + lcDBName) NO-ERROR.
      END.
      ELSE lcDBName = ttDB.ConnName NO-ERROR.
      
      ASSIGN 
         lcDtlTable = lcDbName + "." + ttDB.TableName
         lcDtlQuery = lcDtlTable + ".DateSt = " + lcDtlDate + " AND " +
                      lcDtlTable + ".DtlSeq = " + STRING(iiDtlSeq).
      
      IF NOT ERROR-STATUS:ERROR THEN DO:  

         liDtlFound = fFillCDRTempTable(lcDtlTable, 
                                        lcDtlQuery, 
                                        0,
                                        INPUT-OUTPUT ihDtl).

         IF ttDB.Connect THEN     
            DISCONNECT VALUE(lcDBName).

         IF liDtlFound > 0 THEN LEAVE FindDetails.   
      END.  
      ELSE RETURN FALSE. 
   END.

   EMPTY TEMP-TABLE ttDB.

   /* find also from errorneus cdrs */
   IF liDtlFound = 0 AND icCDRTable NE "ErrorCDR" THEN DO:
      fGetCDRDtl("ErrorCDR",
                 idaDate,
                 iiDtlSeq,
                 INPUT-OUTPUT ihDtl).
   END.
   
   RETURN TRUE. 
   
END FUNCTION.

/**
* fMobcdrCollect: gather tickets from db(s)
*
* @input: icCDRType;char; "post","pre" or both (comma separated) 
          idaFromDate;Date;First calls,
*         idaToDate;Date;Last calls,
*         iiCustNum;Integer;(user)Customer number, 
*         icCustRole;char;"inv" or "user"
*         icCli;Character;CallLineIdentification number,
*         iiInvSeq;Integer;InvoiceSequence Number,
*         iiNotUsed;Integer;Free,
*         icUseCounter;Character;get only counters,
*         icBillCode;Character;Billing Code,
*         icReasonCode;Character;Reason code for 'TUTKA' -operations,
*         iiErrorCodeIn;Integer;ErrorCode,            
* @input-output: oiErrorCodeOut:Integer,
*                tthCDR;Handle;Handle for CDRs,
* @NOTE   dtStartDate and idaToDate;obligatories,
          iiCustNum,icCli,iiInvseq;at least one must be,
          
*/
        
FUNCTION fMobCDRCollect RETURNS INTEGER
  (INPUT icCDRType      AS CHAR,
   INPUT icBrand        AS CHAR,
   INPUT icUser         AS CHAR,
   INPUT idaFromDate    AS DATE,
   INPUT idaToDate      AS DATE,
   INPUT iiCustNum      AS INT,
   INPUT icCustRole     AS CHAR,
   INPUT icCli          AS CHAR,
   INPUT iiInvseq       AS INT,
   INPUT iiNotUsed      AS INT,
   INPUT icUseCounter   AS CHAR,
   INPUT icBillCode     AS CHAR,
   INPUT icReasonCode   AS CHAR,
   INPUT iiErrorCodeIn  AS INT,
   INPUT-OUTPUT oiErrorCodeOut AS INT,
   INPUT-OUTPUT tthCDR         AS HANDLE):

   DEF VAR lcQuery       AS CHAR NO-UNDO.
   DEF VAR tthBuf        AS HANDLE NO-UNDO.
   DEF VAR lhQuery       AS HANDLE NO-UNDO.
   DEF VAR lhField       AS HANDLE NO-UNDO.
   DEF VAR lcDbName      AS CHAR NO-UNDO.      
   DEF VAR lcTableName   AS CHAR NO-UNDO.
   DEF VAR lcErrorQuery  AS CHAR NO-UNDO.
   DEF VAR liMaxQty      AS INT  NO-UNDO.
   DEF VAR liCount       AS INT  NO-UNDO.

   lcQuery = fCallQuery(INPUT  idaFromDate,
                        INPUT  idaToDate,
                        INPUT  iiCustNum,
                        INPUT  icCustRole,
                        INPUT  icCli,
                        INPUT  iiInvseq,
                        INPUT  icBillCode,
                        INPUT  icReasonCode,
                        INPUT  iiErrorCodeIn,
                        OUTPUT oiErrorCodeOut,
                        OUTPUT lcErrorQuery).

   IF lcQuery = "" THEN RETURN 0. 
   
   EMPTY TEMP-TABLE ttDB.
   
   /* get postpaid by default */
   IF icCDRType = "" THEN icCDRType = "post".
   
   /* select which types are collected */
   IF LOOKUP("post",icCDRType) > 0 THEN DO:
      fSetCollectionDBs("mcdr",
                        "MobCDR",
                        idaFromDate,
                        idaToDate,
                        INPUT-OUTPUT ldaActiveMobCDR,
                        INPUT-OUTPUT ldaEndMobCDR).
   END.
                        
   IF LOOKUP("pre",icCDRType) > 0 THEN DO:
      fSetCollectionDBs("prepcdr",
                        "PrepCDR",
                        idaFromDate,
                        idaToDate,
                        INPUT-OUTPUT ldaActivePPCDR,
                        INPUT-OUTPUT ldaEndPPCDR).
   END.
   
   IF LOOKUP("edr",icCDRType) > 0 THEN DO:
      fSetCollectionDBs("prepedr",
                        "PrepEDR",
                        idaFromDate,
                        idaToDate,
                        INPUT-OUTPUT ldaActivePPCDR,
                        INPUT-OUTPUT ldaEndPPCDR).
   END.
   
   IF LOOKUP("fraud",icCDRType) > 0 THEN DO:
      fSetCollectionDBs("fraudcdr",
                        "fraudcdr",
                        idaFromDate,
                        idaToDate,
                        INPUT-OUTPUT ldaActivePPCDR,
                        INPUT-OUTPUT ldaEndPPCDR).
   END.
   
   /* unbillable error calls */
   IF lcErrorQuery > "" THEN DO:
      fSetCollectionDBs("roamcdr",
                        "ErrorCDR",
                        idaFromDate,
                        idaToDate,
                        INPUT-OUTPUT ldaActiveErrorCDR,
                        INPUT-OUTPUT ldaEndErrorCDR).
   END.

   ASSIGN 
      liCount = INDEX(icCDRType,"Qty:")
      liMaxQty = 99999999.
   IF liCount > 0 THEN 
      liMaxQty = INTEGER(ENTRY(1,SUBSTRING(icCDRType,liCount + 4))) NO-ERROR.
   
   FOR EACH ttDB:
        
      IF ttDB.Connect THEN DO:
         lcDBName = "mcdrQuery".
         CONNECT VALUE (ttDB.ConnName + " " + 
                        ttDB.ConnParam + " -ld " + lcDBName) NO-ERROR.
      END.
      ELSE lcDBName = ttDB.ConnName NO-ERROR.
      
      IF NOT ERROR-STATUS:ERROR THEN DO:  

         lcTableName = lcDbName + "." + ttDB.TableName.
         liCount = fFillCDRTempTable(lcTableName, 
                                     REPLACE(lcQuery,"#TBL",lcTableName),
                                     liMaxQty,
                                     INPUT-OUTPUT tthCDR).
         
         IF ttDB.Connect THEN     
            DISCONNECT VALUE(lcDBName).
    
         liMaxQty = liMaxQty - liCount.
         IF liMaxQty <= 0 THEN LEAVE.       
      END. 
       
      ELSE oiErrorCodeOut = 21.
   END.

   /* log reason for browsing tickets */
   IF icReasonCode > "" THEN DO:
   
      CREATE BUFFER tthBuf FOR TABLE tthCDR:DEFAULT-BUFFER-HANDLE.

      CREATE QUERY lhQuery.
      lhQuery:SET-BUFFERS(tthBuf).
      lhQuery:QUERY-PREPARE("FOR EACH " + tthCDR:NAME ).
      
      lhQuery:QUERY-OPEN.

      DO WHILE TRUE:

         lhQuery:GET-NEXT.
   
         IF lhQuery:QUERY-OFF-END THEN LEAVE.

         lhField = tthBuf:BUFFER-FIELD("cli")   .

         FIND FIRST ttCli WHERE 
                    ttCli.ttCli = lhField:BUFFER-VALUE No-ERROR.
         IF NOT AVAIL ttCli THEN DO:
            CREATE ttCli.
            ttCli.ttCli = lhField:BUFFER-VALUE.     
         END.           
      
      END.

      DELETE OBJECT tthBuf NO-ERROR.
      DELETE OBJECT lhQuery NO-ERROR.
      DELETE OBJECT lhField NO-ERROR.
   
      FOR EACH ttCli .
         CREATE CallScanner .
         ASSIGN
            CallScanner.TMSTime     = fmakeTS()
            CallScanner.UserCode    = icUser
            CallScanner.SystemID    = "XFERA_CUI" 
            CallScanner.EventType   = lcQueryCase
            CallScanner.ReasonCode  = icReasonCode
            CallScanner.Level       = ""
            CallScanner.AccessType  = "r"
            CallScanner.Target      = ttcli.ttcli
            CallScanner.StartTime   = STRING(idaFromDate,"99/99/99") + " " +
                                   STRING("00:00:00")
                                                  
            CallScanner.EndTime     = STRING(idaToDate,"99/99/99") + " " +
                                      STRING("23:59:59")
                                                  
            CallScanner.SearchRule  = lcQueryCase          + ";" +
                                   STRING(idaFromDate ) + ";" +
                                   STRING(idaToDate)   + ";" +
                                   STRING(iiCustNum)    + ";" +
                                   icCli                + ";" +
                                   STRING(iiInvseq)     + ";" +  
                                   STRING(icBillCode) NO-ERROR.
      END.
      
   END.
   
   EMPTY TEMP-TABLE ttDB.
   
END FUNCTION.

