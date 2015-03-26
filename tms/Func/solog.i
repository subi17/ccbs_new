/* SOLog.i... FRAME lis & local-UPDATE-record 

Modified ....: 31.08.01 pt NEW FIELD: so-tslot

--------------------------------------------------- */

DEF VAR stname       AS C   format "x(25)"     NO-UNDO.
DEF VAR Comm         AS c   extent 5                       NO-UNDO.
DEF VAR CrTime       AS C   FORMAT "x(8)"                  NO-UNDO.
DEF VAR BaTime       AS C   FORMAT "x(8)"                  NO-UNDO.
DEF VAR AcTime       AS C   FORMAT "x(8)"                  NO-UNDO.
DEF VAR CoTime       AS C   FORMAT "x(8)"                  NO-UNDO.
DEF VAR mnpsend      AS C   format "x(10)"                 NO-UNDO.
DEF VAR lcresp       AS CHAr                   NO-UNDO EXTENT 3.
DEF VAR lcName       AS CHAR                   NO-UNDO.
DEF VAR lcCustName   AS CHAR                   NO-UNDO.
DEF VAR llRefresh    AS LOG NO-UNDO. 

FUNCTION fTime RETURNS CHARACTER(tstamp AS DECIMAL).

  DEF VAR xtime   AS INTEGER.

  xtime = INT(SUBSTR(STRING(tstamp,"99999999.99999"),10,5)).
  RETURN STRING(xtime,"hh:mm:ss").
END FUNCTION.

form
    "OrdSeq ........:" SOLog.SOLog format ">>>>>>>>>" mnpsend AT 50
     FORMAT "x(10)"    SKIP
    "Customer.......:" MSISDN.CustNum FORMAT "zzzzzzz9" 
                   lcCustName format "x(30)"                   SKIP
    "SubscriptionID.:" SOLog.MsSeq    format "zzzzzzz9"        SKIP
    
    /*lcName at 29 format "x(40)"                      SKIP*/
    
    "MSISDN ........:" SOLog.CLI   FORMAT "x(15)"                 SKIP
    "Status ........:" SOLog.Stat stname at 29 format "x(50)"     SKIP
    "Created .......:" SOLog.CreatedTS  /* format "99999999"*/    
    "(" SPACE(0) CrTime SPACE(0) ")"   solog.users            SKIP
    "Batch Time ....:" SOLog.TimeSlotTMS format "99999999.99999"    
    "(" SPACE(0) BaTime SPACE(0) ")"                          SKIP
    "Activate ......:" SOLog.ActivationTS /* format "99999999"*/      
    "(" SPACE(0) AcTime SPACE(0) ")"                          SKIP
    "Completed .....:" SOLog.CompletedTS /* format "99999999"*/      
    "(" SPACE(0) CoTime SPACE(0) ")"                          SKIP 
    "Request ID.....:" Solog.MSRequest FORMAT ">>>>>>>>9"     SKIP
    "CommandLine:"                                           
                   Comm[1]  FORMAT "X(65)"                     SKIP
                   Comm[2]  FORMAT "X(78)"                     SKIP
                   Comm[3]  FORMAT "X(78)"                     SKIP 
                   
    "Response...:" lcresp[1] FORMAT "X(65)"                  SKIP
                   lcresp[2] FORMAT "X(78)"                  SKIP
                   lcresp[3] FORMAT "X(78)"                  SKIP 
WITH  OVERLAY ROW 2 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) /*ac-hdr*/ " VIEW SOLog " 
    NO-LABELS 
    /*1 columns*/
    FRAME lis.

form
    Solog.CommLine FORMAT "X(65)" VIEW-AS EDITOR Size 76 BY 16 
    WITH  OVERLAY ROW 2 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) " VIEW SOLOG Commmand " FRAME lfCommline NO-LABELS.

form
    Solog.Response FORMAT "X(65)" VIEW-AS EDITOR Size 76 BY 16
    WITH  OVERLAY ROW 2 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) " VIEW SOLOG Response " FRAME lfResponse NO-LABELS.
                   
    
PROCEDURE local-UPDATE-record:

   llRefresh = TRUE.
   
   REPEAT ON ENDKEY UNDO, LEAVE:

   IF llRefresh THEN DO: 

      RUN local-find-others.
                                                 
      DISP
        SOLog.SOLog
        MSISDN.CustNum WHEN AVAIL MSISDN
        lcCustName WHEN AVAIL Customer
        SOLog.MsSeq
        SOLog.CreatedTS    fTime(SOLog.CreatedTS)   @ crTime
        SOLog.ActivationTS   fTime(SOLog.ActivationTS)  @ acTime
        SOLog.TimeSlotTMS  fTime(SOLog.TimeSlotTMS) @ baTime
        SOLog.CompletedTS   fTime(SOLog.CompletedTS)  @ coTime
        
        "(" + Solog.users + ")" @ SOLOG.users 
        Solog.MSRequest
        SOLog.CLI
        SOLog.Stat
        stname 
        Comm[1]
        Comm[2]
        Comm[3] 
        lcresp[1]
        lcresp[2]
        lcresp[3] 
        "SMS" WHEN solog.msalog = 1 @ mnpsend
        " "   WHEN solog.msalog = 0 @ mnpsend 

      WITH FRAME lis.
      
      ASSIGN ufk = 0 ufk[1] = 2244 ufk[2] = 9823 ufk[8]= 8 ufk[9]= 1
         ehto = 3 llRefresh = FALSE.
      RUN ufkey.p.
   END.

     READKEY.
     nap = keylabel(lastkey).
     
     IF LOOKUP(nap,"1,f1") > 0 THEN DO:
        ufk = 0. ehto = 3. RUN ufkey.
        DISP solog.commline WITH FRAME lfCommline.
        HIDE FRAME lfCommline.
        llRefresh = TRUE.
     END.
     ELSE IF LOOKUP(nap,"2,f2") > 0 THEN DO:
        ufk = 0. ehto = 3. RUN ufkey.
        DISP solog.response WITH FRAME lfResponse.
        HIDE FRAME lfResponse.
        llRefresh = TRUE.
     END.
     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE.
   END.
   HIDE FRAME lis.
END PROCEDURE.

