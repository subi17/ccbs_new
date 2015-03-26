/* ------------------------------------------------------
  MODULE .......: CONREPUI
  FUNCTION .....: UI for contact report
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 03.05.04/aam 
  VERSION ......: M15
  ------------------------------------------------------ */

{commali.i}
{cparam2.i}
{utumaa.i}
{timestamp.i}

DEF INPUT  PARAMETER icUserCode  AS CHAR  NO-UNDO. 
DEF INPUT  PARAMETER idtConDate  AS DATE  NO-UNDO.
DEF INPUT  PARAMETER iiCustNum   AS INT   NO-UNDO. 
DEF INPUT  PARAMETER iiHandled   AS INT   NO-UNDO. 
DEF INPUT  PARAMETER icFile      AS CHAR  NO-UNDO. 
DEF OUTPUT PARAMETER ocCount     AS INT   NO-UNDO. 

DEF VAR viiva1 AS CHAR      FORMAT "x(112)" NO-UNDO.
DEF VAR viiva2 LIKE viiva1.
DEF VAR viiva3 LIKE viiva1.
DEF VAR viiva4 LIKE viiva1.
DEF VAR sl     AS INT                       NO-UNDO.
DEF VAR rl     AS INT                       NO-UNDO.
DEF VAR lev    AS INT                       NO-UNDO INIT 78.
DEF VAR otsi   AS CHAR EXTENT 39            NO-UNDO.

DEF VAR lcDateHeader AS CHAR NO-UNDO.
DEF VAR ldtConDate   AS DATE NO-UNDO.
DEF VAR liConTime    AS INT  NO-UNDO. 
DEF VAR lcConDate    AS CHAR NO-UNDO.  
DEF VAR lcSesNum     AS CHAR NO-UNDO. 

ASSIGN 
    viiva1   = FILL("=",lev)
    viiva2   = FILL("=",lev)
    viiva3   = FILL("-",lev)
    viiva4   = FILL("-",lev).

form header
   viiva1 AT 1 SKIP
   ynimi at 1 FORMAT "x(30)" 
      "CONTACT REPORT" at 35
      "Page" at 68  
      sl FORMAT "ZZZZ9" SKIP
   lcDateHeader AT 35 FORMAT "X(30)"
      pvm FORMAT "99.99.9999" at 69 SKIP
   viiva2 at 1 SKIP
   "Customer"     TO 8
   "Name"         AT 10
   "Tot.Balance"  TO 52
   "Planned"      AT 54
   "Taken"        AT 63
   SKIP
   viiva3
   WITH WIDTH 112 NO-LABELS NO-BOX FRAME sivuotsi.


FUNCTION fCheckPage RETURNS LOGIC 
    (iAddLine AS INT).

    IF icFile > "" THEN RETURN FALSE.
    
    IF rl + iAddLine >= skayt1 THEN DO:

        IF sl > 0 THEN DO:
           {uprfeed.i rl}
        END.
        
        sl = sl + 1.

        VIEW STREAM tul FRAME sivuotsi.  
        rl = 6.

        RETURN TRUE.
    END.

    ELSE RETURN FALSE.
END.

IF icFile > "" THEN DO:

   ASSIGN lcSesNum               = SESSION:NUMERIC-FORMAT
          SESSION:NUMERIC-FORMAT = "european".
   
   OUTPUT STREAM tul TO VALUE(icFile).

   PUT STREAM tul UNFORMATTED
      "User"          CHR(9)
      "Customer"      CHR(9)
      "Name"          CHR(9)
      "Tot. Balance"  CHR(9)
      "Planned"       CHR(9)
      "Taken"         SKIP.
END.
      
FOR EACH Contact NO-LOCK WHERE
         Contact.Brand    = gcBrand    AND
         Contact.UserCode = icUserCode AND
         Contact.ConDate  = idtConDate AND
         (IF iiHandled < 2
          THEN Contact.ConState = iiHandled
          ELSE TRUE)
BREAK BY Contact.UserCode       
      BY Contact.ConDate
      BY Contact.CustNum:

   IF FIRST-OF(Contact.UserCode) AND icFile = "" THEN DO:

      FIND TMSUser NO-LOCK WHERE 
           TMSUser.UserCode = Contact.UserCode NO-ERROR.
      lcDateHeader = Contact.UserCode + " " +
                     IF AVAILABLE TMSUser THEN TMSUser.UserName ELSE "".
           
      /* each user to it's own page */
      fCheckPage(999).

   END.      

   FIND Customer OF Contact NO-LOCK NO-ERROR. 
   IF Contact.ConState = 1 THEN DO:
      fSplitTS(Contact.ConStamp,
               OUTPUT ldtConDate,
               OUTPUT liConTime).
      lcConDate = STRING(ldtConDate,"99-99-99") + " " + 
                  STRING(liConTime,"hh:mm").
   END.
   ELSE lcConDate = "".
               

   ocCount = ocCount + 1.
   
   IF icFile > "" THEN DO:
      PUT STREAM tul UNFORMATTED
         Contact.UserCode   CHR(9)
         Contact.CustNum    CHR(9)
         (IF AVAILABLE Customer
          THEN Customer.CustName
          ELSE "")          CHR(9)
         Contact.CustBal    CHR(9)
         Contact.PlanDate   CHR(9)
         lcConDate          SKIP.
   END.
   
   ELSE DO:
      fCheckPage(0).
    
      PUT STREAM tul 
      Contact.CustNum        AT 1   FORMAT ">>>>>>>9"
      (IF AVAILABLE Customer
       THEN Customer.CustName
       ELSE "")              AT 10  FORMAT "X(30)"
      Contact.CustBal        TO 52  FORMAT "->>>>>>>9.99"  
      Contact.PlanDate       AT 54  FORMAT "99-99-99"
      lcConDate              AT 63  FORMAT "X(15)"
      SKIP.

      rl = rl + 1.
   END.
END. 

IF icFile > "" THEN DO:
   OUTPUT STREAM tul CLOSE.
   SESSION:NUMERIC-FORMAT = lcSesNum.
END.

ELSE DO:
   {uprfeed.i rl}
END.   


