/* ----------------------------------------------------------------------
  MODULE .......: custnobui
  TASK .........: ui for listing customers whose invoice creation failed
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 10.05.05
  CHANGED ......: 16.12.05/aam a period can be given instead of one day
                  15.09.06/aam optionally only customers with live subscr.  
  Version ......: M15
  ---------------------------------------------------------------------- */

{commali.i}

DEF VAR ldtDate  AS DATE NO-UNDO EXTENT 2.
DEF VAR lcFile   AS CHAR NO-UNDO.
DEF VAR liCnt    AS INT  NO-UNDO. 
DEF VAR lcUser   AS CHAR NO-UNDO. 
DEF VAR llMail   AS LOG  NO-UNDO. 
DEF VAR llActive AS LOG  NO-UNDO.

FORM
   SKIP(3)
   "Read lines from event log and list customers to whom" AT 10 SKIP
   "an invoice was not created."  AT 10 SKIP(2)
   
   ldtDate[1] AT 10 
      LABEL "Period .." 
      FORMAT "99-99-9999"
      HELP "Date when invoices were created"
   "-"
   ldtDate[2] 
      NO-LABEL
      FORMAT "99-99-9999"
      HELP "Date when invoices were created"
      VALIDATE(INPUT ldtDate[2] >= INPUT ldtDate[1],
               "End date cannot be earlier than begin date")
      SKIP

   lcUser AT 10 
      LABEL "User ...."
      FORMAT "X(12)"
      HELP "User, whose events are handled. EMPTY = all"
      SKIP
      
   llActive AT 10
      LABEL "Customers"
      FORMAT "Current/All"
      HELP "Only customers with (C)urrent subscriptions or (A)ll"
      SKIP(1)
      
   lcFile AT 10 
      LABEL "File ...."
      FORMAT "X(50)"
      HELP "Name for the tab separated file"
   llMail AT 10
      LABEL "Mail ...."
      HELP "Mail the file to configured recipients"
   SKIP(4)
   WITH ROW 1 WIDTH 80 SIDE-LABELS TITLE "  Unbilled Customers "
       FRAME fDate.
       
       
ASSIGN ldtDate[1] = DATE(MONTH(TODAY),1,YEAR(TODAY))
       ldtDate[2] = TODAY
       lcUser     = katun
       toimi      = -1
       llMail     = FALSE
       llActive   = FALSE. 

REPEAT WITH FRAME fDate: 

    PAUSE 0.
    DISP ldtDate lcUser lcFile llMail llActive WITH FRAME fDate.

    IF toimi = -1 THEN toimi = 1.
    ELSE DO:
       ASSIGN ufk = 0
              ufk[1] = 7
              ufk[5] = 795
              ufk[8] = 8
              ehto   = 0.
              
       IF ldtDate[1] = ? OR ldtDate[2] = ? OR lcFile = "" 
       THEN ufk[5] = 0.
              
       RUN ufkey.
    END.
    
    IF TOIMI = 1 THEN DO:
    
       ehto = 9.
       RUN ufkey.
       
       REPEAT WITH FRAME fDate ON ENDKEY UNDO, LEAVE:
       
          UPDATE ldtDate 
                 lcUser
                 llActive
                 lcFile
                 llMail.
          LEAVE.
       END.
    
    END.

    ELSE IF toimi = 5 THEN DO:
       RUN custnobill (ldtDate[1],  
                       ldtDate[2],
                       lcUser,
                       llActive,
                       lcFile,
                       llMail,
                       OUTPUT liCnt).

       MESSAGE liCnt "customers listed to file" SKIP
               lcFile
       VIEW-AS ALERT-BOX 
       TITLE " DONE ".
    END.
    
    ELSE IF toimi = 8 THEN LEAVE.
   
END.

HIDE FRAME fDate NO-PAUSE.

 

   

