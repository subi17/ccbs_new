 /* ---------------------------------------------------------------------------
  MODULE .......: subsrel.p
  FUNCTION .....: ui for subscription revenue report 
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 21.08.03/aam 
  MODIFIED .....: 24.09.03/aam brand
  VERSION ......: M15
  --------------------------------------------------------------------------- */

{commali.i}

{utumaa.i "new"}

assign tuni1 = "subsrel"
       tuni2 = "".

def var ufkey       as log                     no-undo.
def var ok          as log   format "Yes/No"   no-undo.
DEF VAR ldtDate1    AS DATE                    NO-UNDO.
DEF VAR ldtDate2    AS DATE                    NO-UNDO. 
DEF VAR lcInvGroup  AS CHAR  FORMAT "X(10)"    NO-UNDO EXTENT 2.
DEF VAR lcCLIType   AS CHAR  FORMAT "X(10)"    NO-UNDO EXTENT 2. 
DEF VAR lcBillCode  AS CHAR  FORMAT "X(16)"    NO-UNDO EXTENT 2.
DEF VAR liCCN       AS INT   FORMAT ">>9"      NO-UNDO EXTENT 2. 
DEF VAR lcFile      AS CHAR  FORMAT "X(50)"    NO-UNDO.

form
   skip(2)
   "     This program prints out a subscription revenue report"   SKIP
   "     from invoiced events. Report is printed by subscription" SKIP
   "     type, product, CCN and B-number."                        SKIP
   SKIP(2)
   ldtDate1  AT 10
        LABEL "Time period"
        HELP "Time period for invoiced events"
        FORMAT "99-99-9999"
   "-" AT 40
   ldtDate2    
        NO-LABEL
        HELP "Time period for invoiced events"
        VALIDATE (INPUT ldtDate2 ge INPUT ldtDate1,
                  "Upper limit must be at least equal to lower limit")
        FORMAT "99-99-9999"
   SKIP  

   lcInvGroup[1] AT 10
        LABEL "Inv. groups"
        HELP "Invoicing group"
   "-" AT 40
   lcInvGroup[2] 
        NO-LABEL
        HELP "Invoicing group"
        VALIDATE (INPUT lcInvGroup[2] ge INPUT lcInvGroup[1],
                  "Upper limit must be at least equal to lower limit")
   SKIP

   lcCLIType[1]  AT 10
        LABEL "Subs. types"
        HELP  "Mobile subscription type"
   "-" AT 40
   lcCLIType[2] 
        NO-LABEL 
        HELP  "Mobile subscription type"
        VALIDATE (INPUT lcCLIType[2] GE INPUT lcCLIType[1],
                  "Upper limit must be at least equal to lower limit")
   SKIP

   lcBillCode[1]  AT 10
        LABEL "Products .."
        HELP  "Products"
   "-" AT 40
   lcBillCode[2] 
        NO-LABEL 
        HELP  "Products"
        VALIDATE (INPUT lcBillCode[2] GE INPUT lcBillCode[1],
                  "Upper limit must be at least equal to lower limit")
   SKIP

   liCCN[1] FORMAT ">>>9" AT 10
        LABEL "CCN ......."
        HELP  "CCN codes"
   "-" AT 40
   liCCN[2] FORMAT ">>>9"
        NO-LABEL 
        HELP  "CCN codes"
        VALIDATE (INPUT liCCN[2] GE INPUT liCCN[1],
                  "Upper limit must be at least equal to lower limit")
   SKIP(1)
   lcFile AT 10
        LABEL "Excel-file "
        HELP "If name is given, then a tab-separated file is made"
   skip(3)

   with row 1 side-labels width 80
        title " " + ynimi + " SUBSCRIPTION REVENUE REPORT " +
        string(pvm,"99-99-99") + " "
        frame valinta.


/* previous month as default */
ASSIGN ldtDate2 = DATE(MONTH(TODAY),1,YEAR(TODAY)) - 1
       ldtDate1 = DATE(MONTH(ldtDate2),1,YEAR(ldtDate2)).

FIND LAST InvGroup NO-LOCK WHERE
          InvGroup.Brand = gcBrand NO-ERROR.
IF AVAILABLE InvGroup THEN ASSIGN lcInvGroup[2] = InvGroup.InvGroup.

FIND LAST CLIType NO-LOCK WHERE
          CLIType.Brand = gcBrand NO-ERROR.
IF AVAILABLE CLIType THEN ASSIGN lcCLIType[2] = CLIType.CLIType.

FIND LAST BillItem NO-LOCK WHERE
          BillItem.Brand = gcBrand NO-ERROR.
IF AVAILABLE BillItem THEN lcBillCode[2] = BillItem.BillCode.

FIND LAST CCN NO-LOCK WHERE
          CCN.Brand = gcBrand NO-ERROR.
IF AVAILABLE CCN THEN liCCN[2] = CCN.CCN.

display ldtDate1 ldtDate2
        lcInvGroup 
        lcCLIType
        lcBillCode
        liCCN
        with frame valinta. 
assign ufkey = false
       nap   = "first". 


toimi:
repeat with frame valinta on endkey undo toimi, next toimi:

      if ufkey then do:
         assign
         ufk[1]= 132 
         ufk[2]= 0  ufk[3]= 0 ufk[4]= 0
         ufk[5]= 63 ufk[6]= 0 ufk[7]= 0 ufk[8]= 8 
         ufk[9]= 1
         ehto = 3 
         ufkey = false.
         run ufkey.p.
      end.

      if nap ne "first" then do:
          readkey.
          ASSIGN
          nap = keylabel(lastkey).
      end.
      else assign nap = "1". 

      if lookup(nap,"1,f1") > 0 then do:
         ehto = 9. run ufkey.p.
         repeat with frame valinta on endkey undo, leave:
            update 
                ldtDate1
                ldtDate2
                lcInvGroup
                lcCLIType
                lcBillCode
                liCCN
                lcFile.
            leave. 
         end.

         display ldtDate1 ldtDate2
                 lcInvGroup 
                 lcCLIType
                 lcBillCode
                 liCCN
                 lcFile
         with frame valinta. 

         ufkey = true.
         next toimi.
      end.

      else if lookup(nap,"5,f5") > 0 then do:
         leave toimi.
      end.

      else if lookup(nap,"8,f8") > 0 then do:
         return.
      end.
end. /* toimi */

ehto = 5.
run ufkey.

IF lcFile = "" THEN DO:
   assign tila = true.
   {utuloste.i "return"}
END.

message "Printing in process".            

run subsrep (ldtDate1, 
             ldtDate2,
             lcInvGroup[1],
             lcInvGroup[2],
             lcCLIType[1],
             lcCLIType[2],
             lcBillCode[1],
             lcBillCode[2],
             liCCN[1],
             liCCN[2],
             lcFile).

IF lcFile = "" THEN DO:
   assign tila = false.
   {utuloste.i}
END.

message "Subscription revenue report is finished."
view-as alert-box
TITLE " Report done ".  

hide message no-pause.
hide frame valinta no-pause.    
