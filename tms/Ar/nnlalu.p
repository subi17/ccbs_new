/* ----------------------------------------------------------------------------
  MODULE .......: NNLALU.P
  FUNCTION .....: lASKULUETTELON print-line
  SOVELLUTUS ...: nn
  CREATED ......: 26.02.1997 pt
  changePVM ....: 19.09.1997 pt IntAccNum
                  06.11.1997 pt parannettu poimintarajoja for each-silmukassa
                  23.11.1997 pt InvGroup -rajaus
                  05.08.1998 pt ConnType
                  22.10.1998 kl display formats
                  04.12.1998 kl all / printing denied
                  08.12.1998 pt in ENGLISH
                  24.08.1999 pt 'unpaid' option
                  02.09.1999 pt "VAT" option
                  09.11.1999 kl 'reminders' option
                  15.11.1999 kl tuning
                  14.02.2000 pt handling of Invoice.AdvPaym
                  26.09.2000 kl lcExtInvID2 init 999999999
                  30.05.2002/aam only summary optionally 
                  14.10.2002/aam InvType and PaymState as selection criteria
                  05.03.2003/tk  tokens               
                  20.03.2003/jp  External customer group
                  11.09.2003/aam brand
                  28.11.2003/aam customer number selection
                  31.01.07/aam Invoice.ExtInvID
                  25.04.07/aam default upper limit for InvType: 98
                  26.06.07/aam logic to invjournal.p,
                               ConnType removed
  Version ......: M15
  --------------------------------------------------------------------------- */

{Syst/commali.i}
{Syst/utumaa.i "new"}
{Func/tmsparam2.i}
{Func/fcurrency.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'invoice'}
{Ar/invjournal.i}

assign tuni1 = "nnlalu"
       tuni2 = "".

def var pvm1       as Date format "99-99-99" NO-UNDO.
def var pvm2       as Date format "99-99-99" NO-UNDO.
def var InvGroup    like InvGroup.InvGroup no-undo.
def var IGName    like InvGroup.IGName no-undo.
def var latil      as log format "Yes/No" NO-UNDO.
def var tikoo      as log format "Yes/No" NO-UNDO.
def var unpaid     as log format "Unpaid/All" NO-UNDO.
def var i          as int no-undo.

def var lcExtInvID1 like Invoice.ExtInvID NO-UNDO.
def var lcExtInvID2 like Invoice.ExtInvID NO-UNDO.
def var edInvNum    like Invoice.ExtInvID NO-UNDO.

def var deny       as lo no-undo format "Denied/All".
def var VATAmt     as lo no-undo format "All/No VAT" init true.
def var rem        as lo no-undo format "All/Denied" init true.

DEF VAR liInvType1   AS INT   NO-UNDO FORMAT ">9".
DEF VAR liInvType2   LIKE liInvType1.
DEF VAR liPaymState1 AS INT   NO-UNDO FORMAT "9".
DEF VAR liPaymState2 LIKE liPaymState1. 
DEF VAR lcCode       AS CHAR  NO-UNDO. 
DEF VAR lcFrameField AS CHAR  NO-UNDO. 
DEF VAR liCustNum    AS INT   NO-UNDO EXTENT 2.
DEF VAR llInvoices   AS LOG   NO-UNDO.

DEF VAR ExtCustGrp   AS LOG   NO-UNDO init FALSE FORMAT "Y/N".
DEF VAR extname      AS CHAR  NO-UNDO.
DEF VAR dExtCustGrp  AS CHAR  NO-UNDO FORMAT "x(25)".
DEF VAR kpl          AS INT   NO-UNDO.


form
   InvGroup   at 2 label "Customers in inv. group." IGName no-label skip
   extcustgrp at 2 label "External customer groups" 
   HELP "Select external customer groups "
   extname format "x(30)" NO-LABEL                                  SKIP

   liCustNum[1] at 2
      label "Customer numbers ......."
      format ">>>>>>>9"
      help "Customers"
   "-"
   liCustNum[2]   
      no-label
      format ">>>>>>>9"
      help "Customers"
      SKIP
      
   pvm1   at 2    label "Invoices made between .."
                  help  "Earliest day of an invoice"
   "-"
   pvm2           help  "Latest day of an invoice"
                  no-label skip

   lcExtInvID1 at 2    label "Invoice Numbers ........"
                  help  "Smallest Invoice No." 
   "-"
   lcExtInvID2         help  "Greatest Invoice No."  
                  no-label skip

   liInvType1 at 2 label "Invoice Types .........."
                   help "Invoice types from"
   "-"
   liInvType2     no-label 
                  help "Invoice types to" skip

   liPaymState1 at 2 label "Payment Status ........."
                   help "Payment status from"
   "-"
   liPaymState2     no-label 
                  help "Payment status to" skip

   rem  at 2      label "Reminders Denied ......."
      help "All or those where reminders are denied (A/D)"   skip
   deny at 2      label "Printing  Denied ......."
      help "All or those where printing is denied (A/D)"     skip
   unpaid at 2    label "(A)ll or (U)npaid only ."
      help "Print (A)ll invoices or only (U)npaid invoices"  skip
   VATAmt at 2       label "Invoices Including VAT ."
      help "Print out (A)ll invoices or ONLY invoices with (N)o VAT"
   skip(1)
   
   "Options for printout" at 2  skip
   llInvoices AT 5 LABEL "Print Invoices ........." 
      HELP "Print each invoice" 
      FORMAT "Yes/No" SKIP
   latil      at 5 label "Print Postings ........." 
      HELP "Print posting data with each invoice" skip
   tikoo      at 5 label "Separate Posting Summary" 
      HELP "Print an account level summary from all postings" skip(1)
   
   with row 1 side-labels width 80
        title color value(ctc) " " + ynimi + "  INVOICE JOURNAL  " +
        string(pvm,"99-99-99") + " " color value(cfc)
        frame rajat.


cfc = "puli". RUN Syst/ufcolor.
pause 0 no-message.

assign pvm2         = date(month(today),01,year(today)) - 1
       pvm1         = date(month(pvm2),01,year(pvm2))
       llInvoices   = TRUE
       latil        = true 
       tikoo        = true 
       liCustNum[2] = 99999999
       liInvType2   = 98
       liPaymState2 = 9
       lcExtInvID2  = FILL("Z",12)
       extName      = "NOT SELECTED".

toimi:
repeat with frame rajat on endkey undo toimi, next toimi:

      display pvm1 pvm2 latil tikoo "ALL" @ IGName rem deny VATAmt 
        unpaid liInvType1 liInvType2 liPaymState1 liPaymState2 
        llInvoices  
        liCustNum  extname
        lcExtInvID1 lcExtInvID2
      with frame rajat.

      IF InvGroup > "" THEN DO:
         FIND InvGroup WHERE 
              InvGroup.Brand    = gcBrand AND
              InvGroup.InvGroup = InvGroup NO-LOCK NO-ERROR.
         IF AVAILABLE InvGroup THEN DISPLAY InvGroup.igname @ igname
            WITH FRAME rajat.
      END.
         
      IF extname = ""
      THEN DISPLAY "NOT SELECTED" @ extname WITH FRAME rajat.

      assign ufk = 0 ufk[1] = 132 ufk[5] = 63 ufk[8] = 8 ehto = 0.
      RUN Syst/ufkey.

      if toimi = 1 then do:
         ehto = 9. RUN Syst/ufkey.

         repeat with frame rajat on endkey undo, leave:
         
            update
                InvGroup
                extcustgrp
                liCustNum[1]
                liCustNum[2]
                   validate(input liCustNum[2] >= input liCustNum[1],
                           "Invalid order")
                pvm1 validate(input pvm1 ne ?, "Date is mandatory")
                pvm2 validate(input pvm2 ne ? and 
                              input pvm2 >= input pvm1,"Invalid order !")
                lcExtInvID1
                lcExtInvID2
                   validate(input lcExtInvID2 >= input lcExtInvID1,
                            "Invalid order !")
                liInvType1
                liInvType2
                   validate(input liInvType2 >= input liInvType1,
                            "Invalid order !")
                liPaymState1
                liPaymState2
                   validate(input liPaymState2 >= input liPaymState1,
                            "Invalid order !")                
                rem 
                deny  
                unpaid 
                VATAmt
                llInvoices
                latil
                tikoo

            with frame rajat editing:

               readkey. nap = keylabel(lastkey).

               IF nap = "F9" AND 
                  (INDEX(FRAME-FIELD,"liInvType") > 0 OR
                   INDEX(FRAME-FIELD,"liPaymState") > 0)
               THEN DO:

                  lcFrameField = FRAME-FIELD.

                  IF LOOKUP(FRAME-FIELD,"liInvType1,liInvType2") > 0
                  THEN DO:

                     RUN Help/h-tmscodes.p(INPUT "Invoice",  /* TableName*/
                                          "InvType", /* FieldName */
                                          "Report", /* GroupCode */
                                    OUTPUT lcCode).

                     IF lcCode ne "" AND lcCode NE ?
                     THEN DO WITH FRAME rajat:
                        IF lcFrameField  = "liInvType1" 
                        THEN DISPLAY INTEGER(lcCode) ;& liInvType1.
                        ELSE DISPLAY INTEGER(lcCode) ;& liInvType2.
                     END.
                  END.

                  ELSE IF LOOKUP(FRAME-FIELD,"liPaymState1,liPaymState2") > 0
                  THEN DO:

                     RUN Help/h-tmscodes.p(INPUT "Invoice",  /* TableName*/
                                          "PaymState", /* FieldName */
                                          "AccRec", /* GroupCode */
                                    OUTPUT lcCode).

                     IF lcCode ne "" AND lcCode NE ?
                     THEN DO WITH FRAME rajat:
                        IF lcFrameField  = "liPaymState1" 
                        THEN DISPLAY INTEGER(lcCode) ;& liPaymState1.
                        ELSE DISPLAY INTEGER(lcCode) ;& liPaymState2.
                     END.
                  END.

                  ehto = 9.
                  RUN Syst/ufkey.
                  NEXT. 
               END.

               if lookup(nap,poisnap) > 0 then do:
                  hide message.
                  if frame-field = "InvGroup" then do:
                     assign frame rajat InvGroup.
                     if InvGroup = "" then do:
                        disp "ALL" @ IGName with frame rajat.
                        IGName = "ALL".
                     end.
                     else do:
                        find InvGroup where
                             InvGroup.Brand    = gcBrand AND
                             InvGroup.InvGroup = InvGroup
                        no-lock no-error.
                        if not avail InvGroup then do:
                           bell.  message "Invalid Invoicing Group Code !".
                           next.
                        end.
                        disp InvGroup.IGName @ IGName with frame rajat.
                        IGName = InvGroup.IGName.
                     end.
                  end.

                  ELSE if frame-field = "extcustgrp" then do:

                     FOR EACH TCustGroup.
                        DELETE TCustGroup.
                     END.

                     assign frame rajat extcustgrp.

                     if extcustgrp NE YES   then do:
                        disp "NOT SELECTED" @ extname with frame rajat.
                     end.
                     else do:
                        RUN Mc/gathecg(INPUT-OUTPUT table TCustGroup).
                        /* DISPLAY Customer groups */
                        EHTO = 9.
                        RUN Syst/ufkey.
                        FOR EACH TCustGroup.
                           dExtCustGrp = dExtCustGrp + TCustGroup.CustGroup +
                           ",".
                        END.
                        /* Remove last comma */

                        IF dextcustgrp ne "" THEN
                           dextcustgrp = SUBSTRING(DextCustGrp,1,                                                         LENGTH(dextCustGrp) - 1).
                        DISP ExtCustGrp dExtCustGrp @ extname with frame rajat.

                        IF  dExtCustGrp = "" THEN
                        DISP "NOT SELECTED" @ extname with frame rajat.

                        apply 13 /* ENTER*/ .
                     end.
                  END.
               end.
               apply lastkey.
            end.

            leave.
         end.

         IF NOT llInvoices THEN DO:
            latil = FALSE.
            DISPLAY latil WITH FRAME rajat.
         END.
 
         next toimi.
      end.


      if toimi = 5 then do:

         leave toimi.
      end.

      if toimi = 8 then return.

end. /* toimi */


IF extcustgrp = FALSE THEN 
   EMPTY TEMP-TABLE TCustGroup.

CREATE ttCriter.
ASSIGN ttCriter.InvGroup    = InvGroup
       ttCriter.CustNum1    = liCustNum[1]
       ttCriter.CustNum2    = liCustNum[2]
       ttCriter.InvDate1    = pvm1
       ttCriter.InvDate2    = pvm2
       ttCriter.ExtInvID1   = lcExtInvID1
       ttCriter.ExtInvID2   = lcExtInvID2
       ttCriter.InvType1    = liInvType1
       ttCriter.InvType2    = liInvType2
       ttCriter.PaymState1  = liPaymState1
       ttCriter.PaymState2  = liPaymState2
       ttCriter.DenyRemind  = NOT rem
       ttCriter.DenyPrint   = deny
       ttCriter.OnlyUnpaid  = unpaid
       ttCriter.ZeroVat     = NOT VatAmt
       ttCriter.Invoices    = llInvoices
       ttCriter.InvAccounts = latil
       ttCriter.Summary     = tikoo.

assign tila = true.
{Syst/utuloste.i "return"}

RUN Ar/invjournal.p (INPUT TABLE TCustGroup,
                INPUT TABLE ttCriter,
                OUTPUT i).
                  
assign tila = false.
{Syst/utuloste.i}

IF RETURN-VALUE BEGINS "ERROR:" THEN 
   MESSAGE "Printing failed:" RETURN-VALUE
   VIEW-AS ALERT-BOX ERROR.
  
ELSE 
   MESSAGE "Invoice journal has been printed for" i "invoices."
   VIEW-AS ALERT-BOX
   TITLE " Finished ".

hide message no-pause.
hide frame rajat no-pause.

