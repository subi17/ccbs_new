 /* ---------------------------------------------------------------------------
  MODULE .......: contrel.p
  FUNCTION .....: ui for contract report 
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 06.10.03 
  MODIFIED .....: 
  VERSION ......: M15
  --------------------------------------------------------------------------- */

{commali.i}

{utumaa.i "new"}

assign tuni1 = "contrel"
       tuni2 = "".

def var ufkey       as log                     no-undo.
def var ok          as log   format "Yes/No"   no-undo.
DEF VAR ldtToDate   AS DATE                    NO-UNDO EXTENT 2.
DEF VAR ldtClDate   AS DATE                    NO-UNDO EXTENT 2.
DEF VAR lcInvGroup  AS CHAR  FORMAT "X(10)"    NO-UNDO EXTENT 2.
DEF VAR liContrType AS INT   FORMAT "9"        NO-UNDO EXTENT 2. 
DEF VAR lcReseller  AS CHAR  FORMAT "X(10)"    NO-UNDO EXTENT 2.
DEF VAR lcSalesman  AS CHAR  FORMAT "X(10)"    NO-UNDO EXTENT 2.
DEF VAR liCustNum   AS INT   FORMAT ">>>>>>9"  NO-UNDO EXTENT 2. 
DEF VAR liClosed    AS INT   FORMAT "9"        NO-UNDO.               

DEF VAR liField     AS INT                     NO-UNDO. 
DEF VAR lcCode      AS CHAR                    NO-UNDO. 

form
   SKIP(4)

   lcInvGroup[1] AT 10
        LABEL "Inv. groups .."
        HELP "Invoicing group"
   "-" AT 40
   lcInvGroup[2] 
        NO-LABEL
        HELP "Invoicing group"
        VALIDATE (INPUT lcInvGroup[2] ge INPUT lcInvGroup[1],
                  "Upper limit must be at least equal to lower limit")
   SKIP

   liCustNum[1]  AT 10
        LABEL "Customers ...."
        HELP  "Customer number"
   "-" AT 40
   liCustNum[2] 
        NO-LABEL 
        HELP  "Customer number"
        VALIDATE (INPUT liCustNum[2] GE INPUT liCustNum[1],
                  "Upper limit must be at least equal to lower limit")
   SKIP

   lcReseller[1]  AT 10
        LABEL "Reseller ....."
        HELP  "Reseller"
   "-" AT 40
   lcReseller[2] 
        NO-LABEL 
        HELP  "Reseller"
        VALIDATE (INPUT lcReseller[2] GE INPUT lcReseller[1],
                  "Upper limit must be at least equal to lower limit")
   SKIP

   lcSalesman[1]  AT 10
        LABEL "Salesmen ....."
        HELP  "Salesman"
   "-" AT 40
   lcSalesman[2] 
        NO-LABEL 
        HELP  "Salesman"
        VALIDATE (INPUT lcSalesman[2] GE INPUT lcSalesman[1],
                  "Upper limit must be at least equal to lower limit")
   SKIP

   liContrType[1]  AT 10
        LABEL "Contract types"
        HELP  "Contract type"
   "-" AT 40
   liContrType[2] 
        NO-LABEL 
        HELP  "Contract type"
        VALIDATE (INPUT liContrType[2] GE INPUT liContrType[1],
                  "Upper limit must be at least equal to lower limit")
   SKIP


   ldtToDate[1]  AT 10
        LABEL "Agreed dates ."
        HELP "Agreed contract end date"
        FORMAT "99-99-9999"
   "-" AT 40
   ldtToDate[2]    
        NO-LABEL
        HELP "Agreed contract end date"
        VALIDATE (INPUT ldtToDate[2] ge INPUT ldtToDate[1],
                  "Upper limit must be at least equal to lower limit")
        FORMAT "99-99-9999"
   SKIP  

   ldtClDate[1]  AT 10
        LABEL "Closing dates "
        HELP "Actual closing date"
        FORMAT "99-99-9999"
   "-" AT 40
   ldtClDate[2]    
        NO-LABEL
        HELP "Actual closing date"
        VALIDATE (INPUT ldtClDate[2] ge INPUT ldtClDate[1],
                  "Upper limit must be at least equal to lower limit")
        FORMAT "99-99-9999"
   SKIP

   liClosed AT 10
        LABEL "Closing state "
        HELP "0=All, 1=Open, 2=Closed, 3=Prematurely closed"
        VALIDATE(INPUT liClosed <= 3,
                 "Valid values are 0-3")
   skip(5)                              

   with row 1 side-labels width 80
        title " " + ynimi + " CONTRACT REPORT " +
        string(pvm,"99-99-99") + " "
        frame valinta.


ASSIGN ldtToDate[1]   = DATE(MONTH(TODAY),1,YEAR(TODAY))
       ldtToDate[2]   = IF MONTH(ldtToDate[1]) = 12
                        THEN DATE(12,31,YEAR(ldtToDate[1]))
                        ELSE DATE(MONTH(ldtToDate[1]) + 1,1,YEAR(ldtToDate[1])) 
                             - 1
       ldtClDate[1]   = ?
       ldtClDate[2]   = ?
       liContrType[1] = 1
       liContrType[2] = 2.

FIND LAST InvGroup NO-LOCK WHERE
          InvGroup.Brand = gcBrand NO-ERROR.
IF AVAILABLE InvGroup THEN ASSIGN lcInvGroup[2] = InvGroup.InvGroup.

FIND LAST Customer NO-LOCK USE-INDEX CustNum WHERE
          Customer.Brand = gcBrand NO-ERROR.
IF AVAILABLE Customer THEN ASSIGN liCustNum[2] = Customer.CustNum.

FIND LAST Salesman NO-LOCK WHERE
          Salesman.Brand = gcBrand NO-ERROR.
IF AVAILABLE Salesman THEN lcSalesman[2] = Salesman.Salesman.

FIND LAST Reseller NO-LOCK WHERE
          Reseller.Brand = gcBrand NO-ERROR.
IF AVAILABLE Reseller THEN lcReseller[2] = Reseller.Reseller.

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
                lcInvGroup
                liCustNum
                lcReseller
                lcSalesman
                liContrType
                ldtToDate
                ldtClDate
                liClosed
            EDITING:

               READKEY.

               IF KEYLABEL(LASTKEY) = "F9" AND 
                  FRAME-FIELD = "liContrType"
               THEN DO:

                  liField = FRAME-INDEX.

                  RUN h-tmscodes(INPUT "Contract",    /* TableName */
                                       "ContrType",   /* FieldName */
                                       "Commission",  /* GroupCode */
                                 OUTPUT lcCode).

                  IF lcCode ne "" AND lcCode NE ?
                  THEN DO:
                     DISPLAY INTEGER(lcCode) ;& liContrType[liField]
                     WITH FRAME valinta.   
                  END.   

                  ehto = 9.
                  RUN ufkey.
                  NEXT. 
               END.

               APPLY LASTKEY.
            END.

            leave. 
         end.

         display ldtToDate
                 ldtClDate
                 lcInvGroup 
                 liCustNum
                 liContrType
                 lcReseller
                 lcSalesman
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

tila = true.
{utuloste.i "return"}

run contrep (lcInvGroup[1],
             lcInvGroup[2],
             liCustNum[1],
             liCustNum[2],
             lcReseller[1],
             lcReseller[2],
             lcSalesman[1],
             lcSalesman[2],
             liContrType[1],
             liContrType[2],
             ldtToDate[1],
             ldtToDate[2],
             ldtClDate[1],
             ldtClDate[2],
             liClosed).

tila = false.
{utuloste.i}

MESSAGE "Contract report is finished."
VIEW-AS ALERT-BOX
TITLE " Report done ".  

hide message no-pause.
hide frame valinta no-pause.    
