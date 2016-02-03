/* ----------------------------------------------------------------------
  MODULE .......: substat.p
  TASK .........: report for unpaid subscriptions
  APPLICATION ..: 
  AUTHOR .......: jp
  CREATED ......: 
  CHANGED ......: 27.11.2003 jp forced lock for creditloss customer
                  21.03.2005 aam&jp function finvbal&fcreditloss
  Version ......: M15
  ---------------------------------------------------------------------- */

{Syst/commali.i}       
{Func/timestamp.i}
{Func/finvbal.i}
{Mm/subscription_status3.i}
{Func/email.i}
{Func/excel.i}
{Func/sog.i}

def var CustomerFrom as I no-undo FORMAT "zzzzzzzz9".
def var CustomerTo   as I no-undo FORMAT "zzzzzzzz9" .
def var fromstamp as de no-undo.
def var tostamp as de no-undo.
def var lkm as i no-undo.
def var ufkey as lo no-undo.
def var total as i no-undo.
def var rowtotal as i no-undo.
DEF VAR filename AS c no-undo format "x(40)".
DEF VAR llcheck  AS LO NO-undo.
DEF VAR lcAction AS CHAR NO-UNDO.
DEF VAR ldDeposit AS DE No-UNDO.

DEF VAR li        AS INT NO-UNDO.

DEF BUFFER zzCustomer FOR Customer.

form
 skip(2)
 "  This program will go through customer's unpaid balance one "       SKIP 
 "  subscription at a time. Program will create requests to LOCK or UNLOCK"                                                                     SKIP
 "  subscription according to pre-defined rules." SKIP(1) 
 "  Created requests are saved and can be browse from Saldoalarms program."
                                                                   SKIP(1)
 "         Customer from ...:" CustomerFrom no-label  SKIP
 "         Customer to .....:" CustomerTo no-label    SKIP
 "         Output file .....:" filename               SKIP
 "         Create SoLog req.:" llcheck no-label       SKIP
 skip(4)
with row 1 width 80 NO-LABELS
   title " " + ynimi + " SUBSCRIPTION STATUS  " + string(pvm,"99-99-99") + " "
FRAME rajat.

assign 
CustomerTo   = 999999999
CustomerFrom = 0
filename     = "/tmp/SS" +
               STRING(year(today),"9999") + 
               string(month(today),"99")  + 
               string(day(today),"99")    + 
               "_"                        +
               REPLACE(STRING(time,"hh:mm:ss"),":","") .
repeat:

loop:
repeat with frame rajat:
   PAUSE 0 no-message.
   ehto = 9. RUN ufkey.
   UPDATE 
   CustomerFrom 
   CustomerTo
   filename
   llcheck .

   ASSIGN
      ufk = 0
      ufk[1] = 132
      ufk[5] = 63
      ufk[8] = 8
      ehto = 0
   ufkey = true.

   run ufkey.
   case toimi:
      when 8 then return.
      when 1 then next loop.
      when 5 then leave loop.
   end.

end.

if keylabel(lastkey) = "f4" then return.

for each ttchange .
    delete ttchange.
end.

   MESSAGE "Check Customer no:". 

   FOR EACH Customer where
            Customer.CustNum >= CustomerFrom AND 
            Customer.CustNum <= CustomerTo  No-LOCK.

      PUT SCREEN row 22 col 20  string(customer.custnum).

      run subscription_status2(input Customer.CustNum,
                               input FALSe,
                               output table tt-subs-stat2).

      FOR EACH  tt-subs-stat2.
         IF tt-subs-stat2.functionality begins "6: open" THEN DO:
            FOR EACH mobsub WHERE 
                     mobsub.CustNum  = tt-subs-stat2.custno AND 
                     mobsub.Msstatus = 30 NO-LOCK .
            
                CREATE ttChange.
                ASSIGN
                   ttChange.ttfunction = tt-subs-stat2.functionality
                   ttChange.CustNum    = mobsub.custNum
                   ttChange.a-nr       = mobsub.cli.
            ENd.
         END.
         ELSE IF tt-subs-stat2.functionality begins "1:GSM" THEN DO:
            FOR EACH mobsub WHERE
                     mobsub.CustNum  = tt-subs-stat2.custno AND
                     mobsub.cli      = tt-subs-stat2.a-nr   AND 
                     mobsub.Msstatus ne 30 NO-LOCK .
                      
               CREATE ttChange.
               ASSIGN
                    ttChange.ttfunction = tt-subs-stat2.functionality
                    ttChange.CustNum    = mobsub.custNum
                    ttChange.a-nr       = mobsub.cli.
            ENd.
         END.
         delete tt-subs-stat2.
      end.
   END.    

   OUTPUt STREAM excel TO VALUE(filename).

   PUT STREAM excel UNFORMATTED
   "CustNum;CLI;Reason;Operation;Customer Deposit" my-nl.
    
   
   FOR EACH ttchange NO-LOCK.

      FIND mobsub WHERE
           mobsub.cli = ttChange.a-nr no-lock no-error.
 
      FIND customer WHERE 
           customer.CustNum = mobsub.CustNum NO-LOCK NO-ERROR.
           
      FIND CustBal WHERE 
           CustBal.Custnum = Customer.custnum NO-LOCK NO-ERROR.
      IF AVAIL custbal THEN lddeposit = custbal.deposit.
      else lddeposit = 0 .
      
      IF AVAIL mobsub THEN 
      PUT STREAM excel UNFORMATTED
         ttChange.Custnum   ";"
         ttChange.a-nr      ";"
         (IF TTChange.ttFunction Begins "1:GSM" THEN "LOCK;"
         ELSE "UNLOCK;")  
         TTChange.ttfunction ";"
         lddeposit my-nl.
      ELSE DO:
         PUT STREAM excel UNFORMATTED
         ttChange.Custnum   ";"
         ttChange.a-nr      ";"
         (IF TTChange.ttFunction Begins "1:GSM" THEN "LOCK;"
         ELSE "UNLOCK;") 
         TTChange.ttfunction ";" lddeposit my-nl   my-nl.
         NEXT.
      END.

      li = li + 1.

      IF llCheck  THEN DO:

         IF TTChange.ttFunction Begins "1:GSM" THEN lcAction = "LOCK".
         ELSE lcAction = "UNLOCK".
         
         CREATE CallAlarm.
         ASSIGN
            CallAlarm.ActStamp   = fmakets()
            CallAlarm.CLSeq      = 1
            CallAlarm.CASeq      = 1
            CallAlarm.CustNo     = ttChange.CustNum
            CallAlarm.CLI        = ttChange.a-nr
            CallAlarm.DeliStat   = 1            
            CallAlarm.Delitype   = 4
            CallAlarm.DeliPara   = lcAction
            CallAlarm.DeliMsg    = lcAction
            CallAlarm.Limit      = 100
            CallAlarm.Brand      = gcBrand 
            CallAlarm.Orig       = "800622800"
            CallAlarm.CreditType = 66.

         RELEASE CallAlarm.
      
      ENd.
   END.

   IF li > 0 THEN
   MESSAGE
   "Totally found: " li
   VIEW-AS ALERT-BOX.
END.

