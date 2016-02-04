/* --------------------------------------------------------------------
  MODULE .......: posrep.p
  FUNCTION .....: Print pos-salesman report
  APPLICATION ..: TMS
  AUTHOR .......: tk  
  CREATED ......: 22.06.04
  MODIFIED .....: 
  VERSION ......: TeleF
  ------------------------------------------------------------------ */

{Syst/testpaa.i}
{Func/excel.i}
{Func/email.i}
{Func/timestamp.i}

DEF VAR Reseller  LIKE Reseller.Reseller NO-UNDO.
DEF VAR dFrom     AS DATE FORMAT "99-99-99" NO-UNDO.
DEF VAR dTo       AS DATE FORMAT "99-99-99" NO-UNDO.
DEF VAR FromStamp AS DE NO-UNDO.
DEF VAR ToStamp   AS DE NO-UNDO.
DEF VAR lcStatus  AS CH NO-UNDO.
DEF VAR CLI       LIKE Order.CLI NO-UNDO.
DEF VAR Salesman1 LIKE Salesman.Salesman NO-UNDO.
DEF VAR Salesman2 LIKE Salesman.Salesman NO-UNDO.

form
 skip(2)
 "      This program will print out all subscriptions related to"
 "      given reseller."
 skip(2)
 "         Reseller Code ...:" Reseller Reseller.RSName SKIP
 "         Salesman Code ...:" Salesman1 Salesman2      SKIP
 "         Order date from .:" dFrom SKIP
 "         Order date to ...:" dTo VALIDATE(INPUT dFrom <= INPUT dTo,
                                            "Invalid order !")  SKIP
 "         CLI .............:" CLI                                  
 skip(6)
with row 1 width 80 NO-LABELS
   title " " + ynimi + " CREATE RESELLER " + string(pvm,"99-99-99") + " "
FRAME rajat.

ASSIGN
   dTo   = TODAY
   dFrom = TODAY - 30.

loop:
repeat with frame rajat:
   PAUSE 0 no-message.
   ehto = 9. RUN Syst/ufkey.
   UPDATE 
      Reseller
      Salesman1
      Salesman2
      dFrom
      dTo
      CLI
   EDITING:
      readkey.
      
      HIDE MESSAGE NO-PAUSE.
      
      IF lookup(keylabel(LASTKEY),poisnap) > 0 THEN DO:
         IF FRAME-FIELD = "Reseller" THEN DO:
            ASSIGN Reseller.
            IF Reseller = "" THEN DO:
               DISP "All" @ Reseller.RSName.
            END.
            ELSE DO:
               FIND FIRST Reseller NO-LOCK WHERE
                          Reseller.Brand  = gcBrand AND
                          Reseller.Reseller = Reseller NO-ERROR.
               IF NOT AVAIL Reseller THEN DO:
                  MESSAGE "Unknown Reseeller !".
                  NEXT.
               END.
               ELSE DISP Reseller.RSName. 
            END.
         END.   
         
         IF FRAME-FIELD = "Salesman1" THEN DO:
            ASSIGN Salesman1.
            IF Salesman1 NE "" THEN DO:
               FIND FIRST Salesman NO-LOCK WHERE
                          Salesman.Brand    = gcBrand AND
                          Salesman.Salesman = Salesman1 NO-ERROR.
               IF NOT AVAIL Salesman THEN DO:
                  MESSAGE "Unknown Salesman !".
                  NEXT.
               END.
               ELSE IF Reseller NE "" AND 
                       Salesman.Reseller NE Reseller THEN DO:
                  MESSAGE "Saleman/Reseller mismatch !".
                  NEXT.
               END.
               ELSE DISP Reseller.RSName. 
            END.
         END.   
         
         IF FRAME-FIELD = "Salesman2" THEN DO:
            ASSIGN Salesman2.
            IF Salesman2 NE "" THEN DO:
               FIND FIRST Salesman NO-LOCK WHERE
                          Salesman.Brand    = gcBrand AND
                          Salesman.Salesman = Salesman2 NO-ERROR.
               IF NOT AVAIL Salesman THEN DO:
                  MESSAGE "Unknown Salesman !".
                  NEXT.
               END.
               ELSE IF Reseller NE "" AND 
                       Salesman.Reseller NE Reseller THEN DO:
                  MESSAGE "Saleman/Reseller mismatch !".
                  NEXT.
               END.
               ELSE DISP Reseller.RSName. 
            END.
         END.   
         
      END.

      apply lastkey.
   END.
   
   ASSIGN
      ufk = 0
      ufk[1] = 132
      ufk[5] = 795
      ufk[8] = 8
      ehto = 0.

   RUN Syst/ufkey.
   case toimi:
      when 8 then return.
      when 1 then next loop.
      when 5 then leave loop.
   end.

end.

if keylabel(lastkey) = "f4" then return.

ASSIGN 
   FromStamp = fHMS2TS(dFrom,"00:00:00")
   ToStamp   = fHMS2TS(dTo,"23:59:59").

output stream excel to /tmp/posrep.txt.

put stream excel unformatted
   "Reseller"      tab
   "Salesman"      tab
   "Order Time"    tab
   "OrderId"       tab
   "Order Status"  tab
   "Customer Name" tab
   "OrgId"         tab
   "CLI"           tab
   "Mobsub Status" tab
   "Activationdate"  skip.


FOR EACH Reseller NO-LOCK WHERE
         Reseller.Brand    = gcBrand AND
         Reseller.Reseller = Reseller,
   EACH Salesman NO-LOCK WHERE
        Salesman.Brand    = gcBrand AND
        Salesman.Reseller = Reseller.Reseller,
   EACH Order NO-LOCK WHERE 
        Order.Brand = gcBrand AND
        Order.CrStamp <= FromStamp AND
        Order.CrStamp >= ToStamp   AND
        Order.Salesman = Salesman.Salesman:

   FIND FIRST TMSCodes WHERE 
              TMSCodes.TableName = "Order" AND
              TMSCodes.FieldName = "StatusCode" AND
              TMSCodes.CodeGroup = "Orders" AND
              TMSCodes.CodeValue = Order.StatusCode
   NO-LOCK NO-ERROR.
   IF AVAIL TMSCodes THEN lcStatus = TMSCodes.CodeName.
   ELSE lcStatus = "".
          
   FIND MobSub NO-LOCK WHERE
        MobSub.Brand = gcBrand AND
        MobSub.MsSeq = Order.MSSeq NO-ERROR.

   FIND FIRST OrderCustomer NO-LOCK WHERE
              OrderCustomer.Brand   = gcBrand       AND
              OrderCustomer.OrderID = Order.OrderID AND
              OrderCustomer.RowType = 1 NO-ERROR.
   IF NOT AVAILABLE OrderCustomer THEN NEXT.             
              
   put stream excel unformatted
      Reseller.Reseller      tab
      Salesman.Salesman      tab
      fTS2HMS(Order.CrStamp) tab
      Order.OrderId          tab
      lcStatus               tab
      OrderCustomer.SurName1 + " " + OrderCustomer.Firstname tab
      OrderCustomer.CustId    tab
      Order.CLI.
    
   IF AVAIL MobSub then put stream excel unformatted
      Mobsub.MSStatus       tab
      Mobsub.Activationdate skip.
   ELSE put stream excel unformatted
      "not created" tab "-" skip.
         
END.         

output stream excel close.

MESSAGE "All done !" VIEW-AS ALERT-BOX.
        
