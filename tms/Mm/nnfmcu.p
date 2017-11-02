/* ----------------------------------------------------------------------
  MODULE .......: nnfmcu.p
  TASK .........: FIND a mobile customer
  APPLICATION ..: nn
  AUTHOR .......: pt
  CREATED ......: 31-05-99
  CHANGED ......: 22.01.01 pt NEW OUTPUT PARAMETER MsSeq
                  06.02.03 jp f7
  Version ......: M15
  ---------------------------------------------------------------------- */

{Syst/commali.i}


def var arg  as c format "x(40)".

DEF OUTPUT PARAMETER CustNum AS i NO-UNDO.
DEF OUTPUT PARAMETER MsSeq AS I  NO-UNDO.
DEFINE VARIABLE gcBrand AS CHARACTER NO-UNDO.
gcBrand = Syst.Var:gcBrand.

form
skip(1)
"  NOTE:   You can find a customer with different arguments." skip
"          Enter the argument that is known and then        " skip
"          select appropriate search method by pressing     " skip
"          appropriate Function Key."                         skip(1)
"          You shall then receive customer's number if      " skip
"          a customer could be found with that argument."     skip(1)
"  Brand.:" gcBrand   HELP "Brand code (*=ALL)"               SKIP
"  Search:" arg                                               skip(1)
WITH 
    centered overlay title " FIND CUSTOMER "  NO-LABELS
    ROW 2 FRAME frm.

ASSIGN
CustNum = 0
MsSeq = 0.


PAUSE 0.
MAIN:
repeat WITH FRAME frm:
   Syst.Var:ehto = 9. RUN Syst/ufkey.p.
   UPDATE gcBrand WHEN Syst.Var:gcAllBrand = TRUE arg.


ACTION:
   repeat WITH FRAME frm:
      ASSIGN
      Syst.Var:ufk    =   0
      Syst.Var:ufk[1] =   7
      Syst.Var:ufk[2] = 209  /* MSISDN       */
      Syst.Var:ufk[3] = 210  /* ICC of SIM   */
      Syst.Var:ufk[4] = 211  /* FIND IMSI    */
      Syst.Var:ufk[5] = 715  /* FIND A-Sub   */
      Syst.Var:ufk[6] = 94   /* FIND invoice */
      Syst.Var:ufk[7] = 93
      Syst.Var:ufk[8] = 8
      Syst.Var:ehto   = 0.  RUN Syst/ufkey.p.

      IF Syst.Var:toimi = 8 THEN LEAVE MAIN.

      IF Syst.Var:toimi = 1 THEN NEXT MAIN.

      IF Syst.Var:toimi = 2 THEN DO:
         IF gcBrand ne "*" THEN 
         FIND MSISDN where 
              MSISDN.CLI = arg  AND
              MSISDN.brand = gcBrand no-lock no-error.
         ELSE
         FIND MSISDN where
              MSISDN.CLI = arg NO-LOCK NO-ERROR.

         IF AVAIL MSISDN THEN DO:

            IF MSISDN.CustNum =  0 THEN DO:
               MESSAGE
               "This MSISDN No is formally OK" SKIP
               "but it does not belong to any" SKIP
               "customer at the moment"
               VIEW-AS ALERT-BOX error.
               NEXT Action.
            END.
                                                                                           CustNum = MSISDN.CustNum.
            IF gcBrand ne "*" THEN 
            FIND MobSub WHERE 
                 MobSub.CLI   = MSISDN.CLI AND 
                 Mobsub.Brand = gcBrand NO-LOCK NO-ERROR.
            ELSE FIND MobSub WHERE
                      MobSub.CLI   = MSISDN.CLI NO-LOCK NO-ERROR.

            IF AVAIL MobSub THEN ASSIGN
               MsSeq = MobSub.MsSeq
               Syst.Var:gcBrand = mobsub.brand.
            LEAVE MAIN.
         END.
       END.


      IF Syst.Var:toimi = 3 THEN DO:
         IF gcBrand ne "*" THEN 
         FIND SIM where 
              SIM.ICC   = arg AND 
              SIM.Brand = gcBrand no-lock no-error.
         ELSE  FIND SIM where
                    SIM.ICC   = arg  no-lock no-error.
         IF AVAIL SIM THEN DO:

            IF SIM.CustNum =  0 THEN DO:
               MESSAGE 
               "This ICC No is formally OK" SKIP
               "but the SIM card where it"
               "exists does NOT belong to any" SKIP
               "customer at the moment"
               VIEW-AS ALERT-BOX error.
               NEXT Action.
            END.
            CustNum = SIM.CustNum.

            /* Try TO FIND an active MobSub THRU MSOwner table */
            FIND FIRST IMSI where IMSI.ICC = SIM.ICC no-lock NO-ERROR.
            IF AVAIL IMSI THEN DO:
               FIND FIRST MSOwner WHERE
                          MSOwner.IMSI = IMSI.IMSI AND
                          MSOwner.TsEnd >= 99999999
               NO-LOCK NO-ERROR.
               IF AVAIL MSOwner THEN ASSIGN
                  Syst.Var:gcBrand = msowner.Brand 
                  MsSeq   = MSOwner.MsSeq.
            END.
            LEAVE MAIN.
         END.   
      END.

      IF Syst.Var:toimi = 4 THEN DO:

         FIND IMSI where 
              IMSI.IMSI = arg no-lock no-error.
         
         IF AVAIL IMSI THEN DO:
            FIND FIRST sim WHERE 
                       sim.icc  = imsi.icc AND
                       sim.Brand = gcBrand 
            NO-LOCK NO-ERROR.
       
            IF NOT AVAIL sim THEN NEXT.

            IF IMSI.CustNum =  0 THEN DO:
               MESSAGE 
               "This IMSI No is formally OK" SKIP
               "but its does NOT belong to any" SKIP
               "customer at the moment"
               VIEW-AS ALERT-BOX error.
               NEXT Action.
            END.
            CustNum = IMSI.CustNum.
            /* FIND a MobSub */
            FIND FIRST MSOwner WHERE
                       MSOwner.IMSI = IMSI.IMSI AND
                       MSOwner.TsEnd >= 99999999
            NO-LOCK NO-ERROR.
            IF AVAIL MSOwner THEN ASSIGN
               Syst.Var:gcBrand = msowner.Brand 
               MsSeq   = MSOwner.MsSeq.
            LEAVE MAIN.
         END.   
      END.

      IF Syst.Var:toimi = 5 THEN DO:
         IF gcBrand ne "*" THEN 
         FIND MobSub where 
              Mobsub.Brand = gcBrand AND 
              MobSub.CLI   = arg no-lock no-error.
         ELSE
         FIND MobSub where MobSub.CLI   = arg no-lock no-error.

         IF AVAIL MobSub THEN DO:
            ASSIGN
               CustNum = MobSub.CustNum 
               Syst.Var:gcBrand = mobsub.brand.
            LEAVE MAIN.
         END.   
      END.

      IF Syst.Var:toimi = 6 THEN DO:
         IF gcBrand ne "*" THEN 
         FIND Invoice where 
              Invoice.InvNum = integer(arg) AND
              Invoice.Brand  = gcBrand no-lock no-error.
         ELSE  FIND Invoice where
                    Invoice.InvNum = integer(arg) no-lock no-error.

         IF AVAIL Invoice THEN DO:
            ASSIGN 
               CustNum = Invoice.CustNum
               Syst.Var:gcBrand = Invoice.Brand.
            LEAVE MAIN.
         END.   
      END.  

      IF Syst.Var:toimi = 7 THEN DO:
         IF gcBrand ne "*" THEN 
         FIND FIRST Customer  where 
                    Customer.CustName Begins arg AND
                    Customer.Brand  = gcBrand NO-LOCK NO-ERROR.
         ELSE FIND FIRST Customer where
                         Customer.CustName Begins arg  NO-LOCK NO-ERROR.

         IF AVAIL Customer THEN DO:
            ASSIGN 
               CustNum = Customer.CustNum
               Syst.Var:gcBrand = Customer.Brand.
            LEAVE MAIN.
         END.   
      END.

      MESSAGE 
         "No customer was found with" SKIP
         "given argument"
         VIEW-AS ALERT-BOX error.

   END.

END.


