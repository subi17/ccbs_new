 /* /mobile/gateway/subscription_status.i.i */

/* 27.11.2003 jp forced closed for creditloss customer */

/***************************************
* INPUT DATA TO TMS:                   *
***************************************/

DEFINE TEMP-TABLE ttChange
FIELD ttfunction AS C
FIELD Custnum     AS I FORMAT "zzzzzzzz9"
FIELD a-nr       AS C  FORMAT "x(14)"
INDEX ttfunction ttfunction Custnum a-nr.

DEFINE TEMP-TABLE tt-subs-stat2
    FIELD custno          AS I    FORMAT "zzzzzzzz9"
    FIELD a-nr            AS CHAR FORMAT "x(12)" 
    FIELD contractID      AS CHAR
    FIELD invno           AS CHAR FORMAT "X(50)"
    FIELD invprice        AS DEC
    FIELD unpaid          AS DEC
    FIELD functionality   AS CHAR FORMAT "X(50)"
    FIELD pierror         AS int
    FIELD invstatus       AS CHAR .


DEFINE temp-table Tinvno       
    FIELD invno like Invoice.Invnum
INDEX invno invno.
 
DEF BUFFER xxcustomer FOR customer.
   
PROCEDURE subscription_status2.

   DEF INPUT   PARAMETER custno  AS   INT.
   DEF INPUT   PARAMETER invspec AS   LOGICAL NO-UNDO.
   DEF OUTPUT  PARAMETER TABLE   FOR  tt-subs-stat2.

   DEF var piError     AS I    NO-UNDO.
   DEF VAR balance     AS DE   NO-UNDO.
   DEF VAR invdate     AS INT  NO-UNDO init 14.
   DEF VAR custno1     AS INT  NO-UNDO.
   DEF VAR custno2     AS INT  NO-UNDO.
   DEF VAR info        AS CHAR NO-UNDO.
   DEF VAR cprice      AS DEC  NO-UNDO. 
   def var unpaidbal   AS DEC  NO-UNDO.
   DEF VAR fixoverdue  AS INT  NO-UNDO init 24.
   DEF VAR moboverdue  AS INT  NO-UNDO init 24.
   DEF VAR fixlimit    AS INT  NO-UNDO init 0.
   DEF VAR moblimit    AS INT  NO-UNDO init 50.
   DEF VAR forced      AS LOG  NO-UNDO init FALSE.
   DEF VAR ContIDList  AS CHAR NO-UNDO.
   DEF VAR oiAcct      AS INT  NO-UNDO.
   
   {Func/cparam.i DefFixOverduedays return}. fixoverdue  = tmsparam.IntVal. 
   {Func/cparam.i DefMobOverduedays return}. moboverdue  = tmsparam.IntVal. 
   {Func/cparam.i DefFixUnpdaiLimit return}. fixlimit    = tmsparam.IntVal. 
   {Func/cparam.i DefMobUnpaidLimit return}. moblimit    = tmsparam.IntVal. 

   for each Tinvno.
       delete Tinvno.
   end.

   if custno = 0 THEN ASSIGN custno1 = 1000   custno2 = 999999999.
   ELSE               ASSIGN custno1 = custno custno2 = custno.

   cprice = 0 .


   FOR EACH Customer NO-LOCK WHERE
            Customer.CustNum >= CustNo1           AND
            Customer.CustNum <= CustNo2 . 
      ASSIGN
         unpaidbal = 0
         forced    = false.

      FOR EACH Invoice NO-LOCK WHERE 
               Invoice.Brand    = gcBrand              AND 
               Invoice.CustNum  = Customer.CustNum     AND 
               Invoice.dueDate <= today - moboverdue   AND 
               Invoice.Invtype  = 1                    AND 
               Invoice.CrInvNum = 0            , 
         FIRST ClaimHist WHERE
               ClaimHist.InvNum     = Invoice.InvNum AND
               ClaimHist.Claim      >= 2             AND
               Claimhist.ClaimDate <= Today - 7 NO-LOCK .

         IF CAN-FINd(FIRST PaymPlan WHERE 
                           PaymPlan.CustNum = Invoice.custnum AND 
                           PaymPlan.PPStatus = 3) THEN DO:
            forced = true.
            NEXT.
         END.
        
         balance = fInvBal (BUFFER Invoice, today) + 
                   fCredLossPaid(BUFFER invoice,today, OUTPUT oiAcct) .

         IF Invoice.PaymStat = 3 THEN forced = true.
         
         IF balance >= 0 OR forced THEN DO:
            CREATE Tinvno.
            ASSIGN
            Tinvno.invno = Invoice.InvNum.
         END.

         unpaidbal = unpaidbal + balance.

         IF forced then unpaidbal = 0 .
      
      END.

      IF Forced THEN DO:
         CREATE tt-subs-stat2.
         ASSIGN
            tt-subs-stat2.custno        = custno 
            tt-subs-stat2.invprice      = unpaidbal
            tt-subs-stat2.functionality = "1:GSM (creditloss) " 
            tt-subs-stat2.pierror       = 0.
      END.
      ELSE IF unpaidbal > moblimit  THEN
      FOR EACH Tinvno,
          EACH Invoice NO-LOCK WHERE
               Invoice.InvNum   = Tinvno.invno         AND
               Invoice.CustNum  = Customer.CustNum,
          EACH xxCustomer WHERE 
               xxCustomer.InvCust = Invoice.Custnum,
          
          EACH Mobsub WHERE 
              Mobsub.CustNum = xxcustomer.CustNum AND 
              Mobsub.msstatus <= 8                AND 
              MobSub.Msstatus >  2
          NO-LOCK.
            
             
             FIND first tt-subs-stat2 WHERE 
                        tt-subs-stat2.custno = Invoice.CustNum AND 
                        tt-subs-stat2.a-nr          = Mobsub.cli NO-ERROR.

             IF avail tt-subs-stat2 THEN NEXT.           
             
             CREATE tt-subs-stat2.                               
             ASSIGN              
                tt-subs-stat2.custno        = Invoice.CustNum
                tt-subs-stat2.a-nr          = Mobsub.cli
                tt-subs-stat2.ContractID    = ""
                tt-subs-stat2.invno         = STRING(Invoice.InvNum)           
                tt-subs-stat2.unpaid        = cprice
                tt-subs-stat2.invprice      = unpaidbal
                tt-subs-stat2.functionality = "1:GSM(unpaid invoice)"
                tt-subs-stat2.pierror       = 0 .
      END.

      ASSIGN
         unpaidbal = 0
         forced    = false.


      FOR EACH Invoice NO-LOCK WHERE 
               Invoice.Brand    = gcBrand              AND 
               Invoice.CustNum  = Customer.CustNum     AND 
               Invoice.Invtype  = 1                    AND 
               Invoice.CrInvNum = 0 .

         IF  Invoice.duedate > today THEN NEXT.
                          
         run invbal(INPUT Invoice.InvNum, OUTPUT balance).

         unpaidbal = unpaidbal + balance.
      END.

      IF unpaidbal < moblimit then 
      FOR EACH Tinvno,
          EACH Invoice NO-LOCK WHERE
                   Invoice.InvNum   = Tinvno.invno         AND
                   Invoice.CustNum  = Customer.CustNum,
              EACH xxCustomer WHERE
                   xxCustomer.InvCust = Invoice.Custnum,
              EACH Mobsub WHERE
                   Mobsub.CustNum = xxcustomer.CustNum AND
                   Mobsub.msstatus = 10  NO-LOCK.
       
         
         IF NOT CAN-FIND(FIRST tt-subs-stat2 WHERE
                               tt-subs-stat2.custno  = Invoice.CustNum) 
         THEN DO:
             
             CREATE tt-subs-stat2.
             ASSIGN
             tt-subs-stat2.custno        = custno 
             tt-subs-stat2.invprice      = unpaidbal
             tt-subs-stat2.functionality = "6: Open " 
             tt-subs-stat2.pierror       = 0.
         END.
      END.
   END.  /* for each Customer */



END PROCEDURE.

