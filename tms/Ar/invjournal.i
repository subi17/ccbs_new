/* invjournal.i        26.06.07/aam

   definitions for invoice journal
   callers:     nnlalu.p
                invjournal.p
                invjournalb.p
*/


DEF TEMP-TABLE TCustGroup NO-UNDO
   FIELD CustGroup LIKE CustGroup.CustGroup
   INDEX CustGroup CustGroup.

DEF TEMP-TABLE ttCriter NO-UNDO
   FIELD InvGroup    AS CHAR
   FIELD CustNum1    AS INT
   FIELD CustNum2    AS INT
   FIELD InvDate1    AS DATE
   FIELD InvDate2    AS DATE
   FIELD ExtInvID1   AS CHAR
   FIELD ExtInvID2   AS CHAR
   FIELD InvType1    AS INT
   FIELD InvType2    AS INT
   FIELD PaymState1  AS INT
   FIELD PaymState2  AS INT
   FIELD DenyRemind  AS LOG
   FIELD DenyPrint   AS LOG
   FIELD OnlyUnpaid  AS LOG
   FIELD ZeroVat     AS LOG
   FIELD Invoices    AS LOG
   FIELD InvAccounts AS LOG
   FIELD Summary     AS LOG
   FIELD ToFile      AS CHAR.
   


