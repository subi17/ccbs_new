/* --------------------------------------------------------------------
  MODULE .......: calcint.p
  TASK .........: calculate Overtime Interests
  APPLICATION ..: nn
  AUTHOR .......: pt
  CREATED ......: 08-09-99
  CHANGED ......: 24.10.02/aam tuning 
                  11.09.03/aam brand 
                  12.01.04/aam input CustNum,
                               use parameters IntDelayPriv & IntDelayComp
  Version ......: M15
---------------------------------------------------------------------- */

{Syst/commali.i}
{Func/cparam2.i}

DEF INPUT  PARAMETER dueday    AS DA  NO-UNDO.
DEF INPUT  PARAMETER payday    AS DA  NO-UNDO.
DEF INPUT  PARAMETER intmet    AS i   NO-UNDO.
DEF INPUT  PARAMETER payment   AS DE  NO-UNDO.
DEF INPUT  PARAMETER iiCustNum AS INT NO-UNDO.
DEF OUTPUT PARAMETER qdays     AS c   NO-UNDO.
DEF OUTPUT PARAMETER qperc     AS c   NO-UNDO.
DEF OUTPUT PARAMETER qsum      AS DE  NO-UNDO.

DEF SHARED VAR intpro  AS DE EXTENT 10 NO-UNDO.
DEF SHARED VAR intdays AS I  EXTENT 10 NO-UNDO.
DEF SHARED VAR intsumm AS DE EXTENT 10 NO-UNDO.

DEF VAR ldIntPerc AS DE NO-UNDO.
DEF VAR xDate     AS DA NO-UNDO.
DEF VAR idays     AS i  NO-UNDO.

DEF VAR delday    AS i    NO-UNDO.
DEF VAR delmon    AS i    NO-UNDO.
DEF VAR i         AS i    NO-UNDO.
DEF VAR co        AS i    NO-UNDO.
DEF VAR liDelay   AS INT  NO-UNDO.
DEF VAR lcParam   AS CHAR NO-UNDO. 

ASSIGN intpro  = 0
       intdays = 0
       intsumm = 0
       liDelay = 0.

/* get delay (days) to be added to due date */
IF iiCustNum > 0 THEN DO:
   
   FIND Customer WHERE Customer.CustNum = iiCustNum NO-LOCK NO-ERROR.
   
   IF AVAILABLE Customer THEN DO:

      IF Customer.Category = "1" 
      THEN lcParam = "IntDelayPriv".
      ELSE lcParam = "IntDelayComp".
      
      liDelay = fCParamI(lcParam).
      
      IF liDelay = ? THEN liDelay = 0.

   END.

END.

/* add possible delay days to due date */
IF payday > dueday + liDelay THEN DO:  /* it was a late payment */

   /* interest is always calculated from due date, even if delay days for
      triggering interest calculation were defined */
   delday = payday - dueday.

   IF intmet = 1 /* Method #1: precise */ THEN DO:

      ASSIGN xDate = payday
             co = 1.

      FOR EACH Interest NO-LOCK WHERE
               Interest.Brand      = gcBrand AND 
               Interest.ValidFrom <= payday  AND
               Interest.IntType   < 2
      BY Interest.ValidFrom DESC:

         IF Interest.ValidFrom > dueday
         THEN ASSIGN idays = xDate - Interest.ValidFrom 
                     xDate = Interest.ValidFrom - 1.
         ELSE ASSIGN idays = xDate - dueday.             

         IF idays > 0 THEN DO:
            ASSIGN
            qdays        = qdays + (if qdays = ""
                                    then ""
                                    else ",") + 
                           string(idays) 
            qperc        = qperc + (if qperc = ""
                                    then ""
                                    else ",") + 
                           string(Interest.IntPerc * 100)  
            intsumm[co]  = round((idays / 365) * Interest.IntPerc * 
                                  payment /  100,2)
            qsum         = qsum + intsumm[co] 
            intpro[co]   = Interest.IntPerc.
            intdays[co]  = idays.
         END. 

         IF Interest.ValidFrom <= dueday THEN LEAVE.

         co = co + 1.
      END.

   END.

   ELSE IF intmet = 2 /*  x % per MONTH */ THEN DO:


      /* get latest Interest % */
      FOR EACH Interest NO-LOCK WHERE 
               Interest.Brand      = gcBrand AND
               Interest.ValidFrom <= payday  AND
               Interest.IntType   < 2
      BY Interest.ValidFrom DESC:

         /* subtract FIRST 'forgiveable' 5 days off the delday */
         ASSIGN

           delday = maximum(0, delday - 5). 
           delmon = 0.

         IF delday > 0 THEN DO:

            ASSIGN 
            /* calculate how many beginning months are included in the delday */
            delmon     = 1 + truncate(delday / 31,0)
            qsum       = round(delmon * payment * Interest.IntPerc / 100,2)

            intsumm[1] = qsum
            intpro[1]  = Interest.IntPerc
            intdays[1] = delday.
         END.

         LEAVE.

      END.
   END.     /*  intmet = 2 */

   ELSE DO: /* unknown method */
       MESSAGE
          "SYSTEM ERROR:" SKIP
          "Unknown Interest Calculation Method" intmet "was" SKIP
          "given for module calcint.p !"                     SKIP
          "CONTACT  TMS SUPPORT !"
       VIEW-AS ALERT-BOX ERROR.

       /* NOTHING IS DONE */

   END. /* unknown intmet */
END.



