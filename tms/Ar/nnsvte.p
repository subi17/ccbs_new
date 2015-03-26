/* ===========================================================================
 MODULE ........: nnsvte.p
 APPLICATION ...: nn
 TASK ..........: Checks existence of direct debit authorization
 CREATED .......: 22.11.1999
 CHANGED .......: 02.11.2001 ht 
                  30.01.2002 ht  seeking based on event DATE (FIND LAST)
                  20.11.2002/aam new input parameter idtDate
                                 -> different logic

 VERSION .......: M15
 ============================================================================*/

DEF INPUT  PARAMETER CustNum       AS I FORMAT "zzzzzz9" NO-UNDO.
DEF INPUT  PARAMETER idtDate      AS DATE               NO-UNDO. 
DEF OUTPUT PARAMETER ptili        AS C FORMAT "x(14)"   NO-UNDO.

DEF VAR ldaEvDate AS DA NO-UNDO.

DEF BUFFER bDDAuth FOR DDAuth.

/* Default: No direct debit authorzation */
ptili = "".

IF idtDate = ? THEN idtDate = TODAY.


   FIND LAST DDAuth 
       WHERE DDAuth.CustNum  = CustNum  AND 
             DDAuth.AuthDate <= idtDate
   NO-LOCK NO-ERROR.

   IF AVAILABLE DDAuth THEN DO:
      /********************************************* 
      *  IF there are, event DATE is checked AND   *
      *  the LAST one possible is used.            *
      *  AND searching is continued...             *
      *********************************************/

      /* NEW, change AND maintenance */
      IF DDAuth.ddProcess = 1  OR
         DDAuth.ddProcess = 2  OR 
         DDAuth.ddProcess = 4  THEN DO:

         ASSIGN ptili = DDAuth.BankAcc. 
      END.

      /* Termination */   
      ELSE DO:

         ASSIGN 
            ptili = "".

         /* Test special case, when
            something ELSE is WITH termination on the same DATE */ 
         FIND LAST bDDAuth WHERE
                   bDDAuth.CustNum   = DDAuth.CustNum   AND 
                   bDDAuth.AuthDate  = DDAuth.AuthDate AND
                   bDDAuth.ddProcess NE 3             NO-LOCK NO-ERROR.

         /* IF found, must use this */
         IF AVAIL bDDAuth THEN DO:
            ASSIGN ptili = bDDAuth.BankAcc. 
         END.

      END.

   END.

