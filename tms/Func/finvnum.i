/* finvnum.i        13.05.03/aam

   get invoice number (also for credit) from defined invoicing group
   
   changed:         28.11.06/aam numbers from IGInvNum,
                                 handle invnum as character
*/

DEF BUFFER bCurInvGroup FOR InvGroup.
DEF BUFFER bSeqIGInvNum FOR IGInvNum.

/* check which group's number sequence should be used */
FUNCTION fSeqInvGroup RETURNS CHARACTER
   (icInvGroup AS CHAR).

   DEF VAR lcSeqGroup AS CHAR NO-UNDO.
   
   FIND bCurInvGroup NO-LOCK WHERE
        bCurInvGroup.Brand    = gcBrand AND
        bCurInvGroup.InvGroup = icInvGroup NO-ERROR.
   IF NOT AVAILABLE bCurInvGroup THEN RETURN "".
   
   IF bCurInvGroup.InvForm > ""
   THEN lcSeqGroup = bCurInvGroup.InvForm.
   ELSE lcSeqGroup = bCurInvGroup.InvGroup.
   
   RETURN lcSeqGroup.
   
END FUNCTION.

/* next free number */
FUNCTION fGetInvNum RETURNS CHARACTER
   (INPUT  icInvGroup AS CHAR,
    INPUT  iiType     AS INT,
    INPUT  idtInvDate AS DATE,
    OUTPUT ocPrefix   AS CHAR).

   icInvGroup = fSeqInvGroup(icInvGroup).
   
   FOR FIRST bSeqIGInvNum NO-LOCK WHERE
             bSeqIGInvNum.Brand     = gcBrand    AND
             bSeqIGInvNum.InvGroup  = icInvGroup AND 
             bSeqIGInvNum.InvType   = iiType     AND 
             bSeqIGInvNum.FromDate <= idtInvDate:

      ocPrefix = bSeqIGInvNum.SeqPrefix.
      
      RETURN bSeqIGInvNum.SeqPrefix + 
             STRING(bSeqIGInvNum.InvNum + 1,"99999999").
   END.

   /* try general if dedicated was not found */
   FOR FIRST bSeqIGInvNum NO-LOCK WHERE
             bSeqIGInvNum.Brand     = gcBrand    AND
             bSeqIGInvNum.InvGroup  = icInvGroup AND 
             bSeqIGInvNum.InvType   = 0          AND 
             bSeqIGInvNum.FromDate <= idtInvDate:

      ocPrefix = bSeqIGInvNum.SeqPrefix.

      RETURN bSeqIGInvNum.SeqPrefix + 
             STRING(bSeqIGInvNum.InvNum + 1,"99999999").
   END.

   /* none found */
   RETURN "".

END FUNCTION.

/* mark last used number to group */
FUNCTION fUpdateInvNum RETURNS LOGICAL
   (icInvGroup AS CHAR,
    iiType     AS INT,
    idtInvDate AS DATE,
    icInvNum   AS CHAR).  
   
   DEF VAR liUpdInvNum AS INT NO-UNDO.
   DEF VAR liSeqInvNum AS INT NO-UNDO.
   
   icInvGroup = fSeqInvGroup(icInvGroup).
   
   updinvgroup:
   DO WHILE TRUE:
   
      FIND FIRST bSeqIGInvNum EXCLUSIVE-LOCK WHERE
                 bSeqIGInvNum.Brand    = gcBrand    AND
                 bSeqIGInvNum.InvGroup = icInvGroup AND
                 bSeqIGInvNum.InvType  = iiType     AND
                 bSeqIGInvNum.FromDate <= idtInvDate 
                 NO-ERROR NO-WAIT.

      /* if group is locked, don't try too long; procedures that create
         invoices check anyway if number to be used is already taken */
      IF LOCKED(bSeqIGInvNum) THEN DO:
         liUpdInvNum = liUpdInvNum + 1.
         IF liUpdInvNum > 3 THEN DO:
            RETURN FALSE.
         END.
         PAUSE 5 NO-MESSAGE.
         NEXT updinvgroup.
      END.

      ELSE IF NOT AVAILABLE bSeqIGInvNum THEN DO:
         /* update general if dedicated was not found */
         IF iiType > 0 THEN DO:
            iiType = 0.
            NEXT updinvgroup.
         END.
            
         RETURN FALSE.
      END.
      
      ELSE DO:

         /* remove prefix (don't use replace) */
         IF bSeqIGInvNum.SeqPrefix > "" THEN DO:
            IF icInvNum BEGINS bSeqIGInvNum.SeqPrefix THEN DO:
               IF LENGTH(icInvNum) > LENGTH(bSeqIGInvNum.SeqPrefix)
               THEN icInvNum = SUBSTRING(icInvNum,
                                         LENGTH(bSeqIGInvNum.SeqPrefix) + 1).
               ELSE icInvNum = "".
            END.
            
            /* don't update if prefix is not the same */
            ELSE icInvNum = "".
         END.
         
         liSeqInvNum = INTEGER(icInvNum) NO-ERROR.
         
         /* invalid integer value */
         IF ERROR-STATUS:ERROR THEN RETURN FALSE.
         
         IF liSeqInvNum > bSeqIGInvNum.InvNum THEN 
             bSeqIGInvNum.InvNum = liSeqInvNum.
               
         RELEASE bSeqIGInvNum.
      
         RETURN TRUE. 
      END. 
      
   END.

END FUNCTION.

