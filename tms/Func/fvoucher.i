/* fvoucher.i       30.04.2002/aam 
   get AND UPDATE the voucher number series FOR payments
   use sequence PaymVouch 
                    17.09.2003/aam use table PaymVouch
                    05.01.2007/aam IgVoucher
*/

{commali.i}

DEF BUFFER bCurInvGroup  FOR InvGroup.
DEF BUFFER bSeqIGVoucher FOR IGVoucher.

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
FUNCTION fGetExtVoucher RETURNS CHARACTER
   (INPUT  icInvGroup AS CHAR,
    INPUT  iiType     AS INT,
    INPUT  idtAccDate AS DATE,
    OUTPUT ocPrefix   AS CHAR).

   DEF VAR liVoucher AS INT NO-UNDO.
   
   ASSIGN icInvGroup = fSeqInvGroup(icInvGroup)
          liVoucher  = ?.
   
   FOR FIRST bSeqIGVoucher NO-LOCK WHERE
             bSeqIGVoucher.Brand     = gcBrand    AND
             bSeqIGVoucher.InvGroup  = icInvGroup AND 
             bSeqIGVoucher.PaymType   = iiType     AND 
             bSeqIGVoucher.FromDate <= idtAccDate:

      ASSIGN ocPrefix  = bSeqIGVoucher.SeqPrefix     
             liVoucher = bSeqIGVoucher.Voucher + 1.
   END.

   /* try general if dedicated was not found */
   IF liVoucher = ? THEN 
   FOR FIRST bSeqIGVoucher NO-LOCK WHERE
             bSeqIGVoucher.Brand     = gcBrand    AND
             bSeqIGVoucher.InvGroup  = icInvGroup AND 
             bSeqIGVoucher.PaymType   = 0          AND 
             bSeqIGVoucher.FromDate <= idtAccDate:

      ASSIGN ocPrefix  = bSeqIGVoucher.SeqPrefix     
             liVoucher = bSeqIGVoucher.Voucher + 1.
   END.

   /* none found */
   IF liVoucher = ? THEN RETURN "".

   REPEAT:
      /* Payment can be found immediately after Voucher is assigned, 
         even before transaction has ended */
      IF NOT CAN-FIND(FIRST Payment WHERE 
                            Payment.Brand      = gcBrand AND
                            Payment.ExtVoucher = ocPrefix + 
                                                 STRING(liVoucher,"99999999"))
      THEN LEAVE.

      ASSIGN liVoucher = liVoucher + 1.
  END.

  RETURN ocPrefix + STRING(liVoucher,"99999999").

END FUNCTION.

/* mark last used number to group */
FUNCTION fUpdateExtVoucher RETURNS LOGICAL
   (icInvGroup AS CHAR,
    iiType     AS INT,
    idtAccDate AS DATE,
    icVoucher  AS CHAR).  
   
   DEF VAR liUpdVoucher AS INT NO-UNDO.
   DEF VAR liSeqVoucher AS INT NO-UNDO.
   
   IF icVoucher = "" THEN RETURN FALSE.
   
   icInvGroup = fSeqInvGroup(icInvGroup).
   
   updinvgroup:
   DO WHILE TRUE:
   
      FIND FIRST bSeqIGVoucher EXCLUSIVE-LOCK WHERE
                 bSeqIGVoucher.Brand     = gcBrand    AND
                 bSeqIGVoucher.InvGroup  = icInvGroup AND
                 bSeqIGVoucher.PaymType  = iiType     AND
                 bSeqIGVoucher.FromDate <= idtAccDate 
                 NO-ERROR NO-WAIT.

      /* if group is locked, don't try too long; procedures that create
         payments check anyway if number to be used is already taken */
      IF LOCKED(bSeqIGVoucher) THEN DO:
         liUpdVoucher = liUpdVoucher + 1.
         IF liUpdVoucher > 3 THEN DO:
            RETURN FALSE.
         END.
         PAUSE 5 NO-MESSAGE.
         NEXT updinvgroup.
      END.

      ELSE IF NOT AVAILABLE bSeqIGVoucher THEN DO:
         /* update general if dedicated was not found */
         IF iiType > 0 THEN DO:
            iiType = 0.
            NEXT updinvgroup.
         END.
            
         RETURN FALSE.
      END.
      
      ELSE DO:

         /* remove prefix (don't use replace) */
         IF bSeqIGVoucher.SeqPrefix > "" THEN DO:
            IF icVoucher BEGINS bSeqIGVoucher.SeqPrefix THEN DO:
               IF LENGTH(icVoucher) > LENGTH(bSeqIGVoucher.SeqPrefix)
               THEN icVoucher = SUBSTRING(icVoucher,
                                          LENGTH(bSeqIGVoucher.SeqPrefix) + 1).
               ELSE icVoucher = "".
            END.
            /* don't update if prefix is not the same */
            ELSE icVoucher = "".
         END.
         
         liSeqVoucher = INTEGER(icVoucher) NO-ERROR.
         
         /* invalid integer value */
         IF ERROR-STATUS:ERROR THEN RETURN FALSE.
         
         IF liSeqVoucher > bSeqIGVoucher.Voucher THEN 
             bSeqIGVoucher.Voucher = liSeqVoucher.
               
         RELEASE bSeqIGVoucher.
      
         RETURN TRUE. 
      END. 
      
   END.

END FUNCTION.

FUNCTION fGetAndUpdExtVoucher RETURNS CHARACTER
   (icInvGroup      AS CHAR,
    iiType          AS INT,
    idtAccDate      AS DATE,
    OUTPUT ocPrefix AS CHAR).

    DEF VAR lcVoucher AS CHAR NO-UNDO.

    /* get next voucher */
    lcVoucher = fGetExtVoucher(icInvGroup,
                               iiType,
                               idtAccDate,
                               OUTPUT ocPrefix).
    
    /* update it as used */
    fUpdateExtVoucher(icInvGroup,
                      iiType,
                      idtAccDate,
                      lcVoucher).
                
    RETURN lcVoucher. 

END.

FUNCTION fGetIntVoucher RETURNS INTEGER:

   DEF VAR liIntVoucher AS INT NO-UNDO.
   
   REPEAT:
      liIntVoucher = NEXT-VALUE(PaymVouch).
      
      IF NOT CAN-FIND(FIRST Payment WHERE Payment.Voucher = liIntVoucher)
      THEN LEAVE.
   END.
   
   RETURN liIntVoucher.

END FUNCTION.
   

/******* old functions, using PaymVouch *********/

/* get the NEXT AVAILABLE voucher number */
FUNCTION fGetVoucher RETURNS INTEGER
   (icBrand       AS CHAR,
    iiVoucherType AS INT).

    DEF VAR xPaymVouch AS INT NO-UNDO.

    FIND LAST PaymVouch NO-LOCK WHERE
              PaymVouch.Brand       = icBrand AND
              PaymVouch.VoucherType = iiVoucherType
              NO-ERROR.

    /* don't update yet, retrieved VALUE is NOT necessarily used */
    ASSIGN xPaymVouch = IF AVAILABLE PaymVouch
                        THEN PaymVouch.Voucher + 1
                        ELSE 1.

    REPEAT:
        /* Payment can be found immediately after Voucher is assigned, 
           even before TRANSACTION has ended */
        IF NOT CAN-FIND(FIRST Payment WHERE Payment.Voucher = xPaymVouch)
        THEN LEAVE.

        ASSIGN xPaymVouch = xPaymVouch + 1.
    END.

    RETURN xPaymVouch. 

END.

/* UPDATE the LAST used number TO db */
FUNCTION fUpdVoucher RETURNS LOGICAL
    (icBrand       AS CHAR,
     iiVoucherType AS INT,
     iiNewValue    AS INT).

    IF NOT CAN-FIND(FIRST PaymVouch WHERE
                          PaymVouch.Brand       = icBrand       AND
                          PaymVouch.VoucherType = iiVoucherType AND
                          PaymVouch.Voucher     = iiNewValue)
    THEN DO:

       /* never update backwards */
       FIND LAST PaymVouch WHERE 
                 PaymVouch.Brand       = icBrand AND
                 PaymVouch.VoucherType = iiVoucherType NO-LOCK NO-ERROR.
       IF AVAILABLE PaymVouch AND PaymVouch.Voucher > iiNewValue
       THEN RETURN FALSE.
       
       /* try to update first, if not possible then just create a new one */
       FIND FIRST PaymVouch EXCLUSIVE-LOCK WHERE
                  PaymVouch.Brand       = icBrand  AND
                  PaymVouch.VoucherType = iiVoucherType
       NO-WAIT NO-ERROR.

       IF LOCKED(PaymVouch) OR NOT AVAILABLE(PaymVouch)
       THEN DO:
          CREATE PaymVouch.
          ASSIGN PaymVouch.Brand       = icBrand
                 PaymVouch.VoucherType = iiVoucherType.
       END.
              
       PaymVouch.Voucher = iiNewValue.

       RELEASE PaymVouch.       

       RETURN TRUE.
    END.

    ELSE RETURN FALSE. 
END.

FUNCTION fGetAndUpdVoucher RETURNS INTEGER
   (icBrand       AS CHAR,
    iiVoucherType AS INT).

    DEF VAR xPaymVouch AS INT NO-UNDO.

    /* get next voucher */
    xPaymVouch = fGetVoucher(icBrand,
                             iiVoucherType).
    
    /* update is as used */
    fUpdVoucher(icBrand,
                iiVoucherType,
                xPaymVouch).
                
    RETURN xPaymVouch. 

END.

