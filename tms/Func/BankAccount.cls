 
 /*------------------------------------------------------------------------
    File        : BankAccount
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : tlamminmaki
    Created     : Wed May 16 08:57:01 EEST 2018
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Func.BankAccount FINAL: 
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
		
	CONSTRUCTOR PRIVATE BankAccount (  ):
	END CONSTRUCTOR.

   METHOD PRIVATE STATIC LOGICAL mValidControlES
      (icBankAcc AS CHARACTER,
       icControl AS CHARACTER,
       icWeight  AS CHARACTER):

      DEFINE VARIABLE lii   AS INTEGER   NO-UNDO.
      DEFINE VARIABLE lcc   AS CHARACTER NO-UNDO.
      DEFINE VARIABLE liSum AS INTEGER   NO-UNDO.

      DO lii = 1 TO LENGTH(icBankAcc):
         lcc = SUBSTRING(icBankAcc,lii,1).
         /* Must be a number */
         IF INDEX("0123456789",lcc) EQ 0
         THEN RETURN FALSE.
         
         liSum = liSum + INTEGER(lcc) * INTEGER(ENTRY(lii,icWeight)).
      END.

      IF liSum MOD 11 < 2 AND icControl NE STRING(liSum MOD 11)
      THEN RETURN FALSE.
      ELSE IF liSum MOD 11 >= 2 AND icControl NE STRING(11 - (liSum MOD 11))
      THEN RETURN FALSE.

      RETURN TRUE.

   END METHOD.

   METHOD PRIVATE STATIC LOGICAL mValidES
      ( icBankAcc AS CHARACTER ):

      IF LENGTH(icBankAcc) NE 24
      THEN RETURN FALSE.    

            /* first 8 numbers from position 5 have their own
               control digit (13. digit) */
      RETURN mValidControlES(SUBSTRING(icBankAcc,5,8),
                             SUBSTRING(icBankAcc,13,1),
                             "4,8,5,10,9,7,3,6") AND
            /* last 10 numbers have their own control digit (14. digit) */
             mValidControlES(SUBSTRING(icBankAcc,15,10),
                             SUBSTRING(icBankAcc,14,1),
                             "1,2,4,8,5,10,9,7,3,6").
   END METHOD.

   METHOD PUBLIC STATIC LOGICAL mValid
      ( icBankAcc AS CHARACTER ):

      /* IBAN Check */
      IF NOT Func.IBAN:mValid(icBankAcc)
      THEN RETURN FALSE.

      CASE SUBSTRING(icBankAcc,1,2):
         WHEN "ES"
         THEN RETURN mValidES(icBankAcc).
         OTHERWISE UNDO, THROW NEW Progress.Lang.AppError
                           (SUBSTITUTE("Unsupported bank account number '&1'",
                                       icBankAcc)).
      END CASE.

   END METHOD.

END CLASS.