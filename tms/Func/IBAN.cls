 
 /*------------------------------------------------------------------------
    File        : IBAN
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : tlamminmaki
    Created     : Wed May 16 08:08:57 EEST 2018
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Func.IBAN FINAL: 
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

   DEFINE PRIVATE STATIC VARIABLE acUpperAlphabetics AS CHARACTER CASE-SENSITIVE
      INITIAL "A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z" NO-UNDO.

   DEFINE PRIVATE STATIC VARIABLE acLowerAlphabetics AS CHARACTER CASE-SENSITIVE
      INITIAL "a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z" NO-UNDO.

   DEFINE PRIVATE STATIC VARIABLE acNumbers AS CHARACTER
      INITIAL "0,1,2,3,4,5,6,7,8,9" NO-UNDO.

	CONSTRUCTOR PRIVATE IBAN (  ):
	END CONSTRUCTOR.

   /*
      Returns IBAN check digits. Uses recursion which allows the use of a very
      big numbers. 
      
      NOTE: icNumericIBAN must contain only numbers.
   */
   METHOD PRIVATE STATIC INTEGER mCheckDigits
      ( icNumericIBAN AS CHARACTER ):

     IF LENGTH(icNumericIBAN) < 10
     THEN RETURN INTEGER(icNumericIBAN) MOD 97.
   
     RETURN mCheckDigits(STRING(INTEGER(SUBSTRING(icNumericIBAN,1,7)) MOD 97) +
                         SUBSTRING(icNumericIBAN,8)).
   
   END METHOD.

   /*
      Converts (lower and upper case) letters to numbers using a mapping
      of the following table:
               
      A 10    B 11    C 12    D 13    E 14    F 15    G 16
      H 17    I 18    J 19    K 20    L 21    M 22    N 23
      O 24    P 25    Q 26    R 27    S 28    T 29    U 30
      V 31    W 32    X 33    Y 34    Z 35
           
               Uses the ASC function: ( ASC("<letter>") - 55 )
       Notes:  
               e.g. "FI"   => "1518"
               e.g. "fi"   => "1518"
               e.g. "FI00" => "151800"
               e.g. "123"  => "123"
   ------------------------------------------------------------------------------*/

   METHOD PRIVATE STATIC CHARACTER mChar2Numchar
      ( icCharacters AS CHARACTER ):
   
       IF LENGTH(icCharacters) = 0
       THEN RETURN "".
       
       IF LOOKUP(SUBSTRING(icCharacters,1,1),acNumbers) <> 0
       THEN RETURN SUBSTRING(icCharacters,1,1) +
                   mChar2Numchar(SUBSTRING(icCharacters,2)).
       ELSE IF LOOKUP(CAPS(SUBSTRING(icCharacters,1,1)),acUpperAlphabetics) <> 0 
       THEN RETURN STRING(ASC(CAPS(SUBSTRING(icCharacters,1,1))) - 55) +
                   mChar2Numchar(SUBSTRING(icCharacters,2)).
       ELSE UNDO, THROW NEW Progress.Lang.AppError
                     (SUBSTITUTE("Cannot convert character '&1' to a number",
                                 SUBSTRING(icCharacters,1,1))).
   
   END METHOD.

   /*
      Checks international bank account number (IBAN) syntax
      Returns TRUE if IBAN is correct, otherwise returns FALSE.
   */
   METHOD PUBLIC STATIC LOGICAL mValid
      ( icIBAN AS CHARACTER ):
      
      DEFINE VARIABLE liLength AS INTEGER    NO-UNDO.
      DEFINE VARIABLE lii      AS INTEGER    NO-UNDO.

      IF icIBAN = ?
      THEN RETURN FALSE.

      liLength = LENGTH(icIBAN).

      /* Maximum IBAN length is 34.
         Minumum IBAN length is 4. */
      IF liLength > 34 OR liLength < 4
      THEN RETURN FALSE.

      /* IBAN format is 2!a2!n30c */
      DO lii = 1 TO liLength:

         IF lii <= 2 AND
             LOOKUP(SUBSTRING(icIBAN,lii,1),acUpperAlphabetics) = 0
         THEN RETURN FALSE.

         IF lii > 2 AND lii < 5 AND
            LOOKUP(SUBSTRING(icIBAN,lii,1),acNumbers) = 0
         THEN RETURN FALSE.

         IF lii > 4 AND
            LOOKUP(SUBSTRING(icIBAN,lii,1),
                   acLowerAlphabetics + "," +
                   acUpperAlphabetics + "," +
                   acNumbers) = 0
         THEN RETURN FALSE.

      END.

      /*
         This stage does all of the following steps:
            1. Moves the first four characters to the right-hand end of the IBAN.
            2. Converts upper and lower case letters to digits
            3. Applies the check character system MOD 97-10
               => If the remainder is 1 (one) the number is valid, otherwise
                  number is invalid.
      */
      IF mCheckDigits(mChar2Numchar(SUBSTRING(icIBAN,5)) +
                      mChar2Numchar(SUBSTRING(icIBAN,1,4))) <> 1
      THEN RETURN FALSE.

      RETURN TRUE.

   END METHOD.

END CLASS.