
/*------------------------------------------------------------------------
    File        : fcreatedir.i
    Purpose     : Use Progress OS-CREATE-DIR command to create
                  multiple directories.

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Fri Oct 03 13:17:16 EEST 2014
    Notes       :
  ----------------------------------------------------------------------*/

&IF "{&fcreatedir_i}" NE "YES"
&THEN

&GLOBAL-DEFINE fcreatedir_i YES

/* Do not call this directly, instead use fCreateDir function! */
FUNCTION fCreateDirRecursion RETURNS INTEGER
    ( iiPosition    AS INTEGER,
      icDirToCreate AS CHARACTER,
      icDelim       AS CHARACTER ):
    
   IF iiPosition = 0
   THEN RETURN 0.

   OS-CREATE-DIR VALUE(SUBSTRING(icDirToCreate,1,iiPosition)).

   IF OS-ERROR <> 0
   THEN RETURN OS-ERROR.

   RETURN fCreateDirRecursion(INDEX(icDirToCreate,icDelim,iiPosition + 1),
                              icDirToCreate,
                              icDelim).

END FUNCTION.

FUNCTION fCreateDir RETURNS INTEGER
    ( icDirToCreate AS CHARACTER ):

   DEFINE VARIABLE lcDelim   AS CHARACTER   NO-UNDO.

   OS-CREATE-DIR VALUE(icDirToCreate).

   IF OS-ERROR = 2 /* "No such file or directory". If we got this message it
                      tells that we should generate multiple directories */
   THEN DO:
      IF OPSYS = "UNIX"
      THEN lcDelim = "/".
      ELSE lcDelim = CHR(92).
            
      RETURN fCreateDirRecursion(INDEX(icDirToCreate,lcDelim),
                                 icDirToCreate + (IF SUBSTRING(icDirToCreate,LENGTH(icDirToCreate)) = lcDelim THEN "" ELSE lcDelim),
                                 lcDelim).
   END.

   ELSE RETURN OS-ERROR.

END FUNCTION.

&ENDIF