/* fconinfo.i       18.03.04/aam 
   contact info for sheets

   changes:         17.03.06/aam fLocalContactInfo
*/

DEF VAR lcBTName     AS CHAR                         NO-UNDO. 
DEF VAR lcBTAddr     AS CHAR                         NO-UNDO.
DEF VAR lcBTPost     AS CHAR                         NO-UNDO. 
DEF VAR lcBTPhone1   AS CHAR                         NO-UNDO. 
DEF VAR lcBTPhone2   AS CHAR                         NO-UNDO. 
DEF VAR lcBTPhone3   AS CHAR                         NO-UNDO. 
DEF VAR lcBTCompID   AS CHAR                         NO-UNDO. 
DEF VAR lcBTHomeLoc  AS CHAR                         NO-UNDO. 
DEF VAR lcBank       AS CHAR                         NO-UNDO EXTENT 3.
DEF VAR lcBTHeader   AS CHAR                         NO-UNDO EXTENT 3.
DEF VAR lcBTCCAddr   AS CHAR                         NO-UNDO EXTENT 3. 
DEF VAR lcBTSender   AS CHAR                         NO-UNDO. 
DEF VAR lcHSender    AS CHAR                         NO-UNDO. 


FIND FIRST Company NO-LOCK WHERE 
           Company.Brand = gcBrand NO-ERROR.
IF AVAILABLE Company THEN ASSIGN
   lcBTName    = Company.CompName
   lcBTAddr    = Company.Address
   lcBTPost    = Company.PostOffice
   lcBTPhone1  = Company.Phone
   lcBTPhone2  = Company.Phone2
   lcBTPhone3  = Company.Phone3
   lcBTCompID  = Company.CompanyID
   lcBTHomeLoc = Company.HomeLocation

   lcBTCCAddr[1] = Company.Address2
   lcBTCCAddr[2] = Company.Address3
   lcBTCCAddr[3] = Company.Address4.

FIND TMSUser WHERE TMSUser.UserCode = katun NO-LOCK NO-ERROR.
IF AVAILABLE TMSUser THEN 
ASSIGN lcBTSender = TMSUser.EMail
       lcHSender  = TMSUser.UserName.


FUNCTION fContactInfo RETURNS LOGICAL:

   PUT STREAM ekirje UNFORMATTED
       "4J"
       SPACE(5)
       STRING(lcBTHeader[1],"X(65)")
       STRING(lcBTHeader[3],"X(40)")
       MY-NL
       
       " J"
       SPACE(5) 
       STRING(lcBTName,"X(65)")
       STRING(lcBTPhone1,"X(40)")
       MY-NL
       
       " J"
       SPACE(5)
       STRING(lcBTAddr,"X(65)")
       STRING(RIGHT-TRIM(lcBTCCAddr[1]) + 
              (IF lcBTCCAddr[2] > ""
               THEN ", " + lcBTCCAddr[2]
               ELSE ""),"x(40)")
       MY-NL
       
       " J"
       SPACE(5)
       STRING(lcBTHomeLoc,"x(65)")
       STRING(lcBTPhone2,"X(40)")
       MY-NL
   
       " J"
       SPACE(5)
       STRING(lcBTCompID,"X(65)")
       STRING(lcBTPhone3,"X(34)")
       MY-NL.

END FUNCTION.

&IF "{&LocalContactInfo}" = "YES" 
&THEN
FUNCTION fLocalContactInfo RETURNS LOGICAL
   (icCurrentFont AS CHAR,
    ilEffects     AS LOG).

   IF ilEffects THEN DO: 
      /* small font on */
      IF lcSFontOn > "" THEN PUT STREAM tul CONTROL lcSFontOn.
   
      /* 12 rows per inch */
      PUT STREAM tul CONTROL CHR(027) + "&l12D".
   END.
   
   PUT STREAM tul UNFORMATTED
      SPACE(5)
      STRING(lcBTHeader[1],"X(55)")
      SKIP
       
      SPACE(5) 
      STRING(lcBTName,"X(55)")
      SKIP
       
      SPACE(5)
      STRING(lcBTAddr,"X(55)")
      STRING(RIGHT-TRIM(lcBTCCAddr[1]) + 
             (IF lcBTCCAddr[2] > ""
              THEN ", " + lcBTCCAddr[2]
              ELSE ""),"x(20)")
      SKIP
       
      SPACE(5)
      STRING(lcBTHomeLoc,"x(55)")
      SKIP
   
      SPACE(5)
      STRING(lcBTCompID,"X(55)")
      SKIP(1)
      
      SPACE(5)
      TRIM(lcBTHeader[2])
      SKIP

      SPACE(5)
      TRIM(lcBTHeader[3])
      SKIP
      
      SPACE(5)
      STRING(lcBTPhone3,"X(20)")
      SKIP.

   IF ilEffects THEN DO: 
      /* current font back on */
      IF icCurrentFont > "" THEN PUT STREAM tul CONTROL icCurrentFont.

      /* 6 rows per inch */   
      PUT STREAM tul CONTROL CHR(027) + "&l6D".
   END.
   
END FUNCTION.

FUNCTION fGetLocalContactInfo RETURNS CHAR:

   RETURN
      lcBTHeader[1] + CHR(9) +
      lcBTName + CHR(9) +
      lcBTAddr + CHR(9) +
      lcBTHomeLoc + CHR(9) +
      lcBTCompID + CHR(9).

END FUNCTION.

&ENDIF


