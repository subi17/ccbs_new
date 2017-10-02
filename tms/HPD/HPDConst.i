/*
   The HPD library first check if a file tms/HPD/HPDConst.i
   is available. If it is not then this file will be copied to
   there.

   NOTE!!! If you modify the tms/HPD/HPDConst.i, you need to re-install
           the HPD library for the changes took effect!!!
*/

&IF "{&HPDConst}" NE "YES"
&THEN

&GLOBAL-DEFINE HPDConst YES

&GLOBAL-DEFINE HPDKeyDelimiter CHR(255)
&GLOBAL-DEFINE HPDDumpDelimiter '|'

/* Should ISO-DATE function be used when
   convertind DATE variable to string format.
   If not then STRING function is used. */
&GLOBAL-DEFINE USE_ISODATE_DATE_FORMAT NO

&ENDIF