&IF "{&HPDEnvironment}" NE "YES"
&THEN

/* Currently values Salt and Yoigo are supported */
&GLOBAL-DEFINE HPD_ENVIRONMENT Yoigo

/* The following line dictates that date will be converted
   to character format using the STRING function.
   
   Without the line the ISO-DATE function is used. */  
&GLOBAL-DEFINE DO_NOT_USE_ISODATE_DATE_FORMAT

&ENDIF