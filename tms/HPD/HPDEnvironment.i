&IF "{&HPDEnvironment}" NE "YES"
&THEN

/* Currently values Salt and Yoigo are supported */
&GLOBAL-DEFINE HPD_ENVIRONMENT Yoigo

/* The following line dictates whether of not old header
   version is used. 

   Without the line the new header version is used
   (it is already used in Salt and DNA) */
&GLOBAL-DEFINE OLD_HEADER_VERSION

/* The following line dictates that date, datetime and datetime-tz will be converted
   to character format using the STRING function.
   
   Without the line the ISO-DATE function is used. */  
&GLOBAL-DEFINE DO_NOT_USE_ISODATE_FORMAT

&ENDIF