&IF "{&HPDEnvironment}" NE "YES"
&THEN

/* Currently values Salt and Yoigo are supported */
&GLOBAL-DEFINE HPD_ENVIRONMENT Yoigo

/* If we are going to switch same header version as
   in Salt and DNA just remove or comment following line */
&GLOBAL-DEFINE OLD_HEADER_VERSION

&ENDIF