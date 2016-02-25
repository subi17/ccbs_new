&IF "{&HPDEnvironment}" NE "YES"
&THEN

/* Currently values Salt and Yoigo are supported */
&GLOBAL-DEFINE HPD_ENVIRONMENT Yoigo

&ENDIF