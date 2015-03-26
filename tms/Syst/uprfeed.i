/* uprfeed.i   06.11.96 /tt 
               22.10.01/aam use ff

*/

IF oso = "" THEN PUT STREAM tul control(chr(012)).
ELSE IF {1} NE spit1 THEN 
    PUT STREAM tul skip(spit1 - {1}).


