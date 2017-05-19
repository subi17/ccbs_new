DEF BUFFER bCustCat FOR CustCat.

FUNCTION fcreateCustcat RETURNS CHAR (
   INPUT icbasecat AS CHAR,
   INPUT icnr AS CHAR,
   INPUT icname AS CHAR,
   INPUT iiSubLimit AS INT,
   INPUT iiActLimit AS INT).
   
   IF CAN-FIND (FIRST custcat WHERE
              custcat.brand EQ "1" AND
              custcat.category EQ icnr) THEN RETURN "Already exist".
   FIND FIRST custcat WHERE
              custcat.brand EQ "1" AND
              custcat.category EQ icbasecat NO-ERROR.
   IF NOT AVAIL CustCat THEN RETURN "Base not exist".
   CREATE bCustCat.
   BUFFER-COPY CustCat EXCEPT custcat.category TO bCustCat.   
   ASSIGN bCustCat.category = icnr
          bCustCat.catname = icname
          bCustCat.mobsublimit = iiSubLimit
          bCustCat.activationlimit = iiActLimit.
   DISP bcustcat.
   RELEASE bCustCat.
END FUNCTION.

fcreateCustCat("20", "21", "PRO SOHO Company CIF", 25, 35).
fcreateCustCat("20", "22", "Big Companies CIF", 25, 35).
fcreateCustCat("40", "42", "PRO Self-employee NIF", 5, 7).
fcreateCustCat("41", "43", "PRO Self-employee NIE", 5, 7).

FIND FIRST CustCat WHERE 
           custcat.brand EQ "1" AND
           custcat.category EQ "20".
   ASSIGN Custcat.catname = "SOHO Company CIF".
FIND FIRST CustCat WHERE
           custcat.brand EQ "1" AND
           custcat.category EQ "40".
   ASSIGN Custcat.catname = "Self Employee NIF"
          Custcat.mobsublimit = 5
          CustCat.activationlimit = 7.
FIND FIRST CustCat WHERE
           custcat.brand EQ "1" AND
           custcat.category EQ "41".
   ASSIGN Custcat.catname = "Self Employee NIE"
          Custcat.mobsublimit = 5
          CustCat.activationlimit = 7.
   

 
