DEF VAR ldaFrom AS DATE INIT 09/07/16.
DEF VAR laskuri AS INT.

FUNCTION create_group returns log (INPUT lcCode as CHAR, INPUT lcName AS CHAR):
   IF CAN-FIND(FIRST ServiceLimitgroup WHERE 
                     Servicelimitgroup.brand = "1" AND
                     servicelimitgroup.groupcode EQ lcCode) THEN
      return false.
   CREATE servicelimitgroup.
   ASSIGN servicelimitgroup.brand = "1"
          servicelimitgroup.groupcode = lcCode
          servicelimitgroup.groupname = lcName
          servicelimitgroup.validfrom = ldafrom
          servicelimitgroup.validto = 12/31/49.
   return true.
END.

FUNCTION create_limit returns log (INPUT lcCode as CHAR, 
                                   INPUT lcName AS CHAR,
                                   INPUT lclimit AS CHAR,
                                   INPUT ldAmt AS DEC,
                                   INPUT liDialType AS INT,
                                   INPUT liUnit AS INT):
   
   IF CAN-FIND(FIRST ServiceLimit WHERE servicelimit.groupcode EQ lccode AND
                                  servicelimit.slname EQ lcName) THEN
      RETURN false.
   find last servicelimit  use-index slseq no-lock no-error.

           if not avail servicelimit then laskuri = 1.
           else laskuri = servicelimit.slseq + 1.   

   CREATE servicelimit.
   ASSIGN servicelimit.dialtype = lidialtype
          servicelimit.inclunit = liUnit
          servicelimit.groupcode = lcCode
          servicelimit.slcode = lcCode + lcLimit
          servicelimit.slname = lcName
          servicelimit.validfrom = ldafrom
          servicelimit.validto = 12/31/49
          servicelimit.slseq = laskuri
          servicelimit.inclamt = ldAmt
          servicelimit.firstmonthcalc = 0
          servicelimit.lastmonthcalc = 0
          servicelimit.webdisp = 1.
   return true.
END.                                   

/* old ones, can be removed
create_group("CONTDSL45", "Contrato DSL45").
create_group("CONTDSL55", "Contrato DSL55").
create_group("CONTFH45_50", "Contrato FH45_50").
create_group("CONTFH55_50", "Contrato FH55_50").
create_group("CONTFH55_300", "Contrato FH55_300").
create_group("CONTFH65_300", "Contrato FH65_300").

create_limit("CONTDSL45", "BDest", "_QTY",300.0).
create_limit("CONTDSL55", "BDest", "_QTY",300.0).
create_limit("CONTFH45_50", "BDest", "_QTY",300.0).
create_limit("CONTFH55_50", "BDest", "_QTY",300.0).
create_limit("CONTFH55_300", "BDest", "_QTY",300.0).
create_limit("CONTFH65_300", "BDest", "_QTY",300.0).

create_limit("CONTDSL45", "National calls", "_MIN",5000.0).
create_limit("CONTDSL55", "National calls", "_MIN",5000.0).
create_limit("CONTFH45_50", "National calls", "_MIN",5000.0).
create_limit("CONTFH55_50", "National calls", "_MIN",5000.0).
create_limit("CONTFH55_300", "National calls", "_MIN",5000.0).
create_limit("CONTFH65_300", "National calls", "_MIN",5000.0).
*/

create_group("CONTDSL", "Contrato DSL").
create_group("CONTFH50", "Contrato FH50").
create_group("CONTFH300", "Contrato FH300").

create_limit("CONTDSL", "BDest", "_QTY",120.0, 0, 7).
create_limit("CONTFH50", "BDest", "_QTY",120.0, 0, 7).
create_limit("CONTFH300", "BDest", "_QTY",120.0, 0, 7).

create_limit("CONTDSL", "National calls", "_MIN",60.0, 1, 1).
create_limit("CONTFH50", "National calls", "_MIN",60.0, 1, 1).
create_limit("CONTFH300", "National calls", "_MIN",60.0, 1, 1).



