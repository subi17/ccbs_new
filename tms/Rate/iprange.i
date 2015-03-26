&IF "{&IPRANGE_I}" NE "YES"
&THEN

&GLOBAL-DEFINE IPRANGE_I YES
/* ----------------------------------------------------------------------
  MODULE .......: iprange.i
  TASK .........: IP range functions.
  APPLICATION ..: TMS
  AUTHOR .......: anttis
  CREATED ......: 24.05.12
  Version ......: xfera
----------------------------------------------------------------------- */
FUNCTION fIpToLong RETURNS DEC
   (pcIP AS CHAR):
   
   DEF VAR i AS INT NO-UNDO. 
   DEF VAR liResult AS INT64.
   DEF VAR liOctet AS INT NO-UNDO. 

   DO i = 1 TO 4:
      liOctet = INT(ENTRY(i,pcIP,".")) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN -1.
      liResult = liResult + liOctet * exp(256, 4 - i).
   END.

   RETURN DEC(liResult).
END.

FUNCTION fLongToIP RETURNS CHAR
   (pdeIP AS DEC):
   
   DEF VAR i AS INT NO-UNDO. 
   DEF VAR lcResult AS CHAR.

   DEF VAR liOctet1 AS INT NO-UNDO. 
   DEF VAR liOctet2 AS INT NO-UNDO. 
   DEF VAR liOctet3 AS INT NO-UNDO. 
   DEF VAR liTemp AS INT NO-UNDO. 
   DEF VAR liOctet4 AS INT NO-UNDO. 
   
   ASSIGN
      lioctet1 = int(trunc(pdeIP / exp(256,3),0))
      lioctet2 = int(trunc((pdeIP MOD exp(256,3)) / exp(256,2),0))
      lioctet3 = int(trunc(((pdeIP MOD exp(256,3)) MOD exp(256,2)) / 256,0))
      lioctet4 = int(((pdeIP MOD exp(256,3)) MOD exp(256,2)) MOD 256). 

   lcResult = STRING(lioctet1) + "." +
              STRING(lioctet2) + "." +
              STRING(lioctet3) + "." +
              STRING(lioctet4).

   RETURN lcResult.
END.

FUNCTION fGetLastIP RETURNS DEC
   (piIP AS DEC, piMask AS INT):
  RETURN DEC(piIP + EXP(2, (32 - piMask)) - 1).
END.

FUNCTION fValidateIPRangeOverlap RETURNS LOGICAL
   (pdeFirstIP AS DEC, pdeLastIP AS DEC):

   IF CAN-FIND(FIRST IPRange NO-LOCK WHERE
                     IPRange.FirstIP > pdeFirstIP AND
                     IPRange.LastIP < pdeLastIP AND
                     IPRange.FirstIP NE pdeFirstIP) THEN RETURN FALSE.

   IF CAN-FIND(FIRST IPRange NO-LOCK WHERE
                     IPRange.FirstIP <= pdeLastIP AND
                     IPRange.LastIP >= pdeLastIP AND
                     IPRange.FirstIP NE pdeFirstIP) THEN RETURN FALSE.
   
   IF CAN-FIND(FIRST IPRange NO-LOCK WHERE
                     IPRange.FirstIP <= pdeFirstIP AND
                     IPRange.LastIP >= pdeFirstIP AND
                     IPRange.FirstIP NE pdeFirstIP) THEN RETURN FALSE.

   RETURN TRUE.
END.

FUNCTION fValidateIPAddressSyntax RETURNS LOG
   (INPUT pcIP AS CHAR):

   DEF VAR i AS INT NO-UNDO. 
   DEF VAR liEntries AS INT NO-UNDO. 
   DEF VAR liOctet AS INT NO-UNDO. 

   liEntries = NUM-ENTRIES(pcIP,".").
   
   IF liEntries NE 4 THEN RETURN FALSE.
   
   DO i = 1 TO liEntries:

      liOctet = INT(ENTRY(i,pcIP,".")) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN FALSE.
   
      IF liOctet < 0 OR liOctet > 255 THEN RETURN FALSE.
   END.

   RETURN TRUE.
END.

FUNCTION fValidateIPRange RETURNS LOGICAL
   (pcIP AS CHAR,
    piNetMask AS INT,
    plNew AS LOG,
    OUTPUT odeFirstIP AS DEC,
    OUTPUT odeLastIP AS DEC,
    OUTPUT ocError AS CHAR):
   
   IF fValidateIPAddressSyntax(pcIP) EQ FALSE THEN DO:
      ocError = "ERROR:Incorrect IP Address".
      RETURN FALSE.
   END.
   
   IF plNew  AND 
      CAN-FIND(FIRST IPRange NO-LOCK WHERE
                     IPRange.NetworkAddress = pcIP) THEN DO:
      ocError = "ERROR: IP address already exists".
      RETURN FALSE.
   END.

   IF (piNetMask < 1 OR piNetMask > 32) THEN DO:
      ocError = "ERROR:Incorrect Netmask".
      RETURN FALSE.
   END.
   
   odeFirstIP = fIpToLong(pcIP).
   odeLastIP = fGetLastIP(odeFirstIP, piNetMask).
/*
   IF NOT fValidateIPRangeOverlap(odeFirstIP, odeLastIP) THEN DO:
      ocError = "ERROR:Overlapping IP range".
      RETURN FAlse.
   END.
*/
   RETURN True.
END.

&ENDIF
