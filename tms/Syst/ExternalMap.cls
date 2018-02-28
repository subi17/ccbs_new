ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS Syst.ExternalMap:

   DEF PRIVATE STATIC VAR instance AS CLASS Syst.ExternalMap NO-UNDO.
   
   METHOD PUBLIC STATIC CHAR get_external_id (iValue AS CHAR,
                                              iType  AS CHAR,
                                              icSrc  AS CHAR):
   /*This returns the ExternalID for given InternalID and Type*/
      RETURN instance:_get_external_id(iValue, iType, icSrc).
   END METHOD.
   
   METHOD PUBLIC STATIC CHAR get_internal_id (iValue AS CHAR,
                                              iType  AS CHAR,
                                              icSrc  AS CHAR):
      RETURN instance:_get_internal_id(iValue, iType, icSrc).
   END METHOD.

   /*This returns the ExternalID for given InternalID and Type*/
   METHOD PUBLIC CHAR _get_external_id (iValue AS CHAR, 
                                        iType  AS CHAR,
                                        icSrc  AS CHAR):

      /* get the first one if source is not given */
      IF icSrc = "" THEN 
      FIND FIRST ExternalID USE-INDEX Type_InternalID_ExternalID WHERE
                 ExternalID.InternalID = iValue AND
                 ExternalID.Type = iType NO-LOCK NO-ERROR.
      ELSE 
      FIND FIRST ExternalID USE-INDEX Type_InternalID_ExternalID WHERE
                 ExternalID.InternalID = iValue AND
                 ExternalID.Type = iType AND
                 ExternalID.ExtSource = icSrc NO-LOCK NO-ERROR.
      IF AVAILABLE ExternalID THEN
         RETURN ExternalID.ExternalID.
      ELSE 
         RETURN "".
         
   END METHOD.

   /*This returns the InternalID for given ExternalID and Type*/
   METHOD PUBLIC CHAR _get_internal_id (iValue AS CHAR, 
                                          iType  AS CHAR,
                                          icSrc  AS CHAR):

      /* get the first one if source is not given */
      IF icSrc = "" THEN 
      FIND FIRST ExternalID USE-INDEX Type_ExternalID_InternalID WHERE
                 ExternalID.ExternalID = iValue AND
                 ExternalID.Type = iType NO-LOCK NO-ERROR.
      ELSE 
      FIND FIRST ExternalID USE-INDEX Type_ExternalID_InternalID WHERE
                 ExternalID.ExternalID = iValue AND
                 ExternalID.Type = iType AND
                 ExternalID.ExtSource = icSrc NO-LOCK NO-ERROR.
      IF AVAILABLE ExternalID THEN
         RETURN ExternalID.InternalID.
      ELSE
         RETURN "".

   END METHOD.

   CONSTRUCTOR STATIC ExternalMap ():
      instance = NEW Syst.ExternalMap().
   END CONSTRUCTOR.

END CLASS.
