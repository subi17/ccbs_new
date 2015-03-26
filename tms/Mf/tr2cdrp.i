/* -----------------------------------------------
  MODULE .......: TR2CDRP.I
  FUNCTION .....: CREATE CGR ranges FOR preprosessor configuration file
  APPLICATION ..: NN
  AUTHOR .......: KL
  CREATED ......: 23.12.98
  MODIFIED .....: 03.04.2000 BY kl: two loops FOR cgr ranges
  VERSION ......: M15
  ------------------------------------------------------ */

DEF VAR iTmp AS i NO-UNDO.

DEF TEMP-TABLE cgr NO-UNDO
   FIELD ROWID     AS i
   FIELD cgr-from  AS c
   FIELD cgr-to    AS c
   FIELD name      AS c
   FIELD oper      AS c
   FIELD type      AS c
   FIELD categ     AS c

   INDEX cgr AS primary
      cgr-from
   INDEX ROWID
      ROWID.

DEF BUFFER xcgr FOR cgr.

FUNCTION fName RETURNS CHAR
   (INPUT name AS CHAR).

   DEF VAR x AS i NO-UNDO.

   /* remove spaces FOR names */
   x = index(name," ").
   DO WHILE x > 0:
      substr(name,x,1) = "".
      x = index(name," ").
   END.
   name = substr(name,1,6).

   RETURN name.

END.

FUNCTION fTr2Cdrp RETURNS INT
   (INPUT ident AS CHAR).

   DEF VAR xtype AS c NO-UNDO.
   DEF VAR xcate AS c NO-UNDO.
   DEF VAR ROW   AS i NO-UNDO.
   DEF VAR x     AS i NO-UNDO.
   DEF VAR line  AS c NO-UNDO.
   DEF VAR prev  AS c NO-UNDO.

   /* FIRST: look FOR special CGR types */
   FOR EACH MedTrunk NO-LOCK where
            MedTrunk.Ident = ident.

      CREATE cgr.
      ASSIGN
         cgr.rowid    = 0
         cgr.cgr-from = string(MedTrunk.TrFrom)
         cgr.cgr-to   = string(MedTrunk.TrTo)
         cgr.oper     = MedTrunk.OpCode
         cgr.type     = MedTrunk.Type
         cgr.categ    = MedTrunk.Categ.

      /* name FOR the CGR range */
      FIND FIRST Operator where
                 Operator.Operator = MedTrunk.OpCode
      NO-LOCK NO-ERROR.
      IF AVAIL Operator THEN cgr.name = Operator.OperName.
      ELSE                 cgr.name = Trunk.TrunkName.

   END.

   /* SECOND: normal CGR types */
   FOR EACH Trunk NO-LOCK,
      /* LOCAL switches */
      FIRST Exchange  NO-LOCK WHERE 
            Exchange.ExCode  = Trunk.ExCode AND
            Exchange.Local   = TRUE            AND
            Exchange.Ident = ident        /*   AND 
      /* SPECIAL RANGES: these are printed already */
      NOT can-find(FIRST MedTrunk where
                         MedTrunk.ExCode = Exchange.ExCode     AND
                         Trunk.TrIn = string(MedTrunk.TrFrom)) */
   BY Trunk.TrIn.

      ASSIGN
         ROW   = ROW + 1
         xtype = ""
         xcate = "".

      /* seek FOR special CGR types */
      FIND FIRST MedTrunk where
                 MedTrunk.ExCode = Exchange.ExCode AND 
                 Trunk.TrunkCode = MedTrunk.TrFrom
      NO-LOCK NO-ERROR.
      /* existing special CGR types */
      IF AVAIL MedTrunk THEN ASSIGN
         xtype = MedTrunk.Type
         xcate = MedTrunk.Categ.
      /* default CGR types */
      ELSE DO:
         FIND FIRST MedDefTrunk where
                    MedDefTrunk.ExCode   = Exchange.ExCode AND
             string(MedDefTrunk.DefFrom) = Trunk.TrunkCode AND
             string(MedDefTrunk.DefTo)   = Trunk.TrunkCode
         NO-LOCK NO-ERROR.
         IF AVAIL MedDefTrunk THEN ASSIGN
            xtype = MedDefTrunk.Type
            xcate = MedDefTrunk.Categ.

         /* IF CGR connection is DIRECT */
         IF Trunk.TrIn THEN ASSIGN
            xtype = "D"
            xcate = "D".

      END.

      CREATE cgr.
      ASSIGN
         cgr.rowid    = ROW
         cgr.cgr-from = Trunk.TrunkCode
         cgr.cgr-to   = Trunk.TrunkCode
         cgr.oper     = Trunk.OpCode
         cgr.name     = Trunk.TrunkName
         cgr.type     = xtype
         cgr.categ    = xcate.

      /* IF CGR is PREFIX analyse */
      IF Trunk.TrInternat THEN DO:
         CREATE xcgr.
         buffer-copy cgr TO xcgr.
         ASSIGN 
            xcgr.type  = "P"
            xcgr.categ = "P".
      END.

      /* CGR leading TO another Tele1 switch */
      if cgr.type = "S" THEN cgr.name = Trunk.TrunkName.
      /* others */
      ELSE DO:
         FIND FIRST Operator where
                    Operator.Operator = Trunk.OpCode
         NO-LOCK NO-ERROR.
         IF AVAIL Operator THEN cgr.name = Operator.OperName.
         else cgr.name =  "???".
      END.
   END.

   /* CREATE normal CGR ranges into TEMP-TABLE */
   FOR EACH cgr where cgr.rowid NE 0. 

      FIND FIRST xcgr where
                 xcgr.rowid = cgr.rowid.

      ASSIGN 
         cgr.cgr-to = string(xcgr.cgr-from)
         prev       = string(xcgr.cgr-from).

      /* another Tele1 switch */
      if cgr.type = "S" THEN DO:

         /* remove individual rows inside the range */
         FOR EACH xcgr where
                  xcgr.cgr-from >  cgr.cgr-from AND
                  xcgr.cgr-to   <= cgr.cgr-to   AND
                  xcgr.name      = cgr.name.
            DELETE xcgr.
         END.

      END.
      /* others */
      ELSE DO:

         /* remove individual rows inside the range */
         FOR EACH xcgr where
                  xcgr.cgr-from >  cgr.cgr-from AND
                  xcgr.cgr-to   <= cgr.cgr-to.
            DELETE xcgr.
         END.

      END.

   END.

   FOR EACH cgr where
            cgr.categ NE "P".

      /* remove spaces FOR names */
      cgr.name = fName(cgr.name).

      IF cgr.cgr-from = cgr.cgr-to THEN
         line = string(cgr.cgr-from).
      ELSE
         line = string(cgr.cgr-from) + "-" +
                string(cgr.cgr-to).

      line = fName(line).

      if cgr.categ = "" then cgr.categ = "O".
      if cgr.type  = "" then cgr.type  = "O".

      PUT STREAM excel UNFORMATTED
         cgr.categ tab
         /*
         cgr.name  tab
         */
         line      tab
         cgr.rowid tab
         cgr.oper  tab
         cgr.type  nlx.

      DELETE cgr.

   END.

   /* PREFIX cgr's left ? */
   FIND FIRST cgr NO-ERROR.

   IF AVAIL cgr THEN DO:

      PUT STREAM excel UNFORMATTED
         nlx
         "PREFIXNAME"
         nlx.

      FOR EACH cgr:

         line = fName(string(cgr.cgr-from)).

         iTmp = index(cgr.name,"X").
         DO WHILE iTmp > 0:
            substr(cgr.name,iTMp,1) = "".
            iTmp = index(cgr.name,"X").
         END.

         PUT STREAM excel UNFORMATTED
            cgr.name  tab
            line      nlx.

         DELETE cgr.

      END.

   END.

END.

