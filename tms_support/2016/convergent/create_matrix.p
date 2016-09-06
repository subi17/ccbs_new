DEF VAR liSeq AS INT.

FUNCTION create_matrix returns int(
   input ickey AS CHAR,
   input icName AS CHAR):

   DEF VAR liPriority AS INT.
   FIND LAST Matrix WHERE
             Matrix.mxkey EQ "PERCONTR".

   liPriority = Matrix.prior + 1.
   
   CREATE Matrix.
   ASSIGN
      Matrix.Brand  = "1"
      Matrix.MXSeq  = NEXT-VALUE(imsi)
      Matrix.mxkey  = icKey
      Matrix.mxname = icName
      Matrix.prior  = liPriority
      Matrix.mxres  = 1.
   RETURN Matrix.MXSeq.
END.

FUNCTION create_mxitem returns int (input icname AS CHAR,
                                    input icValue AS CHAR,
                                    input iiseq AS INT):
   CREATE MXItem.
   ASSIGN
      MXItem.mxseq = iiseq
      MXItem.mxvalue = icValue
      MXItem.mxname = icname.
END.

/* Main */

liSeq = create_matrix("PERCONTR", "Per.contract usage").
create_mxitem("PerContract","CONTS2GB",liSeq).
create_mxitem("PerContract","DATA6",liSeq).
create_mxitem("SubsTypeTo","CONTDSL10",liSeq).
create_mxitem("SubsTypeTo","CONTFH10",liSeq).

liSeq = create_matrix("PERCONTR", "Per.contract usage").
create_mxitem("PerContract","CONTS5GB",liSeq).
create_mxitem("PerContract","DATA6",liSeq).
create_mxitem("SubsTypeTo","CONTDSL15",liSeq).
create_mxitem("SubsTypeTo","CONTFH15",liSeq).

