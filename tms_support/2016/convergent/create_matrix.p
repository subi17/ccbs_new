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

liSeq = create_matrix("PERCONTR", "Convergent 2GB mobile").
create_mxitem("PerContract","CONTS2GB",liSeq).
create_mxitem("PerContract","DATA6",liSeq).
create_mxitem("SubsTypeTo","CONTDSL45",liSeq).
create_mxitem("SubsTypeTo","CONTFH45_50",liSeq).
create_mxitem("SubsTypeTo","CONTFH55_300",liSeq).

liSeq = create_matrix("PERCONTR", "Convergent 10GB mobile").
create_mxitem("PerContract","CONTS10GB",liSeq).
create_mxitem("PerContract","DATA6",liSeq).
create_mxitem("SubsTypeTo","CONTDSL55",liSeq).
create_mxitem("SubsTypeTo","CONTFH55_50",liSeq).
create_mxitem("SubsTypeTo","CONTFH65_300",liSeq).

liSeq = create_matrix("PERCONTR", "Convergent CONTDSL45").
create_mxitem("PerContract","CONTDSL45",liSeq).
create_mxitem("SubsTypeTo","CONTDSL45",liSeq).

liSeq = create_matrix("PERCONTR", "Convergent CONTDSL55").
create_mxitem("PerContract","CONTDSL55",liSeq).
create_mxitem("SubsTypeTo","CONTDSL55",liSeq).

liSeq = create_matrix("PERCONTR", "Convergent CONTFH45_50").
create_mxitem("PerContract","CONTFH45_50",liSeq).
create_mxitem("SubsTypeTo","CONTFH45_50",liSeq).

liSeq = create_matrix("PERCONTR", "Convergent CONTFH55_50").
create_mxitem("PerContract","CONTFH55_50",liSeq).
create_mxitem("SubsTypeTo","CONTFH55_50",liSeq).

liSeq = create_matrix("PERCONTR", "Convergent CONTFH55_300").
create_mxitem("PerContract","CONTFH55_300",liSeq).
create_mxitem("SubsTypeTo","CONTFH55_300",liSeq).

liSeq = create_matrix("PERCONTR", "Convergent CONTFH65_300").
create_mxitem("PerContract","CONTFH65_300",liSeq).
create_mxitem("SubsTypeTo","CONTFH65_300",liSeq).

liSeq = create_matrix("PERCONTR", "Convergent permanency").
create_mxitem("PerContract","FTERM*",liSeq).
create_mxitem("SubsTypeFrom","CONTDSL*",liSeq).
create_mxitem("SubsTypeFrom","CONTFH*",liSeq).
