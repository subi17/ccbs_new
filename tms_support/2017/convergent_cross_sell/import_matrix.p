DEF VAR lcSourceFolder AS CHAR NO-UNDO.
lcSourceFolder = "../tms_support/2017/convergent_cross_sell/".

DEF TEMP-TABLE ttMatrix like matrix.
DEF TEMP-TABLE ttMXItem like mxitem.

input from value(lcSourceFolder + "matrix.d").
repeat:
   create ttMatrix.
   import ttMatrix.
end.

input close.
input from value(lcSourceFolder + "mxitem.d").
repeat:
   create ttMXItem.
   import ttMXItem.
end.

FUNCTION fGetNextMXSeq RETURNS INTEGER ():

   DEFINE BUFFER Matrix FOR Matrix.

   FOR EACH Matrix NO-LOCK BY Matrix.MXSeq DESCENDING:
     RETURN Matrix.MXSeq + 1.
   END.

   RETURN 1.

END FUNCTION.

create_matrix:
DO TRANS:

   FOR EACH ttMatrix:

      if ttMatrix.MxSeq eq 0 then next.

      create matrix.
      matrix.mxseq = fGetNextMXSeq().
      buffer-copy ttMatrix except mxseq to matrix no-error.

      if error-status:error then do:
         MESSAGE "here 1" VIEW-AS ALERT-BOX.
         undo create_matrix, leave create_matrix.
      end.

      FOR EACH ttMxitem:

         if ttMxitem.mxseq eq 0 then next.

         create Mxitem.
         Mxitem.MxSeq = matrix.mxseq.
         buffer-copy ttMXitem except mxseq to mxitem no-error.

         if error-status:error then do:
            MESSAGE "here 2" VIEW-AS ALERT-BOX.
            undo create_matrix, leave create_matrix.
         end.

      end.

   end.
end.
