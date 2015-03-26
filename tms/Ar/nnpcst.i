/* nnpcst.i      04.02.05/aam

   callers: nncimu.p
            nnpcst.p
            del_inv.p
*/
            

DEF TEMP-TABLE wMarked NO-UNDO
    FIELD Line AS INT
    FIELD Amt  AS DEC
    FIELD SubInv AS INT
    INDEX Line IS UNIQUE Line. 

