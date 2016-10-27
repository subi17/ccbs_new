
/* MobSub.i1  : names FOR MobSub status codes 

   changed:     26.04.05/aam status 30-38 added
                28.06.06/aam 40-48 added 

*/

DEF VAR stnames      AS C                      NO-UNDO.

stnames = "UNDEFINED,"                      /* 0  */  +
          "WAITING FIRST CALL,"        /* 1  */  +
          "ACTIVATION ONGOING,"             /* 2  */  +
          "ACTIVATION FAILURE,"             /* 3  */  +
          "ACTIVE,"                  /* 4  */  +
          ","                               /* 5  */  +
          ","                               /* 6  */  +
          "BARRED DUE TO SALDO,"            /* 7  */  +
          "SUSPENDED OR BARRED,"             /* 8  */  +
          "BARRED DUE TO CONTROL,"          /* 9  */  +
          ","                               /* 10 */  +
          "MNP - NOT NPO,"                  /* 11 */  +
          "MNP - NPO,"                      /* 12 */  +
          "MNP - NPOC,"                     /* 13 */  +
          "MNP - REJECTED,"                 /* 14 */  +
          "SUBCRIPTION TERMINATED,"         /* 15 */  +
          "PENDING MOBILE ACTIVATION,,,,,,,,,,,,,," /* 16 */  +       
          "CLOSED DUE TO DEBT,"             /* 30 */  +
          "CLOSED BY CREDIT CONTROL,"       /* 31 */  +
          ",,,,"                                      +
          "CLOSED DUE TO CRIME,"             /* 36 */  +
          "BARRED DUE TO SALDO AND DEBT,"    /* 37 */  +
          "CLOSED BY CUSTOMER AND DEBT,"     /* 38 */  +
          ","                                          +
          "SERVICE BARR. DUE TO DEBT,"       /* 40 */  +
          "SERVICE BARR. BY CC AND DEBT,"    /* 41 */  +
          "SERVICE BARR. BY CUST. + DEBT,"   /* 42 */  +
          ",,,,"                                       + 
          "BARRED DUE TO SALDO + SERV.DEBT," /* 47 */  +
          "CLOSED BY CUSTOMER + SERV.DEBT"   /* 48 */.
