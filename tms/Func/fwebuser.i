/**
 * Account management for web-login
 * 
 * The table web.UserAccount relates Customers (CustNum) to a login-name,
 * a password and a status (which is currently not used).
 *
 * This file is maintained by the WuiTeam
 * Author: Fabian Kreutz 01/2006
 
   Changed:  30.01.06/aam length for password 5 
 */


FUNCTION make_password RETURN CHAR
      ( INPUT piLength AS INT ):
    DEF VAR result AS CHAR NO-UNDO.
    DEF VAR lcnew AS CHAR NO-UNDO.
    DEF VAR lclast AS CHAR NO-UNDO.

    lclast = ''.
    DO WHILE LENGTH(result) < piLength:
        IF LENGTH(result) MOD 2 = 1 THEN
            lcnew = ENTRY(RANDOM(1,6), "a,e,i,o,u,y").
        ELSE
            lcnew = SUBSTR("aehiklnprstu", RANDOM(1,12), 1).
        IF lcnew NE lclast THEN DO:
            result = result + lcnew.
            lclast = lcnew.
        END.
    END.
    RETURN result.
END FUNCTION.

/* Returns FALSE if pcLogin has been changed to a valid value */
FUNCTION check_login_name RETURN LOGICAL
      ( INPUT piCustNum AS INT,
        INPUT-OUTPUT pcLogin AS CHAR ):
    DEF VAR llinvalid AS LOGICAL INITIAL FALSE NO-UNDO.
    DEF VAR lii AS INT NO-UNDO.
    DEF VAR ljj AS INT NO-UNDO.

    IF pcLogin = ? or pcLogin = "" THEN
        llinvalid = TRUE.
    ELSE DO:
        INT(pcLogin) NO-ERROR.
        IF NOT ERROR-STATUS:ERROR AND                    /* A Number        */        
           pcLogin NE STRING(piCustNum) AND            /* Not the CustNum */
           SUBSTR(pcLogin, 1, 1) NE "0" THEN        /* Not starting with 0 */
            llinvalid = TRUE.
    END.
    IF NOT llinvalid THEN DO lii = 1 TO LENGTH(pcLogin):
        ljj = ASC(SUBSTR(pcLogin, lii, 1)).
        /* 48 - 57   digits
         * 58 - 64   : ; < = > ? @
         * 65 - 90   a - z
         * 91 - 96   [ \ ] ^ _ `
         * 97 - 122  A - Z */
        IF ljj < 48 OR ( ljj > 122 AND
                LOOKUP(STRING(ljj), "132,134,142,143,148,153") = 0 ) THEN DO:
            llinvalid = TRUE.
            LEAVE.
        END.
    END.
    IF NOT llinvalid AND
       CAN-FIND(UserAccount
                WHERE UserAccount.login EQ pcLogin
                  AND UserAccount.CustNum NE piCustNum) THEN
        llinvalid = TRUE.
    IF llinvalid THEN
        pcLogin = STRING(piCustNum).
    RETURN NOT llinvalid.
END FUNCTION.
        


/** Returns loginname and password for a given CustNum in output variables.
 * Return value: true if account found and variables set.
 */
FUNCTION get_account_data RETURN LOGICAL
      ( INPUT piCustNum AS INT,
        OUTPUT pcLogin AS CHAR,
        OUTPUT pcPassword AS CHAR ):
    FIND UserAccount NO-LOCK NO-PREFETCH
    WHERE UserAccount.CustNum = piCustNum NO-ERROR.
    IF AVAILABLE UserAccount THEN ASSIGN
        pcLogin = UserAccount.login
        pcPassword = UserAccount.password
    .
    RETURN AVAILABLE UserAccount.
END FUNCTION.


/** Create a new account for given CustNum.
 * Loginname is equal to CustNum if parameter value is ? or empty.
 * If account already exists, returns false and does not update!
 * Might return ?, e.g. on duplicate login name
 */
FUNCTION create_account RETURN LOGICAL
      ( INPUT piCustNum AS INT,
        INPUT pcLogin AS CHAR,
        INPUT pcPassword AS CHAR ):

    IF CAN-FIND( UserAccount WHERE UserAccount.CustNum = piCustNum ) THEN
        RETURN FALSE.
    check_login_name(piCustNum, pcLogin).
    IF pcPassword = ? OR pcPassword = "" THEN
        pcPassword = make_password(5).
    CREATE UserAccount.
    ASSIGN
        UserAccount.CustNum = piCustNum
        UserAccount.Login = pcLogin
        UserAccount.Password = pcPassword
        UserAccount.active = 1
    .
    RETURN TRUE.
END FUNCTION.


/** Removes the account entry for given custnum.
 * Returns TRUE on successful deletion, FALSE if account not found.
 */
FUNCTION remove_account RETURN LOGICAL
    ( INPUT piCustNum AS INT ):
    FIND UserAccount EXCLUSIVE-LOCK
    WHERE UserAccount.CustNum = piCustNum NO-ERROR.
    IF AVAILABLE UserAccount THEN DO:
        DELETE UserAccount.
        RETURN TRUE.
    END.
    RETURN FALSE.
END FUNCTION.


/** Update the loginname and password for given custnum.
 * If loginname and/or password or ? or empty, the loginname is not
 * changed and the password is generated.
 * Returns TRUE on success.
 * Might return ?, e.g. on duplicate login name
 */
FUNCTION update_account RETURN LOGICAL
      ( INPUT piCustNum AS INT,
        INPUT pcLogin AS CHAR,
        INPUT pcPassword AS CHAR ):
    FIND UserAccount EXCLUSIVE-LOCK
    WHERE UserAccount.CustNum = piCustNum NO-ERROR.
    IF AVAILABLE UserAccount THEN DO:
        IF check_login_name(piCustNum, pcLogin) THEN
            UserAccount.Login = pcLogin.
        IF pcPassword = ? OR pcPassword = "" THEN
            pcPassword = make_password(5).
        UserAccount.Password = pcPassword.
        RELEASE UserAccount.
        RETURN TRUE.
    END.
    RETURN FALSE.
END FUNCTION.

