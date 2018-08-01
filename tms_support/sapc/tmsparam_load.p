Syst.Parameters:setc("Scheme", "SAPC", "http").

/* UAT-A */
Syst.Parameters:setc("Host", "SAPC", "localhost").
Syst.Parameters:seti("Port", "SAPC", 212).
Syst.Parameters:seti("MockNOK", "SAPC", 0).

/*
/*Production*/
Syst.Parameters:setc("Host", "SAPC", "").
Syst.Parameters:seti("Port", "SAPC", 212).
Syst.Parameters:seti("MockNOK", "SAPC", 0).
*/