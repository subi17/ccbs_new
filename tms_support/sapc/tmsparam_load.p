/* UAT-A with mock server */
Syst.Parameters:setc("Scheme", "SAPC", "http").
Syst.Parameters:setc("Host", "SAPC", "localhost").
Syst.Parameters:seti("Port", "SAPC", 3004).
Syst.Parameters:seti("MockNOK", "SAPC", 0).
Syst.Parameters:setc("URIPrefix", "SAPC", "/api/web").

/*
/* UAT-A with actual server */
Syst.Parameters:setc("Scheme", "SAPC", "http").
Syst.Parameters:setc("Host", "SAPC", "95.169.251.106").
Syst.Parameters:seti("Port", "SAPC", 96).
Syst.Parameters:seti("MockNOK", "SAPC", 0).
Syst.Parameters:setc("URIPrefix", "SAPC", "").
*/

/*
/*Production*/
Syst.Parameters:setc("Scheme", "SAPC", "http").
Syst.Parameters:setc("Host", "SAPC", "").
Syst.Parameters:seti("Port", "SAPC", 212).
Syst.Parameters:seti("MockNOK", "SAPC", 0).
Syst.Parameters:setc("URIPrefix", "SAPC", "").
*/