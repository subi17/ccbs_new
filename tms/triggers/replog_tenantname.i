&IF "{&replog_tenantname_i}" NE "YES"
&THEN

&GLOBAL-DEFINE replog_tenantname_i YES

FUNCTION fRepLogTenantName RETURNS CHARACTER
   (ihBuffer AS HANDLE):

   CASE ihBuffer:BUFFER-TENANT-NAME:
      WHEN "" OR WHEN "Default"
      THEN RETURN "".
      OTHERWISE RETURN ihBuffer:BUFFER-TENANT-NAME.
   END. 

END FUNCTION.

&ENDIF 