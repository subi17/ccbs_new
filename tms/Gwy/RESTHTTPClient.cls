 
 /*------------------------------------------------------------------------
    File        : RESTHTTPClient
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : tlamminmaki
    Created     : Wed May 16 16:18:54 EEST 2018
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
/*USING OpenEdge.Logging.ILogWriter.*/
USING OpenEdge.Net.HTTP.ClientBuilder.
USING OpenEdge.Net.HTTP.DefaultHttpClientBuilder.
USING OpenEdge.Net.HTTP.IHttpClient.
USING OpenEdge.Net.HTTP.IHttpClientLibrary.
USING OpenEdge.Net.HTTP.ICookieJar.
USING OpenEdge.Net.HTTP.IHttpRequest.
USING OpenEdge.Net.HTTP.IHttpResponse.
USING OpenEdge.Net.URI.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Gwy.RESTHTTPClient: 
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

   DEFINE PRIVATE VARIABLE aoClientBuilder AS CLASS DefaultHttpClientBuilder NO-UNDO.
   DEFINE PRIVATE VARIABLE aoHTTPClient    AS CLASS IHttpClient              NO-UNDO.

   CONSTRUCTOR PUBLIC RESTHTTPClient():

      aoClientBuilder = CAST(ClientBuilder:Build(), DefaultHttpClientBuilder).
   
   END CONSTRUCTOR.

   DESTRUCTOR PUBLIC RESTHTTPClient ( ):

      IF VALID-OBJECT(aoHTTPClient)
      THEN DELETE OBJECT aoHTTPClient.

      IF VALID-OBJECT(aoClientBuilder)
      THEN DELETE OBJECT aoClientBuilder.

   END DESTRUCTOR.

   /* 
      The set methods must be called before calling this method
   */
   METHOD PUBLIC VOID mDoRESTHTTPClient():

      IF VALID-OBJECT(aoHTTPClient)
      THEN DELETE OBJECT aoHTTPClient.

      aoHTTPClient = aoClientBuilder:Client.

   END METHOD.

   METHOD PUBLIC IHttpResponse mExecute
      (ioRequest AS CLASS IHttpRequest):

      RETURN aoHTTPClient:Execute(ioRequest).

   END METHOD.

   METHOD PUBLIC VOID mSetRequestTimeout
      ( ideValue AS DECIMAL ): /* A zero or positive value */
      aoClientBuilder:SetRequestTimeout(ideValue).
   END METHOD.

   METHOD PUBLIC VOID mSetRetryPause
      ( ideValue AS DECIMAL ): /* A zero or positive value */
      aoClientBuilder:SetRetryPause(ideValue).
   END METHOD.

   METHOD PUBLIC VOID mSetNumRetries
      ( iiValue AS INTEGER ): /* A zero or positive value */
      aoClientBuilder:SetNumRetries(iiValue).
   END METHOD.

   /* Sets the rest client name and version
      Default name: 'OpenEdge-HttpClient'
      Default version: 0.4.0'
   */
   METHOD PUBLIC VOID mSetNameVersion
      ( icName AS CHARACTER,
        icVersion AS CHARACTER ):
      aoClientBuilder:Named(icName, icVersion).
   END METHOD.

   /* It is probably not a good idea to call this.
      The default library should work ok (will be created using a method
      BuildLibrary in a class OpenEdge.Net.HTTP.ClientBuilder) */ 
   METHOD PUBLIC VOID mSetLibrary
      ( ioLibrary AS CLASS IHttpClientLibrary ):
      aoClientBuilder:UsingLibrary(ioLibrary).
   END METHOD.

/*
   METHOD PUBLIC VOID mSetLogger
      ( ioLogger AS CLASS ILogWriter ):
      aoClientBuilder:UsingLogger(ioLogger).
   END METHOD.
*/

   METHOD PUBLIC VOID mSetProxy
      ( ioProxyURI AS CLASS URI ):
      aoClientBuilder:ViaProxy(ioProxyURI).
   END METHOD.

   METHOD PUBLIC VOID mSetProxy
      ( icProxyURI AS CHARACTER ):
      aoClientBuilder:ViaProxy(icProxyURI).
   END METHOD.

   METHOD PUBLIC VOID mSetCookie
      ( ioCookieJar AS CLASS ICookieJar ):
      aoClientBuilder:KeepCookies(ioCookieJar).
   END METHOD.

   METHOD PUBLIC VOID mSetCookie():
      aoClientBuilder:KeepCookies().
   END METHOD.

END CLASS.