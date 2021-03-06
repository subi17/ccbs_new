&IF "{&MYSQL4GL_MYSQL_DEFS_I}" NE "YES" &THEN
&GLOBAL-DEFINE MYSQL4GL_MYSQL_DEFS_I YES

&GLOBAL-DEFINE LIBMYSQLCLIENT "/usr/sfw/lib/sparcv9/libmysqlclient_r.so"
&GLOBAL-DEFINE MYSQL_DEFAULT_SOCKET "/var/run/mysqld/mysqld.sock"

&GLOBAL-DEFINE MYSQL_PTR_TYPE INT64

&GLOBAL-DEFINE MYSQL_OPT_CONNECT_TIMEOUT		0
&GLOBAL-DEFINE MYSQL_OPT_COMPRESS			1
&GLOBAL-DEFINE MYSQL_OPT_NAMED_PIPE			2
&GLOBAL-DEFINE MYSQL_INIT_COMMAND			3
&GLOBAL-DEFINE MYSQL_READ_DEFAULT_FILE			4
&GLOBAL-DEFINE MYSQL_READ_DEFAULT_GROUP			5
&GLOBAL-DEFINE MYSQL_SET_CHARSET_DIR			6
&GLOBAL-DEFINE MYSQL_SET_CHARSET_NAME			7
&GLOBAL-DEFINE MYSQL_OPT_LOCAL_INFILE			8
&GLOBAL-DEFINE MYSQL_OPT_PROTOCOL			9
&GLOBAL-DEFINE MYSQL_SHARED_MEMORY_BASE_NAME		10
&GLOBAL-DEFINE MYSQL_OPT_READ_TIMEOUT			11
&GLOBAL-DEFINE MYSQL_OPT_WRITE_TIMEOUT			12
&GLOBAL-DEFINE MYSQL_OPT_USE_RESULT			13
&GLOBAL-DEFINE MYSQL_OPT_USE_REMOTE_CONNECTION		14
&GLOBAL-DEFINE MYSQL_OPT_USE_EMBEDDED_CONNECTION	15
&GLOBAL-DEFINE MYSQL_OPT_GUESS_CONNECTION		16
&GLOBAL-DEFINE MYSQL_SET_CLIENT_IP			17
&GLOBAL-DEFINE MYSQL_SECURE_AUTH			18
&GLOBAL-DEFINE MYSQL_REPORT_DATA_TRUNCATION		19
&GLOBAL-DEFINE MYSQL_OPT_RECONNECT			20
&GLOBAL-DEFINE MYSQL_OPT_SSL_VERIFY_SERVER_CERT		21

/* new more secure passwords */
&GLOBAL-DEFINE MYSQL_CLIENT_LONG_PASSWORD	1
/* Found instead of affected rows */
&GLOBAL-DEFINE MYSQL_CLIENT_FOUND_ROWS		2
/* Get all column flags */
&GLOBAL-DEFINE MYSQL_CLIENT_LONG_FLAG		4
/* One can specify db on connect */
&GLOBAL-DEFINE MYSQL_CLIENT_CONNECT_WITH_DB	8
/* Don't allow database.table.column */
&GLOBAL-DEFINE MYSQL_CLIENT_NO_SCHEMA		16
/* Can use compression protocol */
&GLOBAL-DEFINE MYSQL_CLIENT_COMPRESS		32
/* Odbc client */
&GLOBAL-DEFINE MYSQL_CLIENT_ODBC		64
/* Can use LOAD DATA LOCAL */
&GLOBAL-DEFINE MYSQL_CLIENT_LOCAL_FILES		128
/* Ignore spaces before '(' */
&GLOBAL-DEFINE MYSQL_CLIENT_IGNORE_SPACE	256
/* New 4.1 protocol */
&GLOBAL-DEFINE MYSQL_CLIENT_PROTOCOL_41		512
/* This is an interactive client */
&GLOBAL-DEFINE MYSQL_CLIENT_INTERACTIVE		1024
/* Switch to SSL after handshake */
&GLOBAL-DEFINE MYSQL_CLIENT_SSL			2048
   /* IGNORE sigpipes */
&GLOBAL-DEFINE MYSQL_CLIENT_IGNORE_SIGPIPE	4096
/* Client knows about transactions */
&GLOBAL-DEFINE MYSQL_CLIENT_TRANSACTIONS	8192
/* Old flag for 4.1 protocol  */
&GLOBAL-DEFINE MYSQL_CLIENT_RESERVED		16384
/* New 4.1 authentication */
&GLOBAL-DEFINE MYSQL_CLIENT_SECURE_CONNECTION	32768
/* Enable/disable multi-stmt support */
&GLOBAL-DEFINE MYSQL_CLIENT_MULTI_STATEMENTS	65536
/* Enable/disable multi-results */
&GLOBAL-DEFINE MYSQL_CLIENT_MULTI_RESULTS	131072


&ENDIF /* MYSQL4GL_MYSQL_DEFS_I */
