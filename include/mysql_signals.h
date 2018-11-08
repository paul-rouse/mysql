/*
 * Wrappers for MySQL API calls that are known to block and to be
 * vulnerable to interruption by GHC's RTS signals.
 */

#ifndef _mysql_signals_h
#define _mysql_signals_h

#include "mysql.h"

#if !defined(MARIADB_BASE_VERSION) && MYSQL_VERSION_ID >= 80000
typedef char my_bool;
#endif

MYSQL *STDCALL _hs_mysql_real_connect(MYSQL *mysql, const char *host,
				      const char *user,
				      const char *passwd,
				      const char *db,
				      unsigned int port,
				      const char *unix_socket,
				      unsigned long clientflag);
void STDCALL _hs_mysql_close(MYSQL *sock);
int STDCALL _hs_mysql_ping(MYSQL *mysql);
int STDCALL _hs_mysql_real_query(MYSQL *mysql, const char *q,
				 unsigned long length);
const char *STDCALL _hs_mysql_stat(MYSQL *mysql);
my_bool STDCALL _hs_mysql_commit(MYSQL * mysql);
my_bool STDCALL _hs_mysql_rollback(MYSQL * mysql);
my_bool STDCALL _hs_mysql_autocommit(MYSQL * mysql, my_bool auto_mode);
my_bool STDCALL _hs_mysql_change_user(MYSQL *mysql, const char *user,
				      const char *passwd, const char *db);
int STDCALL _hs_mysql_select_db(MYSQL *mysql, const char *db);
MYSQL_FIELD *STDCALL _hs_mysql_fetch_field(MYSQL_RES *result);
MYSQL_ROW STDCALL _hs_mysql_fetch_row(MYSQL_RES *result);
unsigned long *STDCALL _hs_mysql_fetch_lengths(MYSQL_RES *result);
MYSQL_RES *STDCALL _hs_mysql_store_result(MYSQL *mysql);
MYSQL_RES *STDCALL _hs_mysql_use_result(MYSQL *mysql);
void STDCALL _hs_mysql_free_result(MYSQL_RES *result);
int STDCALL _hs_mysql_next_result(MYSQL *mysql);
void STDCALL _hs_mysql_close(MYSQL *sock);

#endif /* _mysql_signals_h */
