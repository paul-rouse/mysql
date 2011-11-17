/*
 * Wrap MySQL API calls that are known to block and to be vulnerable
 * to interruption by GHC's RTS signals.
 */

#include "mysql_signals.h"
#include <pthread.h>
#include <signal.h>
#include <stdio.h>

static sigset_t sigs[1];
static int sigs_inited;

static void init_rts_sigset(void)
{
    static pthread_mutex_t sigs_mutex = PTHREAD_MUTEX_INITIALIZER;

    pthread_mutex_lock(&sigs_mutex);
    if (!sigs_inited) {
	sigemptyset(sigs);
	sigaddset(sigs, SIGALRM);
	sigaddset(sigs, SIGVTALRM);
	sigs_inited = 1;
    }
    pthread_mutex_unlock(&sigs_mutex);
}

#define block_rts_signals() \
    do { \
        if (!sigs_inited) init_rts_sigset(); \
        pthread_sigmask(SIG_BLOCK, sigs, NULL);	\
    } while (0)

#define unblock_rts_signals() pthread_sigmask(SIG_UNBLOCK, sigs, NULL)

MYSQL *STDCALL _hs_mysql_real_connect(MYSQL *mysql, const char *host,
				      const char *user,
				      const char *passwd,
				      const char *db,
				      unsigned int port,
				      const char *unix_socket,
				      unsigned long clientflag)
{
    MYSQL *ret;
    block_rts_signals();
    ret = mysql_real_connect(mysql, host, user, passwd, db, port, unix_socket,
			     clientflag);
    unblock_rts_signals();

    return ret;
}

void STDCALL _hs_mysql_close(MYSQL *sock)
{
    block_rts_signals();
    mysql_close(sock);
    unblock_rts_signals();
}

int STDCALL _hs_mysql_ping(MYSQL *mysql)
{
    int ret;
    block_rts_signals();
    ret = mysql_ping(mysql);
    unblock_rts_signals();
    return ret;
}

int STDCALL _hs_mysql_real_query(MYSQL *mysql, const char *q,
				 unsigned long length)
{
    int ret;
    block_rts_signals();
    ret = mysql_real_query(mysql, q, length);
    unblock_rts_signals();
    return ret;
}

const char *STDCALL _hs_mysql_stat(MYSQL *mysql)
{
    const char *ret;
    block_rts_signals();
    ret = mysql_stat(mysql);
    unblock_rts_signals();
    return ret;
}

my_bool STDCALL _hs_mysql_commit(MYSQL * mysql)
{
    my_bool ret;
    block_rts_signals();
    ret = mysql_commit(mysql);
    unblock_rts_signals();
    return ret;
}

my_bool STDCALL _hs_mysql_rollback(MYSQL * mysql)
{
    my_bool ret;
    block_rts_signals();
    ret = mysql_rollback(mysql);
    unblock_rts_signals();
    return ret;
}

my_bool STDCALL _hs_mysql_autocommit(MYSQL *mysql, my_bool auto_mode)
{
    my_bool ret;
    block_rts_signals();
    ret = mysql_autocommit(mysql, auto_mode);
    unblock_rts_signals();
    return ret;
}

my_bool STDCALL _hs_mysql_change_user(MYSQL *mysql, const char *user,
				      const char *passwd, const char *db)
{
    my_bool ret;
    block_rts_signals();
    ret = mysql_change_user(mysql, user, passwd, db);
    unblock_rts_signals();
    return ret;
}

int STDCALL _hs_mysql_select_db(MYSQL *mysql, const char *db)
{
    int ret;
    block_rts_signals();
    ret = mysql_select_db(mysql, db);
    unblock_rts_signals();
    return ret;
}

MYSQL_FIELD *STDCALL _hs_mysql_fetch_field(MYSQL_RES *result)
{
    MYSQL_FIELD *ret;
    block_rts_signals();
    ret = mysql_fetch_field(result);
    unblock_rts_signals();
    return ret;
}

MYSQL_ROW STDCALL _hs_mysql_fetch_row(MYSQL_RES *result)
{
    MYSQL_ROW ret;
    block_rts_signals();
    ret = mysql_fetch_row(result);
    unblock_rts_signals();
    return ret;
}

unsigned long *STDCALL _hs_mysql_fetch_lengths(MYSQL_RES *result)
{
    unsigned long *ret;
    block_rts_signals();
    ret = mysql_fetch_lengths(result);
    unblock_rts_signals();
    return ret;
}

MYSQL_RES *STDCALL _hs_mysql_store_result(MYSQL *mysql)
{
    MYSQL_RES *ret;
    block_rts_signals();
    ret = mysql_store_result(mysql);
    unblock_rts_signals();
    return ret;
}

MYSQL_RES *STDCALL _hs_mysql_use_result(MYSQL *mysql)
{
    MYSQL_RES *ret;
    block_rts_signals();
    ret = mysql_use_result(mysql);
    unblock_rts_signals();
    return ret;
}

void STDCALL _hs_mysql_free_result(MYSQL_RES *result)
{
    block_rts_signals();
    mysql_free_result(result);
    unblock_rts_signals();
}

int STDCALL _hs_mysql_next_result(MYSQL *mysql)
{
    int ret;
    block_rts_signals();
    ret = mysql_next_result(mysql);
    unblock_rts_signals();
    return ret;
}
