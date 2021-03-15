// -*- c-basic-offset: 4 -*-
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <assert.h>
#include <sys/socket.h>
#include <netinet/in.h>

#include <uv.h>

#include <gauche.h>
#include <gauche/static.h>

#define DEFAULT_PORT 2222
#define DEFAULT_BACKLOG 10

uv_loop_t *loop;
uv_timer_t timeout;

ScmObj result_proc = SCM_UNDEFINED;

void error_exit(ScmObj c)
{
    ScmObj m = Scm_ConditionMessage(c);
    if (SCM_FALSEP(m)) {
        Scm_Printf(SCM_CURERR, "gosh: Thrown unknown condition: %S\n", c);
    } else {
        Scm_ReportError(c, SCM_TRUE);
    }
    Scm_Exit(1);
}

struct sockaddr_in addr;

typedef struct {
    uv_write_t req;
    uv_buf_t buf;
    uv_stream_t *client;
} write_req_t;

void free_write_req(uv_write_t *req) {
    write_req_t *wr = (write_req_t*) req;
    free(wr->buf.base);
    free(wr);
}

ScmObj write_done_proc = SCM_UNDEFINED;

void echo_write(uv_write_t *req, int status) {
    if (status) {
        Scm_Printf(SCM_CURERR, "Write error %s\n", uv_strerror(status));
    }

    write_req_t *wr = (write_req_t*)req;
    uv_stream_t *client = wr->client;
    free_write_req(req);

    ScmEvalPacket epak;
    if (Scm_Apply(write_done_proc, SCM_LIST1(SCM_MAKE_INT(client)), &epak) < 0) {
        error_exit(epak.exception);
    }
}

ScmObj read_proc = SCM_UNDEFINED;

void echo_read(uv_stream_t *client, ssize_t nread, const uv_buf_t *buf) {
    if (nread > 0) {
        ScmEvalPacket epak;
        if (Scm_Apply(read_proc, SCM_LIST2(
                          SCM_MAKE_INT(client),
                          Scm_MakeString(buf->base, nread, -1, 0)),
                      &epak) < 0) {
            error_exit(epak.exception);
        }

        return;
    }
    if (nread < 0) {
        if (nread != UV_EOF)
            Scm_Printf(SCM_CURERR, "Read error %s\n", uv_err_name(nread));
    }

    free(buf->base);
}

void alloc_buffer(uv_handle_t *handle, size_t suggested_size, uv_buf_t *buf) {
    buf->base = (char*) malloc(suggested_size);
    buf->len = suggested_size;
}

ScmObj new_conn_proc = SCM_UNDEFINED;

void on_new_connection(uv_stream_t *server, int status) {
    if (status < 0) {
        Scm_Printf(SCM_CURERR, "New connection error %s\n", uv_strerror(status));
        return;
    }

    uv_tcp_t *client = (uv_tcp_t*) malloc(sizeof(uv_tcp_t));
    uv_tcp_init(loop, client);
    if (uv_accept(server, (uv_stream_t*) client) == 0) {
        uv_read_start((uv_stream_t*) client, alloc_buffer, echo_read);
    }
    else {
        uv_close((uv_handle_t*) client, NULL);
    }

    ScmEvalPacket epak;
    if (Scm_Apply(new_conn_proc, SCM_NIL, &epak) < 0) {
        error_exit(epak.exception);
    }
}

ScmObj dequeue_response_proc = SCM_UNDEFINED;

void handle_response(uv_idle_t* handle) {
    while (1) {
        ScmEvalPacket epak;
        if (Scm_Apply(dequeue_response_proc, SCM_NIL, &epak) < 0) {
            error_exit(epak.exception);
        }
        ScmObj result = epak.results[0];
        if (SCM_PAIRP(result)) {
            // (id 'res client "response")
            // (id 'close client)
            long id = SCM_INT_VALUE(SCM_CAR(result));
            const char *tag =
                SCM_STRING_BODY_START(
                    SCM_STRING_BODY(SCM_SYMBOL_NAME(SCM_CADR(result))));
            ScmObj body = SCM_CDDR(result);

            if (!strcmp("res", tag)) {
                uv_stream_t *client = (uv_stream_t*)SCM_INT_VALUE(SCM_CAR(body));
                const ScmStringBody* content = SCM_STRING_BODY(SCM_CADR(body));

                write_req_t *req = (write_req_t*) malloc(sizeof(write_req_t));
                int size = SCM_STRING_BODY_SIZE(content);
                char *string = (char*)malloc(size);
                memcpy(string, SCM_STRING_BODY_START(content), size);
                req->buf = uv_buf_init(string, size);
                req->client = client;
                uv_write((uv_write_t*) req, client, &req->buf, 1, echo_write);
            } else if (!strcmp("close", tag)) {
                uv_stream_t *client = (uv_stream_t*)SCM_INT_VALUE(SCM_CAR(body));
                /* Scm_Printf(SCM_CURERR, "close: %S\n", body); */
                uv_close((uv_handle_t*)client, NULL);
            } else {
                Scm_Printf(SCM_CURERR, "handle_response: unknown tag %s\n", tag);
                abort();
            }
        } else {
            usleep(1000);
            return;
        }
    }
}

int main(int argc, char **argv) {
    loop = uv_default_loop();

    // Gauche
    Scm_Init(GAUCHE_SIGNATURE);

    if (argc < 1) {
        Scm_Printf(SCM_CURERR, "usage: %s infile\n", argv[0]);
        Scm_Exit(1);
    }

    ScmLoadPacket lpak;
    Scm_AddLoadPath(LIBDIR, 0);
    if (Scm_Load("violet", 0, &lpak) < 0) {
        error_exit(lpak.exception);
    }
    ScmModule *violet_mod = SCM_FIND_MODULE("violet", 0);

    Scm_AddLoadPath(".", 0);
    const char *infile = argv[1];
    if (Scm_Load(infile, 0, &lpak) < 0) {
        error_exit(lpak.exception);
    }


    ScmObj init_proc = SCM_UNDEFINED;
    ScmModule *mod = Scm_CurrentModule();

    SCM_BIND_PROC(init_proc,             "violet-init",              violet_mod);
    SCM_BIND_PROC(new_conn_proc,         "violet-on-new-connection", violet_mod);
    SCM_BIND_PROC(read_proc,             "violet-on-read",           violet_mod);
    SCM_BIND_PROC(write_done_proc,       "violet-on-write-done",     violet_mod);
    SCM_BIND_PROC(dequeue_response_proc, "violet-dequeue-response!", violet_mod);

    ScmEvalPacket epak;
    if (Scm_Apply(init_proc, SCM_NIL, &epak) < 0) {
        error_exit(epak.exception);
    }

    uv_timer_init(loop, &timeout);

    // Main loop
    uv_idle_t idler;

    uv_idle_init(uv_default_loop(), &idler);
    uv_idle_start(&idler, handle_response);

    uv_tcp_t server;
    uv_tcp_init(loop, &server);

    uv_ip4_addr("0.0.0.0", DEFAULT_PORT, &addr);

    uv_tcp_bind(&server, (const struct sockaddr*)&addr, 0);
    int r = uv_listen((uv_stream_t*) &server, DEFAULT_BACKLOG, on_new_connection);
    if (r) {
        Scm_Printf(SCM_CURERR, "Listen error %s\n", uv_strerror(r));
        return 1;
    }
    Scm_Printf(SCM_CURERR, "%s: starting server on port %d\n",
               __FUNCTION__, DEFAULT_PORT);
    return uv_run(loop, UV_RUN_DEFAULT);
}
