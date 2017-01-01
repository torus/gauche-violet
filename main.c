#include <stdio.h>
#include <stdlib.h>
#include <sys/socket.h>
#include <netinet/in.h>

#include <uv.h>

#include <gauche.h>
#include <gauche/static.h>

#define DEFAULT_PORT 2222
#define DEFAULT_BACKLOG 10

uv_loop_t *loop;

struct sockaddr_in addr;

typedef struct {
  uv_write_t req;
  uv_buf_t buf;
} write_req_t;

void free_write_req(uv_write_t *req) {
  write_req_t *wr = (write_req_t*) req;
  free(wr->buf.base);
  free(wr);
}

void echo_write(uv_write_t *req, int status) {
  if (status) {
    fprintf(stderr, "Write error %s\n", uv_strerror(status));
  }
  printf("echo_write: %p\n", req);
  free_write_req(req);
}

void error_exit(ScmObj c)
{
    ScmObj m = Scm_ConditionMessage(c);
    if (SCM_FALSEP(m)) {
        Scm_Printf(SCM_CURERR, "gosh: Thrown unknown condition: %S\n", c);
    } else {
        Scm_Printf(SCM_CURERR, "gosh: %S: %A\n", Scm_ConditionTypeName(c), m);
    }
    Scm_Exit(1);
}

ScmObj read_proc;

void echo_read(uv_stream_t *client, ssize_t nread, const uv_buf_t *buf) {
  if (nread > 0) {
    ScmEvalPacket epak;
    if (Scm_Apply(read_proc, SCM_LIST2(SCM_MAKE_INT(client),
                                       Scm_MakeString(buf->base, nread, -1, 0)), &epak) < 0) {
      error_exit(epak.exception);
    }

    return;
  }
  if (nread < 0) {
    if (nread != UV_EOF)
      fprintf(stderr, "Read error %s\n", uv_err_name(nread));
    uv_close((uv_handle_t*) client, NULL);
  }

  free(buf->base);
}

void alloc_buffer(uv_handle_t *handle, size_t suggested_size, uv_buf_t *buf) {
  buf->base = (char*) malloc(suggested_size);
  buf->len = suggested_size;
}

ScmObj new_conn_proc;

void on_new_connection(uv_stream_t *server, int status) {
  if (status < 0) {
    fprintf(stderr, "New connection error %s\n", uv_strerror(status));
    // error!
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

  printf("%p\n", client);

  ScmEvalPacket epak;
  if (Scm_Apply(new_conn_proc, SCM_NIL, &epak) < 0) {
    error_exit(epak.exception);
  }
}

ScmObj dequeue_response_proc;

void handle_response(uv_idle_t* handle) {
  ScmEvalPacket epak;
  if (Scm_Apply(dequeue_response_proc, SCM_NIL, &epak) < 0) {
    error_exit(epak.exception);
  }
  ScmObj result = epak.results[0];
  if (SCM_PAIRP(result)) {
    uv_stream_t *client = (uv_stream_t*)SCM_INT_VALUE(SCM_CAR(result));
    ScmObj cdr = SCM_CDR(result);
    if (SCM_STRINGP(cdr)) {
      const ScmStringBody* body = SCM_STRING_BODY(cdr);
      write_req_t *req = (write_req_t*) malloc(sizeof(write_req_t));
      char *string = (char*)malloc(SCM_STRING_BODY_SIZE(body));
      memcpy(string, SCM_STRING_BODY_START(body), SCM_STRING_BODY_SIZE(body));
      req->buf = uv_buf_init(string, SCM_STRING_BODY_SIZE(body));
      printf("handle_response: %p\n", req);
      uv_write((uv_write_t*) req, client, &req->buf, 1, echo_write);
    }
  }
}

int main() {
  Scm_Init(GAUCHE_SIGNATURE);

  ScmLoadPacket lpak;
  if (Scm_Load("./script.scm", 0, &lpak) < 0) {
    error_exit(lpak.exception);
  }

  ScmObj hello_proc = Scm_GlobalVariableRef(Scm_CurrentModule(),
                                            SCM_SYMBOL(SCM_INTERN("hello")),
                                            SCM_BINDING_STAY_IN_MODULE);
  ScmEvalPacket epak;
  if (Scm_Apply(hello_proc, SCM_NIL, &epak) < 0) {
    error_exit(epak.exception);
  }

  new_conn_proc = Scm_GlobalVariableRef(Scm_CurrentModule(),
                                        SCM_SYMBOL(SCM_INTERN("on-new-connection")),
                                        SCM_BINDING_STAY_IN_MODULE);

  read_proc = Scm_GlobalVariableRef(Scm_CurrentModule(),
                                    SCM_SYMBOL(SCM_INTERN("on-read")),
                                    SCM_BINDING_STAY_IN_MODULE);

  dequeue_response_proc = Scm_GlobalVariableRef(Scm_CurrentModule(),
                                                SCM_SYMBOL(SCM_INTERN("dequeue-response!")),
                                                SCM_BINDING_STAY_IN_MODULE);

  loop = uv_default_loop();

  uv_idle_t idler;

  uv_idle_init(uv_default_loop(), &idler);
  uv_idle_start(&idler, handle_response);

  uv_tcp_t server;
  uv_tcp_init(loop, &server);

  uv_ip4_addr("0.0.0.0", DEFAULT_PORT, &addr);

  uv_tcp_bind(&server, (const struct sockaddr*)&addr, 0);
  int r = uv_listen((uv_stream_t*) &server, DEFAULT_BACKLOG, on_new_connection);
  if (r) {
    fprintf(stderr, "Listen error %s\n", uv_strerror(r));
    return 1;
  }
  return uv_run(loop, UV_RUN_DEFAULT);
}
