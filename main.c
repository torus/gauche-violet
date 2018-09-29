// -*- c-basic-offset: 4 -*-
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <sys/socket.h>
#include <netinet/in.h>

#include <uv.h>
#include <curl/curl.h>

#include <gauche.h>
#include <gauche/static.h>

#define DEFAULT_PORT 2222
#define DEFAULT_BACKLOG 10

uv_loop_t *loop;
CURLM *curl_handle;
uv_timer_t timeout;

typedef struct curl_context_s {
    uv_poll_t poll_handle;
    curl_socket_t sockfd;
} curl_context_t;

ScmObj result_proc = SCM_UNDEFINED;

void error_exit(ScmObj c)
{
    ScmObj m = Scm_ConditionMessage(c);
    if (SCM_FALSEP(m)) {
        Scm_Printf(SCM_CURERR, "gosh: Thrown unknown condition: %S\n", c);
    } else {
        Scm_ReportError(c);
    }
    Scm_Exit(1);
}

size_t download_callback(char *ptr, size_t size, size_t nmemb, void *userdata) {
  int id = (int)userdata;
  printf("download_callback: %p, %ld, %ld, %p\n", ptr, size, nmemb, userdata);
  char *buf = (char*)malloc(size * nmemb + 1);
  strncpy(buf, ptr, size * nmemb);
  buf[size * nmemb] = '\0';
  /* printf("%s\n", buf); */

  ScmEvalPacket epak;
  if (Scm_Apply(result_proc, SCM_LIST2(SCM_MAKE_INT(id),
                                       Scm_MakeString(buf, size * nmemb, -1, SCM_STRING_COPYING)),
                &epak) < 0) {
    error_exit(epak.exception);
  }

  free(buf);
  return CURLE_OK;
}

curl_context_t *create_curl_context(curl_socket_t sockfd) {
    curl_context_t *context;

    context = (curl_context_t*) malloc(sizeof *context);

    context->sockfd = sockfd;

    int r = uv_poll_init_socket(loop, &context->poll_handle, sockfd);
    assert(r == 0);
    context->poll_handle.data = context;

    return context;
}

void curl_close_cb(uv_handle_t *handle) {
    curl_context_t *context = (curl_context_t*) handle->data;
    free(context);
}

void destroy_curl_context(curl_context_t *context) {
    uv_close((uv_handle_t*) &context->poll_handle, curl_close_cb);
}


void add_download(const char *url, int id) {
    /* char filename[50]; */
    /* sprintf(filename, "%d.download", num); */
    /* FILE *file; */

    /* file = fopen(filename, "w"); */
    /* if (file == NULL) { */
    /*     fprintf(stderr, "Error opening %s\n", filename); */
    /*     return; */
    /* } */

    CURL *handle = curl_easy_init();
    curl_easy_setopt(handle, CURLOPT_WRITEFUNCTION, download_callback);
    curl_easy_setopt(handle, CURLOPT_WRITEDATA, (void*)id);
    curl_easy_setopt(handle, CURLOPT_URL, url);
    curl_multi_add_handle(curl_handle, handle);
    fprintf(stderr, "Added download %s\n", url);
}

void check_multi_info(void) {
    char *done_url;
    CURLMsg *message;
    int pending;

    while ((message = curl_multi_info_read(curl_handle, &pending))) {
        switch (message->msg) {
        case CURLMSG_DONE:
            curl_easy_getinfo(message->easy_handle, CURLINFO_EFFECTIVE_URL,
                            &done_url);
            printf("%s DONE\n", done_url);

            curl_multi_remove_handle(curl_handle, message->easy_handle);
            curl_easy_cleanup(message->easy_handle);
            break;

        default:
            fprintf(stderr, "CURLMSG default\n");
            abort();
        }
    }
}

void curl_perform(uv_poll_t *req, int status, int events) {
    uv_timer_stop(&timeout);
    int running_handles;
    int flags = 0;
    if (status < 0)                      flags = CURL_CSELECT_ERR;
    if (!status && events & UV_READABLE) flags |= CURL_CSELECT_IN;
    if (!status && events & UV_WRITABLE) flags |= CURL_CSELECT_OUT;

    curl_context_t *context;

    context = (curl_context_t*)req;

    curl_multi_socket_action(curl_handle, context->sockfd, flags, &running_handles);
    check_multi_info();   
}

void on_timeout(uv_timer_t *req) {
    int running_handles;
    curl_multi_socket_action(curl_handle, CURL_SOCKET_TIMEOUT, 0, &running_handles);
    check_multi_info();
}

void start_timeout(CURLM *multi, long timeout_ms, void *userp) {
    if (timeout_ms <= 0)
        timeout_ms = 1; /* 0 means directly call socket_action, but we'll do it in a bit */
    uv_timer_start(&timeout, on_timeout, timeout_ms, 0);
}

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
  printf("echo_write: freed\n");
}

ScmObj read_proc = SCM_UNDEFINED;

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

ScmObj new_conn_proc = SCM_UNDEFINED;

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
      // (id 'get "url")
      int id = SCM_INT_VALUE(SCM_CAR(result));
      const char *tag = SCM_STRING_BODY_START(SCM_STRING_BODY(SCM_SYMBOL_NAME(SCM_CADR(result))));
      ScmObj body = SCM_CDDR(result);

      printf("handle_response: % 8d: %s\n", id, tag);

      if (!strcmp("res", tag)) {
        uv_stream_t *client = (uv_stream_t*)SCM_INT_VALUE(SCM_CAR(body));
        const ScmStringBody* content = SCM_STRING_BODY(SCM_CADR(body));

        write_req_t *req = (write_req_t*) malloc(sizeof(write_req_t));
        int size = SCM_STRING_BODY_SIZE(content);
        char *string = (char*)malloc(size + 1);
        memcpy(string, SCM_STRING_BODY_START(content), size);
        string[size] = '\0';
        req->buf = uv_buf_init(string, size + 1);
        printf("handle_response: %p\n%s\n", req, string);
        uv_write((uv_write_t*) req, client, &req->buf, 1, echo_write);
      } else if (!strcmp("close", tag)) {
        uv_stream_t *client = (uv_stream_t*)SCM_INT_VALUE(SCM_CAR(body));
        printf("handle_response: closing %p\n", client);
        uv_close((uv_handle_t*)client, NULL);
      } else if (!strcmp("get", tag)) {
        const ScmStringBody* content = SCM_STRING_BODY(SCM_CAR(body));
        char *url = (char*)malloc(SCM_STRING_BODY_SIZE(content) + 1);
        url[SCM_STRING_BODY_SIZE(content)] = '\0';
        memcpy(url, SCM_STRING_BODY_START(content), SCM_STRING_BODY_SIZE(content));
        add_download(url, id);
        // needs free
      }
    } else {
      return;
    }
  }
}

int handle_socket(CURL *easy, curl_socket_t s, int action, void *userp, void *socketp) {
    curl_context_t *curl_context;
    if (action == CURL_POLL_IN || action == CURL_POLL_OUT) {
        if (socketp) {
            curl_context = (curl_context_t*) socketp;
        }
        else {
            curl_context = create_curl_context(s);
            curl_multi_assign(curl_handle, s, (void *) curl_context);
        }
    }

    switch (action) {
        case CURL_POLL_IN:
            uv_poll_start(&curl_context->poll_handle, UV_READABLE, curl_perform);
            break;
        case CURL_POLL_OUT:
            uv_poll_start(&curl_context->poll_handle, UV_WRITABLE, curl_perform);
            break;
        case CURL_POLL_REMOVE:
            if (socketp) {
                uv_poll_stop(&((curl_context_t*)socketp)->poll_handle);
                destroy_curl_context((curl_context_t*) socketp);                
                curl_multi_assign(curl_handle, s, NULL);
            }
            break;
        default:
            abort();
    }

    return 0;
}

int main() {
  loop = uv_default_loop();

  // Gauche
  Scm_Init(GAUCHE_SIGNATURE);

  ScmLoadPacket lpak;
  if (Scm_Load("./script.scm", 0, &lpak) < 0) {
    error_exit(lpak.exception);
  }

  ScmObj init_proc = SCM_UNDEFINED;

  SCM_BIND_PROC(init_proc,             "init",              Scm_CurrentModule());
  SCM_BIND_PROC(new_conn_proc,         "on-new-connection", Scm_CurrentModule());
  SCM_BIND_PROC(read_proc,             "on-read",           Scm_CurrentModule());
  SCM_BIND_PROC(dequeue_response_proc, "dequeue-response!", Scm_CurrentModule());
  SCM_BIND_PROC(result_proc,           "on-result",         Scm_CurrentModule());

  ScmEvalPacket epak;
  if (Scm_Apply(init_proc, SCM_NIL, &epak) < 0) {
    error_exit(epak.exception);
  }

  // Curl
  if (curl_global_init(CURL_GLOBAL_ALL)) {
    fprintf(stderr, "Could not init cURL\n");
    return 1;
  }
  uv_timer_init(loop, &timeout);

  curl_handle = curl_multi_init();
  curl_multi_setopt(curl_handle, CURLMOPT_SOCKETFUNCTION, handle_socket);
  curl_multi_setopt(curl_handle, CURLMOPT_TIMERFUNCTION, start_timeout);

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
    fprintf(stderr, "Listen error %s\n", uv_strerror(r));
    return 1;
  }
  return uv_run(loop, UV_RUN_DEFAULT);
}
