#include "chibi/eval.h"
#include "fio.h"
#include "fio_cli.h"
#include "fio_tls.h"
#include "http.h"
#include "redis_engine.h"

static sexp ctx2;

static void on_http_request(http_s *h);
/* HTTP upgrade request handler */
static void on_http_upgrade(http_s *h, char *requested_protocol, size_t len);
/* Command Line Arguments Management */
static void initialize_cli(int argc, char const *argv[]);
/* Initializes Redis, if set by command line arguments */
static void initialize_redis(void);

int ws_start(void) {
  initialize_cli(0,NULL);
  initialize_redis();
  /* TLS support */
  fio_tls_s *tls = NULL;
  if (fio_cli_get_bool("-tls")) {
    char local_addr[1024];
    local_addr[fio_local_addr(local_addr, 1023)] = 0;
    tls = fio_tls_new(local_addr, NULL, NULL, NULL);
  }
  /* optimize WebSocket pub/sub for multi-connection broadcasting */
  websocket_optimize4broadcasts(WEBSOCKET_OPTIMIZE_PUBSUB, 1);
  /* listen for inncoming connections */
  if (http_listen(fio_cli_get("-p"), fio_cli_get("-b"),
                  .on_request = on_http_request,
                  .on_upgrade = on_http_upgrade,
                  .max_body_size = (fio_cli_get_i("-maxbd") * 1024 * 1024),
                  .ws_max_msg_size = (fio_cli_get_i("-maxms") * 1024),
                  .public_folder = fio_cli_get("-public"),
                  .log = fio_cli_get_bool("-log"),
                  .timeout = fio_cli_get_i("-keep-alive"),
                  .tls = tls,
                  .ws_timeout = fio_cli_get_i("-ping")) == -1) {
    /* listen failed ?*/
    perror(
        "ERROR: facil.io couldn't initialize HTTP service (already running?)");
    exit(1);
  }
  fio_start(.threads = fio_cli_get_i("-t"), .workers = fio_cli_get_i("-w"));
  fio_cli_end();
  fio_tls_destroy(tls);
  return 0;
}

/* *****************************************************************************
HTTP Request / Response Handling
***************************************************************************** */

static void on_http_request(http_s *h) {
  /* set a response and send it (finnish vs. destroy). */
  http_send_body(h, "Hello World!", 12);
}

/* *****************************************************************************
HTTP Upgrade Handling
***************************************************************************** */

/* WebSocket Handlers */
static void ws_on_open(ws_s *ws);
static void ws_on_message(ws_s *ws, fio_str_info_s msg, uint8_t is_text);
static void ws_on_shutdown(ws_s *ws);
static void ws_on_close(intptr_t uuid, void *udata);

/* HTTP upgrade callback */
static void on_http_upgrade(http_s *h, char *requested_protocol, size_t len) {
  /* Upgrade to  WebSockets and set the request path as a nickname. */
  FIOBJ nickname;
  if (fiobj_obj2cstr(h->path).len > 1) {
    nickname = fiobj_str_new(fiobj_obj2cstr(h->path).data + 1,
                             fiobj_obj2cstr(h->path).len - 1);
  } else {
    nickname = fiobj_str_new("Guest", 5);
  }
  /* Test for upgrade protocol (websocket) */
  if (len == 9 && requested_protocol[1] == 'e') {
    if (fio_cli_get_bool("-v")) {
      fprintf(stderr, "* (%d) new WebSocket connection: %s.\n", getpid(),
              fiobj_obj2cstr(nickname).data);
    }
    http_upgrade2ws(h, .on_message = ws_on_message, .on_open = ws_on_open,
                    .on_shutdown = ws_on_shutdown, .on_close = ws_on_close,
                    .udata = (void *)nickname);
  } else {
    fprintf(stderr, "WARNING: unrecognized HTTP upgrade request: %s\n",
            requested_protocol);
    http_send_error(h, 400);
    fiobj_free(nickname); // we didn't use this
  }
}

/* *****************************************************************************
Globals
***************************************************************************** */

static fio_str_info_s CHAT_CANNEL = {.data = "chat", .len = 4};


/* *****************************************************************************
WebSockets Callbacks
***************************************************************************** */

static void ws_on_message(ws_s *ws, fio_str_info_s msg, uint8_t is_text) {
  sexp ctx = ctx2;
 sexp_gc_var1(cmd); 
 sexp_gc_preserve1(ctx,cmd);
 cmd = sexp_list3(ctx,sexp_c_string(ctx, msg, -1),
        sexp_make_integer(ctx, size),sexp_make_integer(ctx, type));
 cmd = sexp_cons(ctx,sexp_make_integer(ctx, fd), cmd);
 cmd = sexp_cons(ctx,sexp_intern(ctx, "onmessage", -1),cmd); 
 sexp_eval(ctx, cmd, NULL);
 sexp_gc_release1(ctx);   
  // Add the Nickname to the message
 

//  FIOBJ str = fiobj_str_copy((FIOBJ)websocket_udata_get(ws));
//  fiobj_str_write(str, ": ", 2);
//  fiobj_str_write(str, msg.data, msg.len);
  // publish
  //fio_publish(.channel = CHAT_CANNEL, .message = fiobj_obj2cstr(str));
  // free the string
//  fiobj_free(str);
//  (void)is_text; // we don't care.
//  (void)ws;      // this could be used to send an ACK, but we don't.
}
static void ws_on_open(ws_s *ws) {
  sexp ctx = ctx2;
 sexp_gc_var1(cmd); 
 sexp_gc_preserve1(ctx,cmd);
 cmd = sexp_list2(ctx, sexp_intern(ctx, "onopen", -1),sexp_make_integer(ctx, fd));
 sexp_eval(ctx, cmd, NULL);
 sexp_gc_release1(ctx);
  //websocket_subscribe(ws, .channel = CHAT_CANNEL);
 // websocket_write(
  //    ws, (fio_str_info_s){.data = "Welcome to Server.", .len = 18}, 1);
  //FIOBJ tmp = fiobj_str_copy((FIOBJ)websocket_udata_get(ws));
  //fiobj_str_write(tmp, " joind the chat.", 16);
  //fio_publish(.channel = CHAT_CANNEL, .message = fiobj_obj2cstr(tmp));
  //fiobj_free(tmp);
}
static void ws_on_shutdown(ws_s *ws) {
  websocket_write(
      ws, (fio_str_info_s){.data = "Server shutting down, goodbye.", .len = 30},
      1);
}

static void ws_on_close(intptr_t uuid, void *udata) {
  sexp ctx = ctx2;
  sexp_gc_var1(cmd); 
  sexp_gc_preserve1(ctx,cmd);
  cmd = sexp_list2(ctx, sexp_intern(ctx, "onclose", -1),sexp_make_integer(ctx, fd));
  sexp_eval(ctx, cmd, NULL);
  sexp_gc_release1(ctx); 
  /* Let everyone know we left the chat */
  //fiobj_str_write((FIOBJ)udata, " left the chat.", 15);
  //fio_publish(.channel = CHAT_CANNEL, .message = fiobj_obj2cstr((FIOBJ)udata));
  /* free the nickname */
  //fiobj_free((FIOBJ)udata);
  (void)uuid; // we don't use the ID
}



/* *****************************************************************************
Redis initialization
***************************************************************************** */
static void initialize_redis(void) {
  if (!fio_cli_get("-redis") || !strlen(fio_cli_get("-redis")))
    return;
  FIO_LOG_STATE("* Initializing Redis connection to %s\n",
                fio_cli_get("-redis"));
  fio_url_s info =
      fio_url_parse(fio_cli_get("-redis"), strlen(fio_cli_get("-redis")));
  fio_pubsub_engine_s *e =
      redis_engine_create(.address = info.host, .port = info.port,
                          .auth = info.password);
  if (e)
    fio_state_callback_add(FIO_CALL_ON_FINISH,
                           (void (*)(void *))redis_engine_destroy, e);
  FIO_PUBSUB_DEFAULT = e;
}

/* *****************************************************************************
CLI helpers
***************************************************************************** */
static void initialize_cli(int argc, char *argv[]) {
  argv
  /*     ****  Command line arguments ****     */
  fio_cli_start(
      argc, argv, 0, 0, NULL,
      // Address Binding
      FIO_CLI_PRINT_HEADER("Address Binding:"),
      FIO_CLI_INT("-port -p port number to listen to. defaults port 3000"),
      "-bind -b address to listen to. defaults any available.",
      FIO_CLI_BOOL("-tls use a self signed certificate for TLS."),
      // Concurrency
      FIO_CLI_PRINT_HEADER("Concurrency:"),
      FIO_CLI_INT("-workers -w number of processes to use."),
      FIO_CLI_INT("-threads -t number of threads per process."),
      // HTTP Settings
      FIO_CLI_PRINT_HEADER("HTTP Settings:"),
      "-public -www public folder, for static file service.",
      FIO_CLI_INT(
          "-keep-alive -k HTTP keep-alive timeout (0..255). default: 10s"),
      FIO_CLI_INT(
          "-max-body -maxbd HTTP upload limit in Mega Bytes. default: 50Mb"),
      FIO_CLI_BOOL("-log -v request verbosity (logging)."),
      // WebSocket Settings
      FIO_CLI_PRINT_HEADER("WebSocket Settings:"),
      FIO_CLI_INT("-ping websocket ping interval (0..255). default: 40s"),
      FIO_CLI_INT("-max-msg -maxms incoming websocket message "
                  "size limit in Kb. default: 250Kb"),
      // Misc Settings
      FIO_CLI_PRINT_HEADER("Misc:"),
      FIO_CLI_STRING("-redis -r an optional Redis URL server address."),
      FIO_CLI_PRINT("\t\ta valid Redis URL would follow the pattern:"),
      FIO_CLI_PRINT("\t\t\tredis://user:password@localhost:6379/"),
      FIO_CLI_INT("-verbosity -V facil.io verbocity 0..5 (logging level)."));

  /* Test and set any default options */
  if (!fio_cli_get("-p")) {
    /* Test environment as well */
    char *tmp = getenv("PORT");
    if (!tmp)
      tmp = "3000";
    /* CLI et functions (unlike fio_cli_start) ignores aliases */
    fio_cli_set("-p", tmp);
    fio_cli_set("-port", tmp);
  }
  if (!fio_cli_get("-b")) {
    char *tmp = getenv("ADDRESS");
    if (tmp) {
      fio_cli_set("-b", tmp);
      fio_cli_set("-bind", tmp);
    }
  }
  if (!fio_cli_get("-public")) {
    char *tmp = getenv("HTTP_PUBLIC_FOLDER");
    if (tmp) {
      fio_cli_set("-public", tmp);
      fio_cli_set("-www", tmp);
    }
  }

  if (!fio_cli_get("-redis")) {
    char *tmp = getenv("REDIS_URL");
    if (tmp) {
      fio_cli_set("-redis", tmp);
      fio_cli_set("-r", tmp);
    }
  }
  if (fio_cli_get("-V")) {
    FIO_LOG_LEVEL = fio_cli_get_i("-V");
  }

  fio_cli_set_default("-ping", "40");

  /* CLI set functions (unlike fio_cli_start) ignores aliases */
  fio_cli_set_default("-k", "10");
  fio_cli_set_default("-keep-alive", "10");

  fio_cli_set_default("-max-body", "50");
  fio_cli_set_default("-maxbd", "50");

  fio_cli_set_default("-max-message", "250");
  fio_cli_set_default("-maxms", "250");
}

sexp sexp_ws_close_client_stub (sexp ctx, sexp self, sexp_sint_t n, sexp arg0) {
  sexp res;
  if (! sexp_exact_integerp(arg0))
    return sexp_type_exception(ctx, self, SEXP_FIXNUM, arg0);
  res = sexp_make_integer(ctx, ws_close_client(sexp_sint_value(arg0)));
  return res;
}

sexp sexp_ws_sendframe_stub (sexp ctx, sexp self, sexp_sint_t n, sexp arg0, sexp arg1, sexp arg2, sexp arg3, sexp arg4) {
  sexp res;
  if (! sexp_exact_integerp(arg0))
    return sexp_type_exception(ctx, self, SEXP_FIXNUM, arg0);
  if (! sexp_stringp(arg1))
    return sexp_type_exception(ctx, self, SEXP_STRING, arg1);
  if (! sexp_exact_integerp(arg2))
    return sexp_type_exception(ctx, self, SEXP_FIXNUM, arg2);
  if (! sexp_exact_integerp(arg4))
    return sexp_type_exception(ctx, self, SEXP_FIXNUM, arg4);
  res = sexp_make_integer(ctx, ws_sendframe(sexp_sint_value(arg0), sexp_string_data(arg1), sexp_uint_value(arg2), sexp_truep(arg3), sexp_sint_value(arg4)));
  return res;
}

sexp sexp_ws_start_stub (sexp ctx, sexp self, sexp_sint_t n) {
  sexp res;
  res = sexp_make_integer(ctx, ws_start());
  return res;
}


sexp sexp_init_library (sexp ctx, sexp self, sexp_sint_t n, sexp env, const char* version, const sexp_abi_identifier_t abi) {
  sexp_gc_var3(name, tmp, op);
  if (!(sexp_version_compatible(ctx, version, sexp_version)
        && sexp_abi_compatible(ctx, abi, SEXP_ABI_IDENTIFIER)))
    return SEXP_ABI_ERROR;
  sexp_gc_preserve3(ctx, name, tmp, op);
  op = sexp_define_foreign(ctx, env, "ws_close_client", 1, sexp_ws_close_client_stub);
  if (sexp_opcodep(op)) {
    sexp_opcode_return_type(op) = sexp_make_fixnum(SEXP_FIXNUM);
    sexp_opcode_arg1_type(op) = sexp_make_fixnum(SEXP_FIXNUM);
  }
  op = sexp_define_foreign(ctx, env, "ws_sendframe", 5, sexp_ws_sendframe_stub);
  if (sexp_opcodep(op)) {
    sexp_opcode_return_type(op) = sexp_make_fixnum(SEXP_FIXNUM);
    sexp_opcode_arg1_type(op) = sexp_make_fixnum(SEXP_FIXNUM);
    sexp_opcode_arg2_type(op) = sexp_make_fixnum(SEXP_STRING);
    sexp_opcode_arg3_type(op) = sexp_make_fixnum(SEXP_FIXNUM);
    sexp_opcode_argn_type(op) = sexp_make_vector(ctx, SEXP_TWO, sexp_make_fixnum(SEXP_OBJECT));
    sexp_vector_set(sexp_opcode_argn_type(op), SEXP_ZERO, sexp_make_fixnum(SEXP_BOOLEAN));
    sexp_vector_set(sexp_opcode_argn_type(op), SEXP_ONE, sexp_make_fixnum(SEXP_FIXNUM));
  }
  op = sexp_define_foreign(ctx, env, "ws_start", 0, sexp_ws_start_stub);
  if (sexp_opcodep(op)) {
    sexp_opcode_return_type(op) = sexp_make_fixnum(SEXP_FIXNUM);
  }
  sexp_gc_release3(ctx);
  ctx2 = ctx;  
  sexp_preserve_object(ctx, ctx2);
  return SEXP_VOID;
}

