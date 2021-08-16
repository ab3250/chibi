(define-library (websocket)
  (import
    (scheme base))
  (export
    ws_sendframe
    ws_sendframe_txt
    ws_sendframe_bin
    ws_close_client
    ws_getaddress
    ws_get_state
    ws_socket
    ws_start)        	                
(include-shared "websocket"))
