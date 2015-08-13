# racket-web-server
Simple web server written in Racket as a learning exercise
To start server, saving connection config for exiting in the future:
(define connection (serve port-num))
To close a connection:
(exit connection)
