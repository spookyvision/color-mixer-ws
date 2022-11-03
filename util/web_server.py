#!/usr/bin/env python3
import cgi
from http.server import BaseHTTPRequestHandler, HTTPServer, SimpleHTTPRequestHandler
from sys import argv
import time
import os

hostName = "localhost"
serverPort = 8081

start = time.time()

data = """{"2e2765f4-7fb7-4a8a-b61f-6dc874db15e9":{"uuid":"2e2765f4-7fb7-4a8a-b61f-6dc874db15e9","length":1,"bgr":false,"colors":[{"red":255,"green":150,"blue":0},{"red":255,"green":10,"blue":120}],"chill_idx":0,"chill_fac":100},"ae98126f-915e-470d-93a0-4b40a853a0c8":{"uuid":"ae98126f-915e-470d-93a0-4b40a853a0c8","length":1,"bgr":false,"colors":[{"red":166,"green":0,"blue":255},{"red":2,"green":192,"blue":192}],"chill_idx":1,"chill_fac":100},"bdc6cf10-c223-4c9e-9b94-88495d81617a":{"uuid":"bdc6cf10-c223-4c9e-9b94-88495d81617a","length":1,"bgr":false,"colors":[{"red":20,"green":200,"blue":141},{"red":200,"green":176,"blue":20}],"chill_idx":2,"chill_fac":100},"cba45b51-fd9a-48f4-95b3-070099050887":{"uuid":"cba45b51-fd9a-48f4-95b3-070099050887","length":1,"bgr":false,"colors":[{"red":200,"green":20,"blue":30},{"red":200,"green":200,"blue":10}],"chill_idx":3,"chill_fac":100}}""".encode(
    'utf-8')
data = "{}".encode("utf-8")
data = '{"1d3bd22e-3680-40aa-87e9-da3bdee55c4e":{"uuid":"1d3bd22e-3680-40aa-87e9-da3bdee55c4e","length":1,"bgr":false,"colors":[{"red":255,"green":150,"blue":0},{"red":255,"green":10,"blue":220}],"chill_idx":0,"chill_fac":500,"brightness":10}}'.encode(
    'utf-8')


class MyServer(SimpleHTTPRequestHandler):
    # protocol_version = "HTTP/1.1"

    def __init__(self, *args, **kwargs):
        self.directory = os.fspath(directory)
        super().__init__(*args, **kwargs)

    def do_OPTIONS(self):
        self.send_response(204)
        self.send_header('Access-Control-Allow-Origin', '*')
        self.send_header('Access-Control-Allow-Methods',
                         'GET, PUT, POST, OPTIONS')
        self.send_header('Access-Control-Allow-Headers', 'content-type')
        self.end_headers()

    def do_POST(self):
        self.send_response(204)
        self.send_header('Access-Control-Allow-Origin', '*')
        self.send_header('Access-Control-Allow-Methods',
                         'POST')
        self.send_header('Access-Control-Allow-Headers', 'content-type')

        self.log_message("headers")

        self.end_headers()
        self.flush_headers()
        global data
        data = self.rfile.read()
        self.log_message("read")
        self.log_message("response")
        self.wfile.write(b"ok")
        # self.data = data.decode("utf-8")
        print(data)

    def do_GET(self):
        def get(wat):
            self.send_response(200)

            self.send_header('Access-Control-Allow-Origin', '*')
            self.send_header('Access-Control-Allow-Methods', '*')
            self.send_header('Access-Control-Allow-Headers', '*')

            self.end_headers()
            self.wfile.write(wat)

        if self.path == '/now':
            dt = int((time.time() - start)*1000)
            get(f'{dt}'.encode('ascii'))

        elif self.path == '/data':
            get(data)
        else:
            super().do_GET()


if __name__ == "__main__":
    global directory
    directory = argv[1]
    webServer = HTTPServer((hostName, serverPort), MyServer)
    print("Server started http://%s:%s" % (hostName, serverPort))

    try:
        webServer.serve_forever()
    except KeyboardInterrupt:
        pass

    webServer.server_close()
    print("Server stopped.")
