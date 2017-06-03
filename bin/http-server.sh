#!/bin/bash
PORT=11002
echo "Visit: strophejs.localhost:"$PORT" to test webpage"
#webpack-dev-server --history-api-fallback --port $PORT
#python -m http.server "$PORT"
python2 -c "import BaseHTTPServer as bhs, SimpleHTTPServer as shs; bhs.HTTPServer(('strophejs.localhost', $PORT), shs.SimpleHTTPRequestHandler).serve_forever()"
