const http = require("http");
const fs = require("fs");
const path = require("path");

const PORT = 4000;
const ROOT = path.join(__dirname, "docs");

const MIME = {
  ".html": "text/html",
  ".css": "text/css",
  ".js": "application/javascript",
  ".json": "application/json",
  ".png": "image/png",
  ".jpg": "image/jpeg",
  ".svg": "image/svg+xml",
  ".woff": "font/woff",
  ".woff2": "font/woff2",
};

http.createServer((req, res) => {
  let filePath = path.join(ROOT, req.url === "/" ? "index.html" : req.url);
  if (!path.extname(filePath)) filePath += ".html";

  fs.readFile(filePath, (err, data) => {
    if (err) {
      res.writeHead(404);
      res.end("Not found");
      return;
    }
    const ext = path.extname(filePath);
    res.writeHead(200, { "Content-Type": MIME[ext] || "application/octet-stream" });
    res.end(data);
  });
}).listen(PORT, () => console.log(`Serving docs/ at http://localhost:${PORT}`));
