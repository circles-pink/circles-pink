import { NextFunction, Request, Response } from "express";
import express from "express";
import request from "request";
import bodyParser from "body-parser";

const app = express();

var myLimit = typeof process.argv[2] != "undefined" ? process.argv[2] : "100kb";
console.log("Using limit: ", myLimit);

app.use(bodyParser.json({ limit: myLimit }));

app.all("*", function (req: Request, res: Response, next: NextFunction) {
  // Set CORS headers: allow all origins, methods, and headers: you may want to lock this down in a production environment
  res.header("Access-Control-Allow-Origin", "*");
  res.header("Access-Control-Allow-Methods", "GET, PUT, PATCH, POST, DELETE");
  res.header(
    "Access-Control-Allow-Headers",
    req.header("access-control-request-headers")
  );

  if (req.method === "OPTIONS") {
    // CORS Preflight
    res.send();
  } else {
    var targetURL = req.header("Target-URL");
    if (!targetURL) {
      res.status(500).send({
        error: "There is no Target-Endpoint header in the request",
      });
      return;
    }
    request(
      {
        url: targetURL + req.url,
        method: req.method,
        json: req.body,
        headers: { Authorization: req.header("Authorization") },
      },
      function (error, response, body) {
        if (error) {
          console.error("There was an error", error);
        }
        console.log(body);
      }
    ).pipe(res);
  }
});

app.set("port", process.env.PORT || 3000);

app.listen(app.get("port"), function () {
  console.log("Proxy server listening on port " + app.get("port"));
});
