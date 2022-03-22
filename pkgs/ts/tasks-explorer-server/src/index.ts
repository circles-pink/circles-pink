import { Client } from "@notionhq/client";
import express from "express";
import cors from "cors";

const app = express();

if (process.env.DEV) {
  console.log("Starting in dev mode");
  app.use(cors());
}

const port = process.env.PORT || 8080;

const notion = new Client({
  auth: process.env.NOTION_TOKEN,
});

app.get("/", (req, res) => {
  notion.search({}).then((result) => {
    res.json(result);
  });
});

app.listen(port, () => {
  console.log(`server started at http://localhost:${port}`);
});
