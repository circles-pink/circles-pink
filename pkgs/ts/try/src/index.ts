import { Client } from "@notionhq/client";

// Initializing a client
const notion = new Client({
  auth: process.env.NOTION_TOKEN,
});

(async () => {
  //   const r = await notion.databases.query({
  //     database_id: "97c952d7b04541c790ab0f1e4cf5b10c",
  //     filter: {property: {}}
  //   });

  // notion.pages
  //   .retrieve({ page_id: "a2a29b04-1491-47de-af06-c41fb035ed09" })
  //   .then(console.log);
  //   console.log(r);
  //const listUsersResponse = await notion.users.list({});

  notion.search({}).then((r) => console.log(JSON.stringify(r)));
})();
