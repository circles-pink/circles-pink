import { Chain } from "@circles-pink/zeus-client/src/admin/zeus";
import * as querystring from "querystring";

if (!process.env.DIRECTUS_ADMIN_TOKEN) {
  throw new Error();
}

const qs = querystring.escape(
  `access_token=${process.env.DIRECTUS_ADMIN_TOKEN}`
);

const chain = Chain(`http://directus.circles.local/graphql/?${qs}`);

const main = async () => {
  // const result = await chain("query")({
  //   views_by_id: [
  //     { id: "hello" },
  //     {
  //       id: true,
  //       translations: [
  //         {},
  //         { foo: true, languages_id: [{}, { name: true, code: true }] },
  //       ],
  //     },
  //   ],
  // }).catch((e) => console.log(JSON.stringify(e, null, 2)));

  const result = await chain("mutation")({
    create_views_items: [
      {
        data: [
          {
            id: "hello4",
            status: "oops",
            enum: "oops",
          },
          // {
          //   id: "hello4",
          //   status: "oops",
          //   enum: "oops",
          //   translations: [
          //     {
          //       foo: "123",
          //       languages_id: {
          //         code: "us",
          //         name: "usa",
          //       },
          //     },
          //     {
          //       foo: "123111",
          //       languages_id: {
          //         code: "de",
          //         name: "deut",
          //       },
          //     },
          //   ],
          // },
          // {
          //   id: "hello5",
          //   status: "oops",
          //   enum: "oops",
          //   translations: [
          //     {
          //       foo: "123",
          //       languages_id: {
          //         code: "us",
          //         name: "usa",
          //       },
          //     },
          //     {
          //       foo: "123111",
          //       languages_id: {
          //         code: "de",
          //         name: "deut",
          //       },
          //     },
          //   ],
          // },
        ],
      },
      { id: true },
    ],
  }).catch((e) => console.log(JSON.stringify(e, null, 2)));

  console.log(JSON.stringify(result, null, 2));
};

main();
