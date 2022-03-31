import { Chain } from "@circles-pink/zeus-client/src/admin/zeus";


if (!process.env.DIRECTUS_ADMIN_TOKEN) {
  throw new Error();
}

const chain = Chain(`http://directus.circles.local/graphql`, {
  headers: {
    "Content-Type": "application/json",
    Authorization: `Bearer ${process.env.DIRECTUS_ADMIN_TOKEN}`,
  },
});

const main = async () => {
  const result = await chain("mutation")({
    create_views_items: [
      {
        data: [
          {
            id: "hello3",
            status: "oops",
            enum: "oops",
            translations: [
              {
                foo: "123",
                languages_id: {
                  code: "us",
                  name: "usa",
                },
              },
              {
                foo: "123111",
                languages_id: {
                  code: "de",
                  name: "deut",
                },
              },
            ],
          },
          {
            id: "hello5",
            status: "oops",
            enum: "oops",
            translations: [
              {
                foo: "123",
                languages_id: {
                  code: "us",
                  name: "usa",
                },
              },
              {
                foo: "123111",
                languages_id: {
                  code: "de",
                  name: "deut",
                },
              },
            ],
          },
        ],
      },
      { id: true },
    ],
  }).catch((e) => console.log(JSON.stringify(e, null, 2)));

  console.log(JSON.stringify(result, null, 2));
};

main();
