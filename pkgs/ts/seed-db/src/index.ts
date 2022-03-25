import { Chain } from "../zeus";

const chain = Chain("http://directus.circles.local/graphql");

const main = async () => {
  //   const result = await chain("query")({
  //     views_by_id: [{ id: "foo" }, { id: true }],
  //   }).catch((e) => console.log(e));

  const result = await chain("mutation")({
    create_views_item: [{ data: { id: "baz", status: "baz" } }, { id: true }],
  }).catch((e) => console.log(e));

  console.log(result);
};

main();
