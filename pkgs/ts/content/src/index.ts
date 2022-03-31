import { Chain } from "@circles-pink/zeus-client/src/public/zeus";

type Content = {};

export const getContent = async ({
  endpoint,
}: {
  endpoint: string;
}): Promise<Content> => {
  const chain = Chain(endpoint, {
    headers: {
      "Content-Type": "application/json",
    },
  });

  return chain("query")({
    views: [{}, {}],
  });
};
