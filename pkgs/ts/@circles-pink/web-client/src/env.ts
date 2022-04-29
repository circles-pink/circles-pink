const envRaw = {
  gardenApi: process.env.STORYBOOK_GARDEN_API,
  gardenApiUsers: process.env.STORYBOOK_GARDEN_API_USERS,
  gardenGraphApi: process.env.STORYBOOK_GARDEN_GRAPH_API,
  gardenSubgraphName: process.env.STORYBOOK_GARDEN_SUBGRAPH_NAME,
  gardenRelay: process.env.STORYBOOK_GARDEN_RELAY,
  gardenHubAddress: process.env.STORYBOOK_GARDEN_HUB_ADDRESS,
  gardenProxyFactoryAddress: process.env.STORYBOOK_GARDEN_PROXY_FACTORY_ADRESS,
  gardenSafeMasterAddress: process.env.STORYBOOK_GARDEN_SAFE_MASTER_ADDRESS,
  gardenEthereumNodeWebSocket: process.env.STORYBOOK_GARDEN_ETHEREUM_NODE_WS,
};

type RemoveUndefined<T> = { [key in keyof T]: Exclude<T[key], undefined> };

type Env = RemoveUndefined<typeof envRaw>;

const parseEnv = (env_: typeof envRaw): Env => {
  const xs = Object.entries(env_).forEach(([k, v]) => {
    if (v === undefined) {
      throw new Error(`Env var missing: ${k}`);
    }
  });

  return env_ as Env;
};

export const env = parseEnv(envRaw);
