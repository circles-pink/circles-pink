const envDefault = {
  gardenApi: 'https://api.circles.garden',
  gardenApiUsers: 'https://api.circles.garden/api/users',
  gardenGraphApi: 'https://api.thegraph.com',
  gardenSubgraphName: 'azf20/circles-ubi',
  gardenRelay: 'https://relay.circles.garden',
  gardenHubAddress: '0x29b9a7fBb8995b2423a71cC17cf9810798F6C543',
  gardenProxyFactoryAddress: '0x8b4404DE0CaECE4b966a9959f134f0eFDa636156',
  gardenSafeMasterAddress: '0x2CB0ebc503dE87CFD8f0eCEED8197bF7850184ae',
  gardenEthereumNodeWebSocket: 'wss://dark-frosty-field.xdai.quiknode.pro',
};

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
  const xs = Object.entries(env_).map(([k, v]) => {
    if (v === undefined) {
      return [k, envDefault[k as keyof Env]];
    } else {
      return [k, v];
    }
  });
  return Object.fromEntries(xs) as Env;
};

export const env = parseEnv(envRaw);
