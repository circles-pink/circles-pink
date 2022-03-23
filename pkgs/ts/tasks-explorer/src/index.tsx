import React, { Dispatch, SetStateAction } from "react";
import CytoscapeComponent from "react-cytoscapejs";
import dagre from "cytoscape-dagre";
import Cytoscape from "cytoscape";
import COSEBilkent from "cytoscape-cose-bilkent";
import { Client as Styletron } from "styletron-engine-atomic";
import { Provider as StyletronProvider } from "styletron-react";
import { LightTheme, BaseProvider, styled } from "baseui";
import { Checkbox, LABEL_PLACEMENT } from "baseui/checkbox";
import { Accordion, Panel } from "baseui/accordion";
import { Button, SIZE } from "baseui/button";

// -----------------------------------------------------------------------------
// Types
// -----------------------------------------------------------------------------

type NotionTask = {
  id: string;
  properties: {
    Name: { title: { plain_text: string }[] };
    Status: { select: null | { name: string } };
    dependencies: { relation: { id: string }[] };
  };
  url: string;
};

type Result =
  | {}
  | ({
      object: "page";
      id: string;
      parent: { database_id: string };
    } & NotionTask);

type ApiResult = { results: Result[] };

type Status = "Backlog" | "To Do" | "Done" | "In Progress";

type Node = { data: Task };

type Edge = { data: { source: string; target: string } };

type Task = {
  id: string;
  label: string;
  dependencies: string[];
  status: Status;
  href: string;
};

// -----------------------------------------------------------------------------
// Init
// -----------------------------------------------------------------------------

Cytoscape.use(COSEBilkent);
const engine = new Styletron();

// -----------------------------------------------------------------------------
// UI / TasksExplorer
// -----------------------------------------------------------------------------

type TasksExplorerProps = { url: string; database_id: string };

type Filter = {
  status: { [key in Status]: boolean };
};

const filterItem =
  (filter: Filter) =>
  (task: Task): boolean =>
    filter.status[task.status];

const filterData = (filter: Filter, data: Task[]): Task[] =>
  data.filter(filterItem(filter));

const getElements = (data: Task[]): (Node | Edge)[] => {
  const nodes: Node[] = data.map((x) => ({
    data: x,
  }));

  const edges: Edge[] = data.flatMap((source) =>
    source.dependencies.map((target) => ({
      data: { source: source.id, target: target, label: "X" },
    }))
  );

  return [...nodes, ...edges];
};

const layout = {
  name: "cose-bilkent",
  // other options
  padding: 50,
  nodeDimensionsIncludeLabels: true,
  idealEdgeLength: 100,
  edgeElasticity: 0.1,
  animate: "end",
  animationDuration: 200,
  //nodeRepulsion: 8500,
  //randomize: true,
};

export const TasksExplorer = ({ url, database_id }: TasksExplorerProps) => {
  const [data, setData] = React.useState<Task[]>([]);

  const [cy, setCy] = React.useState<Cytoscape.Core | undefined>();

  const [filter, setFilter] = React.useState<Filter>({
    status: { Backlog: true, "To Do": true, Done: true, "In Progress": true },
  });

  const data_ = filterData(filter, data);

  React.useEffect(() => {
    if (!cy) return;
    var layout_ = cy.layout(layout);
    layout_.run();
  }, [JSON.stringify(data_)]);

  const refreshApi = React.useCallback(
    () =>
      fetch(url)
        .then((response) => response.json())
        .then((result: ApiResult) => {
          const tasks = getNotionTasks(database_id)(result).map(toTask);
          setData(tasks);
        }),
    []
  );

  React.useEffect(() => {
    refreshApi();
  }, [database_id, url]);

  const elements = getElements(data_);

  const stylesheets = [
    {
      selector: "node",
      style: {
        width: "label",
        height: "label",
        padding: "8px",
        shape: "round-rectangle",
        "background-color": (node) => statusToColor(node.data("status")),
        label: "data(label)", // here you can label the nodes
      } as any,
    },
    {
      selector: "node[label]",
      style: {
        label: "data(label)",
        "font-size": "20",
        color: "black",
        "text-halign": "center",
        "text-valign": "center",
      },
    },
    {
      selector: "edge",
      style: {
        "curve-style": "bezier",
        "target-arrow-shape": "triangle",
        width: 1.5,
      },
    },
  ];

  return (
    <StyletronProvider value={engine}>
      <BaseProvider theme={LightTheme}>
        <div style={{ display: "flex" }}>
          <div style={{ width: "50%" }}>
            <Filter
              filter={filter}
              setFilter={setFilter}
              refreshApi={refreshApi}
            />
          </div>
          <CytoscapeComponent
            cy={(cy_) => {
              if (!cy) setCy(cy_);
              cy_.on("tap", "node", function () {
                try {
                  // your browser may block popups
                  window.open(this.data("href"));
                } catch (e) {
                  // fall back on url change
                  window.location.href = this.data("href");
                }
              });
            }}
            elements={elements}
            style={{
              width: "100%",
              height: "600px",
              backgroundColor: "rgba(249, 249, 245, 0.5)",
              boxShadow: "0 0 4px 1px rgba(0,0,0,0.05)",
            }}
            layout={layout}
            stylesheet={stylesheets}
          />
        </div>
      </BaseProvider>
    </StyletronProvider>
  );
};

// -----------------------------------------------------------------------------
// UI / Filter
// -----------------------------------------------------------------------------

type FilterProps = {
  filter: Filter;
  setFilter: Dispatch<SetStateAction<Filter>>;
  refreshApi: () => void;
};

const Filter = ({ filter, setFilter, refreshApi }: FilterProps) => (
  <Accordion>
    <Panel title="API">
      <Button onClick={() => refreshApi()} size={SIZE.mini}>
        Refresh
      </Button>
    </Panel>
    <Panel title="Status">
      {(Object.entries(filter.status) as [Status, boolean][]).map(
        ([key, value]) => (
          <Checkbox
            key={key}
            checked={filter.status[key]}
            onChange={(e) =>
              setFilter((s) => ({
                ...s,
                status: {
                  ...s.status,
                  [key]: !s.status[key],
                },
              }))
            }
            labelPlacement={LABEL_PLACEMENT.right}
          >
            {key}
          </Checkbox>
        )
      )}
    </Panel>
  </Accordion>
);

// -----------------------------------------------------------------------------
// UI / Select
// -----------------------------------------------------------------------------

type SelectMultiProps = { options: { id: string }[] };

const SelectMulti = ({}: SelectMultiProps) => <></>;

// -----------------------------------------------------------------------------
// Util
// -----------------------------------------------------------------------------

const statusToColor = (status: Status): string => {
  switch (status) {
    case "Backlog":
      return "rgba(227, 226, 224, 0.5)";
    case "To Do":
      return "rgb(211, 229, 239)";
    case "In Progress":
      return "rgb(253, 236, 200)";
    case "Done":
      return "rgb(219, 237, 219)";
  }
};

const getNotionTasks =
  (db_id: string) =>
  (apiResult: ApiResult): NotionTask[] => {
    const tasks = apiResult.results.filter((item) => {
      if (!("object" in item)) return false;
      if (!item.properties.Status.select) return false;
      if (item.object !== "page") return false;
      if (!("parent" in item)) return false;
      if (!("database_id" in item.parent)) return false;
      return item.parent.database_id === db_id;
    }) as unknown as NotionTask[];

    return tasks;
  };

const toTask = (notionTask: NotionTask): Task => {
  return {
    id: notionTask.id,
    label: notionTask.properties.Name.title.map((x) => x.plain_text).join(""),
    dependencies: notionTask.properties.dependencies.relation.map((x) => x.id),
    status: notionTask.properties.Status.select.name as Status,
    href: notionTask.url,
  };
};
