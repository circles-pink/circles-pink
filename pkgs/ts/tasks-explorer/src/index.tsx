import React from "react";
import CytoscapeComponent from "react-cytoscapejs";
import dagre from "cytoscape-dagre";
import Cytoscape from "cytoscape";
import COSEBilkent from "cytoscape-cose-bilkent";

Cytoscape.use(COSEBilkent);

type TasksExplorerProps = { url: string; database_id: string };

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

type Task = {
  id: string;
  label: string;
  dependencies: string[];
  status: Status;
  href: string;
};

type Status = "Backlog" | "To Do" | "Done" | "In Progress";

type Node = { data: Task };

type Edge = { data: { source: string; target: string } };

const toTask = (notionTask: NotionTask): Task => {
  return {
    id: notionTask.id,
    label: notionTask.properties.Name.title.map((x) => x.plain_text).join(""),
    dependencies: notionTask.properties.dependencies.relation.map((x) => x.id),
    status: notionTask.properties.Status.select.name as Status,
    href: notionTask.url,
  };
};

export const TasksExplorer = ({ url, database_id }: TasksExplorerProps) => {
  const [data, setData] = React.useState<Task[]>([]);

  React.useEffect(() => {
    fetch(url)
      .then((response) => response.json())
      .then((result: ApiResult) => {
        const tasks = getNotionTasks(database_id)(result).map(toTask);
        setData(tasks);
      });
  }, [database_id, url]);

  const nodes: Node[] = data.map((x) => ({
    data: x,
  }));

  const edges: Edge[] = data.flatMap((source) =>
    source.dependencies.map((target) => ({
      data: { source: source.id, target: target, label: "X" },
    }))
  );

  const layout = {
    name: "cose-bilkent",
    // other options
    padding: 50,
    nodeDimensionsIncludeLabels: true,
    idealEdgeLength: 100,
    edgeElasticity: 0.1,
    //nodeRepulsion: 8500,
    //randomize: true,
  };

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
        link: "http://www.spiegel.de",
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

  const elements = [...nodes, ...edges];

  if (data.length === 0) return null;

  return (
    <CytoscapeComponent
      cy={(cy) => {
        cy.on("tap", "node", function () {
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
  );
};

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
