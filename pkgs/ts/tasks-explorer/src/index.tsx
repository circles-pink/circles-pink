import React from "react";

type TasksExplorerProps = { url: string };

type Task = {
  id: string;
  properties: {
    name: { title: { plain_text: string }[] };
    dependencies: { relation: { id: string }[] };
  };
};

type Result =
  | unknown
  | ({ object: "page"; id: string; parent: { database_id: string } } & Task);

type ApiResult = { results: Result[] };

const db_id = "97c952d7-b045-41c7-90ab-0f1e4cf5b10c";

export const TasksExplorer = ({ url }: TasksExplorerProps) => {
  const [data, setData] = React.useState<Task[]>([]);

  React.useEffect(() => {
    fetch(url)
      .then((response) => response.json())
      .then((result: any) => {
        const tasks = result.results.filter((item) => {
          if (item.object !== "page") return false;
          if (!("parent" in item)) return false;
          if (!("database_id" in item.parent)) return false;
          return item.parent.database_id === db_id;
        }) as unknown as Task[];
        setData(tasks);
      });
  }, []);

  return <pre>{JSON.stringify(data, null, 2)}</pre>;
};
