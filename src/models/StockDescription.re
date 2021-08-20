open SheetDecoder;
open KeyMapping;

[@bs.val] external fetch: string => Js.Promise.t('a) = "fetch";
type description = string;

type t = {
  name: string,
  description,
};

type cell = {
  row: int,
  col: int,
  value: string,
};

let default: t = {name: "", description: ""};

let parseRows = (rows: list(row)): list(t) => {
  rows
    |> List.map(row => {
      switch row.cols {
      | [name, _, _, description] => {name: name.value, description: description.value}
      | _ => default
      };
    })
}

let useStockMatcher = stockDescriptionTsvUrl => {
  let (descriptions, setDescriptions) = React.useState(_ => None);

  React.useEffect0(() => {
    Js.Promise.(
      fetch(stockDescriptionTsvUrl)
      |> then_(response => response##text())
      |> then_(tsv => {
           tsv |> tsvToRows |> parseRows |> (s => setDescriptions(_ => Some(s)));
           Js.Promise.resolve();
         })
      |> catch(err => {
           Js.log(err);
           setDescriptions(_ => None);
           Js.Promise.resolve();
         })
      |> ignore
    );
    None;
  });

  // returning matcher function
  (keySwitch: option(keySwitch)) => (
    switch (keySwitch, descriptions) {
    | (Some(s), Some(ds)) => ds->List.find_opt(d => s.name == d.name, _)
    | _ => None
    }:
      option(t)
  );
};
