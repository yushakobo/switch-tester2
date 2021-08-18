open SheetDecoder;

[@bs.val] external fetch: string => Js.Promise.t('a) = "fetch";
type description = string;

type t = {
  name: string,
  description,
};

let default: t = {name: "", description: ""};

let updateStockDescription = (m: t, cell: cell): t =>
  switch (cell.col) {
  | 1 => {...m, name: cell.value}
  | 4 => {...m, description: cell.value}
  | _ => m
  };

let makeStockDescription = (data: array(cell)): t =>
  data
  |> Array.fold_left((m, cell) => updateStockDescription(m, cell), default);

let parseSheet = (cells: list(cell)): option(list(t)) => {
  let rowGroupedCells =
    cells
    |> Array.of_list
    |> CollectionUtils.groupBy((cell: cell) => cell.row)
    |> Array.to_list;
  switch (rowGroupedCells) {
  | [_, ...rows] =>
    let descriptions =
      rows
      |> List.map(row =>
           switch (row) {
           | (_rowNum, rowData) => makeStockDescription(rowData)
           }
         );
    Some(descriptions);
  | [] => None
  };
};

let _parseSheet = (sheet: sheet): option(list(t)) => {
  let rowGroupedCells =
    sheet.feed.entry
    |> Array.map((e: entry) => e.cell)
    |> CollectionUtils.groupBy((cell: cell) => cell.row)
    |> Array.to_list;
  switch (rowGroupedCells) {
  | [_, ...rows] =>
    let descriptions =
      rows
      |> List.map(row =>
           switch (row) {
           | (_rowNum, rowData) => makeStockDescription(rowData)
           }
         );
    Some(descriptions);
  | [] => None
  };
};

let useStockMatcher = stockDescriptionTsvUrl => {
  let (descriptions, setDescriptions) = React.useState(_ => None);

  React.useEffect0(() => {
    Js.Promise.(
      fetch(stockDescriptionTsvUrl)
      |> then_(response => response##text())
      |> then_(tsv => {
           tsv |> TsvParser.parseTsv |> parseSheet |> (s => setDescriptions(_ => s));
           Js.Promise.resolve();
         })
      // |> then_(tsv => {
      //      jsonResponse
      //      |> sheet
      //      |> parseSheet
      //      |> (d => setDescriptions(_ => d));
      //      Js.Promise.resolve();
      //    })
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
  (keySwitch: option(KeySwitch.t)) => (
    switch (keySwitch, descriptions) {
    | (Some(s), Some(ds)) => ds->List.find_opt(d => s.name == d.name, _)
    | _ => None
    }:
      option(t)
  );
};

// let useStockMatcher = stockDescriptionJsonUrl => {
//   let (descriptions, setDescriptions) = React.useState(_ => None);

//   React.useEffect0(() => {
//     Js.Promise.(
//       fetch(stockDescriptionJsonUrl)
//       |> then_(response => response##json())
//       |> then_(jsonResponse => {
//            jsonResponse
//            |> sheet
//            |> parseSheet
//            |> (d => setDescriptions(_ => d));
//            Js.Promise.resolve();
//          })
//       |> catch(err => {
//            Js.log(err);
//            setDescriptions(_ => None);
//            Js.Promise.resolve();
//          })
//       |> ignore
//     );
//     None;
//   });

//   // returning matcher function
//   (keySwitch: option(KeySwitch.t)) => (
//     switch (keySwitch, descriptions) {
//     | (Some(s), Some(ds)) => ds->List.find_opt(d => s.name == d.name, _)
//     | _ => None
//     }:
//       option(t)
//   );
// };
