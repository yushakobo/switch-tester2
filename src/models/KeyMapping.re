open SheetDecoder;
[@bs.val] external fetch: string => Js.Promise.t('a) = "fetch";

type keyInfo = {
  key: string,
  isCtrl: bool,
};

let defaultKeyInfo: keyInfo = {key: "", isCtrl: false};

type t = {
  keyInfo,
  keySwitch: KeySwitch.t,
};

let default: t = {keyInfo: defaultKeyInfo, keySwitch: KeySwitch.default()};

let updateKeyMapping = (m: t, cell: cell, header: array(cell)): t =>
  switch (cell.col) {
  | 1
  | 2 => m
  | 3 => {
      ...m,
      keyInfo: {
        ...m.keyInfo,
        key: cell.value,
      },
    }
  | 4 => {
      ...m,
      keyInfo: {
        ...m.keyInfo,
        isCtrl: cell.value == "TRUE",
      },
    }
  | _ => {
      ...m,
      keySwitch: KeySwitch.updateKeySwitch(m.keySwitch, cell, header),
    }
  };


let makeKeyMapping = (data: array(cell), header: array(cell)): t =>
  data
  |> Array.fold_left(
       (m, cell) => updateKeyMapping(m, cell, header),
       default,
     );

let _parseSheet = (sheet: sheet): option(list(t)) => {
  let rowGroupedCells =
    sheet.feed.entry
    |> Array.map((e: entry) => e.cell)
    |> CollectionUtils.groupBy((cell: cell) => cell.row)
    |> Array.to_list;
  switch (rowGroupedCells) {
  | [(_, header), ...rows] =>
    let keyMappings =
      rows
      |> List.map(row =>
           switch (row) {
           | (_rowNum, rowData) => makeKeyMapping(rowData, header)
           }
         )
      |> List.filter(kmap => kmap.keySwitch.name != "");
    Some(keyMappings);
  | [] => None
  };
};

let parseSheet = (cells: list(cell)): option(list(t)) => {
  let rowGroupedCells =
    cells
    |> Array.of_list
    |> CollectionUtils.groupBy((cell: cell) => cell.row)
    |> Array.to_list;
  switch (rowGroupedCells) {
  | [(_, header), ...rows] =>
    let keyMappings =
      rows
      |> List.map(row =>
           switch (row) {
           | (_rowNum, rowData) => makeKeyMapping(rowData, header)
           }
         )
      |> List.filter(kmap => kmap.keySwitch.name != "");
    Some(keyMappings);
  | [] => None
  };
};

let useKeyMatcher = keyMappingTsvUrl => {
  let (keyMappings, setKeyMappings) = React.useState(_ => None);

  React.useEffect0(() => {
    Js.Promise.(
      fetch(keyMappingTsvUrl)
      |> then_(response => response##text())
      |> then_(tsv => {
           tsv |> TsvParser.parseTsv |> parseSheet |> (s => setKeyMappings(_ => s));
           Js.Promise.resolve();
         })
      |> catch(err => {
           Js.log(err);
           setKeyMappings(_ => None);
           Js.Promise.resolve();
         })
      |> ignore
    );
    None;
  });

  let isKeyEq = (key: ReactEvent.Keyboard.t, keyMap: keyInfo): bool =>
    ReactEvent.Keyboard.key(key) == keyMap.key
    && ReactEvent.Keyboard.ctrlKey(key) == keyMap.isCtrl;

  // returning matcher function
  (key: option(ReactEvent.Keyboard.t)) => (
    switch (key, keyMappings) {
    | (Some(k), Some(ms)) =>
      ms
      ->List.find_opt(m => isKeyEq(k, m.keyInfo), _)
      ->Belt.Option.map(m => m.keySwitch)
    | _ => None
    }: option(KeySwitch.t)
  );
};

// let _useKeyMatcher = keyMappingJsonUrl => {
//   let (keyMappings, setKeyMappings) = React.useState(_ => None);

//   React.useEffect0(() => {
//     Js.Promise.(
//       fetch(keyMappingJsonUrl)
//       |> then_(response => response##json())
//       |> then_(jsonResponse => {
//            jsonResponse |> sheet |> parseSheet |> (s => setKeyMappings(_ => s));
//            Js.Promise.resolve();
//          })
//       |> catch(err => {
//            Js.log(err);
//            setKeyMappings(_ => None);
//            Js.Promise.resolve();
//          })
//       |> ignore
//     );
//     None;
//   });

//   let isKeyEq = (key: ReactEvent.Keyboard.t, keyMap: keyInfo): bool =>
//     ReactEvent.Keyboard.key(key) == keyMap.key
//     && ReactEvent.Keyboard.ctrlKey(key) == keyMap.isCtrl;

//   // returning matcher function
//   (key: option(ReactEvent.Keyboard.t)) => (
//     switch (key, keyMappings) {
//     | (Some(k), Some(ms)) =>
//       ms
//       ->List.find_opt(m => isKeyEq(k, m.keyInfo), _)
//       ->Belt.Option.map(m => m.keySwitch)
//     | _ => None
//     }:
//       option(KeySwitch.t)
//   );
// };
