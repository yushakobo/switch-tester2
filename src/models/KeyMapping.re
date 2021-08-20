open SheetDecoder;
[@bs.val] external fetch: string => Js.Promise.t('a) = "fetch";

type keyInfo = {
  key: string,
  isCtrl: bool,
};

type attributeType = Belt.Map.String.t(string);
let arrayOfAttributes = Belt.Map.String.toArray;
type keySwitch = {
  id: int,
  name: string,
  imageUrl: string,
  type_: string,
  price: string,
  weight: string,
  comment: string,
  pin: string,
  order_no: string,
  otherAttributes: attributeType,
};

let defaultKeySwitch: keySwitch = {
  id: 0,
  name: "",
  imageUrl: "",
  type_: "",
  price: "",
  weight: "",
  comment: "",
  pin: "",
  order_no: "",
  otherAttributes: Belt.Map.String.empty,
};

let defaultKeyInfo: keyInfo = {key: "", isCtrl: false};

type t = {
  keyInfo,
  keySwitch,
};

let makeAttributes = (cols: list(col), headers: list(col)): attributeType => {
  cols
    ->Belt.List.map(c => {
      let title = headers
        ->Belt.List.getBy(h => h.index == c.index)
        ->Belt.Option.map(h => h.value)
        ->Belt.Option.getWithDefault("");
      (title, c.value)
    })
    ->Belt.List.keep(((t, v)) => t!="" && v!="")
    ->Belt.List.toArray
    ->Belt.Map.String.fromArray
};

let rowToKeyMapping = (row: row, header: row): t => {
  switch row.cols {
  | [_, _, key, isCtrl, name, imageUrl, type_, price, weight, comment, pin, order_no, ...rest] => {
    keyInfo: {key: key.value, isCtrl: isCtrl.value == "TRUE"},
    keySwitch: {
    id: 0,
    name: name.value,
    imageUrl: imageUrl.value,
    type_: type_.value,
    price: price.value,
    weight: weight.value,
    comment: comment.value,
    pin: pin.value,
    order_no: order_no.value,
    otherAttributes: makeAttributes(rest, header.cols)
    }
  }
  | _ => {keyInfo: defaultKeyInfo, keySwitch: defaultKeySwitch}
  };
};

let parseRows = (rows: list(row)): option(list(t)) => {
  switch (rows) {
  | [header, ...rest] =>
    let keyMappings =
      rest
      |> List.map(row => rowToKeyMapping(row, header))
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
           tsv |> tsvToRows |> parseRows |> (s => setKeyMappings(_ => s));
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
    }: option(keySwitch)
  );
};
