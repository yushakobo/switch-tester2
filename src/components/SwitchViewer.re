[@bs.scope "document"] [@bs.val]
external addKeybordEventListener:
  (string, ReactEvent.Keyboard.t => unit) => unit =
  "addEventListener";

[@bs.scope "document"] [@bs.val]
external removeKeybordEventListener:
  (string, ReactEvent.Keyboard.t => unit) => unit =
  "removeEventListener";

type help =
  | SwitchPins
  | SwitchType;

module HelpView = {
  [@react.component]
  let make = (~helpType: option(help), ~onHide) => {
    switch (helpType) {
    | None => React.null
    | Some(h) =>
      let helpImageUrl =
        switch (h) {
        | SwitchPins => Consts.helpImageOfPins
        | SwitchType => Consts.helpImageOfSwitchTypes
        };
      <div>
        <div className="modal">
          <img src=helpImageUrl className="help-image" />
        </div>
        <div className="overlay" onClick=onHide />
      </div>;
    };
  };
};

module DefaultView = {
  [@react.component]
  let make = () => {
    <div className="center">
      <img src=Consts.urlOfTopImage className="default-image" />
    </div>;
  };
};

module HelpButton = {
  [@react.component]
  let make = (~helpType: help, ~showHelpCallback) => {
    <img
      src="question.svg"
      className="help-icon"
      onClick={_ => showHelpCallback(helpType)}
    />;
  };
};

module SwitchAttributesView = {
  [@react.component]
  let make = (~attributes: KeySwitch.attributeType) => {
    <dl>
      {attributes
       |> KeySwitch.arrayOfAttributes
       |> Array.map(((k, v)) => {
            [|
              <dt key=k> {React.string(k)} </dt>,
              <dd key={j|$(k)-$(v)|j}> {React.string(v)} </dd>,
            |]
          })
       |> Belt.Array.concatMany
       |> React.array}
    </dl>;
  };
};

module SwitchView = {
  [@react.component]
  let make =
      (~keySwitch: option(KeySwitch.t), ~stock: option(StockDescription.t)) => {
    let (helpType, setHelpType) = React.useState(_ => None);

    let stockDescription =
      stock
      ->Belt.Option.map(st => st.description)
      ->Belt.Option.getWithDefault("");

    let handleShowHelp = (helpType: help) => {
      setHelpType(_ => Some(helpType));
    };

    switch (keySwitch) {
    | None => <DefaultView />
    | Some(s) =>
      <div className="summary">
        <HelpView helpType onHide={_ => setHelpType(_ => None)} />
        <div className="summary-image">
          <img src={s.imageUrl} className="switch-image" />
        </div>
        <div className="summary-info">
          <p className="switch-comment"> {React.string(s.comment)} </p>
          <p className="switch-name"> {React.string(s.name)} </p>
          <div className="right">
            <span className="stock-description">
              {React.string(stockDescription)}
            </span>
          </div>
          <div className="right">
            <span className="small"> {React.string({j|価格 |j})} </span>
            <span className="switch-price"> {React.string(s.price)} </span>
            <span className="small"> {React.string({j| 円|j})} </span>
          </div>
          <p className="switch-type">
            {React.string(s.type_ ++ {j|軸 / |j} ++ s.weight ++ "g")}
            <HelpButton helpType=SwitchType showHelpCallback=handleShowHelp />
          </p>
          <p className="switch-type">
            {React.string({j|ピン数: |j} ++ s.pin)}
            <HelpButton helpType=SwitchPins showHelpCallback=handleShowHelp />
          </p>
        </div>
        <div className="switch-detail">
          <SwitchAttributesView attributes={s.otherAttributes} />
        </div>
      </div>
    };
  };
};

module DebugView = {
  [@react.component]
  let make = (~currentKey) => {
    switch (currentKey) {
    | None => <p> {React.string("none")} </p>
    | Some(key) =>
      <div>
        <p> {React.string("key: " ++ ReactEvent.Keyboard.key(key))} </p>
        <p>
          {React.string(
             "ctrl: " ++ (ReactEvent.Keyboard.ctrlKey(key) ? "true" : "false"),
           )}
        </p>
      </div>
    };
  };
};

// entry point
[@react.component]
let make = () => {
  let count = React.useRef(0);
  let (currentKey, setCurrentKey) = React.useState(() => None);
  let (last4, setLast4) = React.useState(() => []);
  let (showDebug, setShowDebug) = React.useState(() => false);

  let keyMatcher =
    KeyMapping.useKeyMatcher(Consts.urlOfSwitchesSpreadsheetJson);
  let stockMatcher =
    StockDescription.useStockMatcher(Consts.urlOfStocksSpreadsheetJson);

  let handleKeyDown = (e: ReactEvent.Keyboard.t): unit => {
    e->ReactEvent.Keyboard.preventDefault;
    Js.log(ReactEvent.Keyboard.key(e));

    React.Ref.setCurrent(count, 0);
    setCurrentKey(_ => Some(e));
    setLast4(prev => [ReactEvent.Keyboard.key(e), ...prev]);
  };

  let onTick = () => {
    let currentCount = React.Ref.current(count);
    if (currentCount >= Consts.claerIntervalSec) {
      React.Ref.setCurrent(count, 0);
      setCurrentKey(_ => None);
    } else {
      React.Ref.setCurrent(count, currentCount + 1);
    };
  };

  React.useEffect0(() => {
    let id = Js.Global.setInterval(onTick, 1000);
    addKeybordEventListener("keydown", handleKeyDown);

    Some(
      () => {
        Js.Global.clearInterval(id);
        removeKeybordEventListener("keyDown", handleKeyDown);
      },
    );
  });

  React.useEffect1(
    () => {
      switch (Belt.List.take(last4, 4)) {
      | Some(["0", "Y", "I", "P"]) => setShowDebug(_ => true)
      | _ => ()
      };
      Js.log(Belt.List.toArray(last4));
      None;
    },
    [|last4|],
  );

  let keySwitch: option(KeySwitch.t) = keyMatcher(currentKey);
  let stock: option(StockDescription.t) = stockMatcher(keySwitch);

  <div className="container">
    {showDebug ? <DebugView currentKey /> : React.null}
    <SwitchView keySwitch stock />
  </div>;
};
