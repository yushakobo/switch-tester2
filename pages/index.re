module Head = {
  [@react.component] [@bs.module "next/head"]
  external make: (~children: React.element) => React.element = "default";
};

[@react.component]
let make = () => {
  <div>
    <Head> <link rel="stylesheet" href="../style.css" /> </Head>
    <SwitchViewer />
  </div>;
};

let default = make;
