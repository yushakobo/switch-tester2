type col = {
  index: int,
  value: string,
};

type row = {
  index: int,
  cols: list(col),
};

let textToRows = (seperator: char) => 
  (data: string) => {
    let lines = String.split_on_char('\n', data);
    lines |> List.mapi(
        (rowIndex, row) => {
          let cols: list(col) = String.split_on_char(seperator, row)
                      |> List.mapi((index, value) => {index, value});
          {index: rowIndex, cols: cols}
        }
      )
  }

let tsvToRows = textToRows('\t');
let csvToRows = textToRows(',');
