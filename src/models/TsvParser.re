open SheetDecoder;

let parseTsv = (data: string): list(cell) =>
  String.split_on_char('\n', data)
  |> List.mapi(
      (rowIndex, row) => 
          List.mapi(
              (colIndex, col) => {row: rowIndex, col: colIndex+1, value: col},
              String.split_on_char('\t', row)
          )
    )
  |> List.flatten

