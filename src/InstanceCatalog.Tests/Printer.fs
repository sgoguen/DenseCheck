namespace InstanceCatalog

module Printer =

    open System

    type Writer =
        inherit IDisposable

        abstract Write: string -> unit

    type Printer =
        abstract Line: int
        abstract Column: int
        abstract PushIndentation: unit -> unit
        abstract PopIndentation: unit -> unit
        abstract Print: string -> unit
        abstract PrintNewLine: unit -> unit

    type PrinterImpl(writer: Writer, ?indent: string) =
        let indentSpaces = defaultArg indent "    "
        let builder = Text.StringBuilder()
        let mutable indent = 0
        let mutable line = 1
        let mutable column = 0

        member _.Flush() =
            if builder.Length > 0 then
                writer.Write(builder.ToString())
                builder.Clear() |> ignore

        interface IDisposable with
            member _.Dispose() = writer.Dispose()

        interface Printer with
            member _.Line = line
            member _.Column = column

            member _.PrintNewLine() =
                builder.AppendLine() |> ignore
                line <- line + 1
                column <- 0

            member _.PushIndentation() = indent <- indent + 1

            member _.PopIndentation() =
                if indent > 0 then
                    indent <- indent - 1

            member _.Print(str: string) =
                if not (String.IsNullOrEmpty(str)) then

                    if column = 0 then
                        let indent = String.replicate indent indentSpaces
                        builder.Append(indent) |> ignore
                        column <- indent.Length

                    builder.Append(str) |> ignore
                    column <- column + str.Length
