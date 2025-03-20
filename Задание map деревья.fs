open System

// Функция для вывода сообщений
let printMessage format = Printf.kprintf (printfn "%s") format

// Структура узла дерева
type Leaf = 
    { value: string; left: Leaf option; right: Leaf option }

// Функция для вставки строки в бинарное дерево
let rec insertLeaf (tree: Leaf option) (newValue: string) : Leaf option =
    match tree with 
    | Some t -> 
        if newValue < t.value then 
            Some { value = t.value; left = insertLeaf t.left newValue; right = t.right }
        else if newValue > t.value then 
            Some { value = t.value; left = t.left; right = insertLeaf t.right newValue }
        else 
            Some t 
    | None -> 
        Some { value = newValue; left = None; right = None }

// Функция для замены последнего символа в строках дерева
let replaceLastChar (newChar: char) (value: string) : string =
    if value.Length > 0 then
        value.Substring(0, value.Length - 1) + string newChar
    else
        value // Если строка пустая, ничего не меняем

// Функция для применения функции к каждому элементу дерева и создания нового дерева
let rec map (f: string -> string) (tree: Leaf option) : Leaf option =
    match tree with
    | None -> None
    | Some t ->
        let newValue = f t.value
        Some { value = newValue; left = map f t.left; right = map f t.right }

// Функция для печати дерева
let rec printTree (tree: Leaf option) (indent: string) =
    match tree with
    | None -> ()
    | Some n ->
        printfn "%s%s" indent n.value
        printTree n.left (indent + "    ")
        printTree n.right (indent + "    ")

// Функция для ввода слов
let rec inputWords () =
    printMessage "Введите строку (или нажмите Enter для завершения ввода): "
    let input = Console.ReadLine()
    if String.IsNullOrWhiteSpace(input) then
        []
    else
        input :: inputWords()

// Функция для создания дерева из списка слов
let rec createTree (words: string list) (tree: Leaf option) : Leaf option =
    match words with
    | [] -> tree
    | head :: tail -> 
        let newTree = insertLeaf tree head
        createTree tail newTree

[<EntryPoint>]
let main argv =
    let stringValues = inputWords()
    let tree = createTree stringValues None

    printMessage "Созданное дерево:"
    printTree tree ""

    printMessage "Введите символ для замены последнего символа в строках: "
    let inputChar = Console.ReadLine()

    let newChar =
        if inputChar.Length = 1 then
            inputChar.[0]
        else
            printMessage "Введено неверное значение, используется символ по умолчанию '!'."
            '!'

    let updatedTree = map (fun s -> replaceLastChar newChar s) tree 

    printMessage "Обновленное дерево:"
    printTree updatedTree ""

    0 
