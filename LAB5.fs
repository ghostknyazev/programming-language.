// МАЛЫШЕНКО ЯРОСЛАВ КМБ-2 ВАРИАНТ 5 ЛАБ 5


open System
open System.IO


// Универсальная функция для вывода сообщений с поддержкой форматирования
let printMessage format = Printf.kprintf (printfn "%s") format



// РЕШЕНИЕ ЗАДАЧ 1 
// Функция для нахождения минимальной цифры в числе
let rec minDigit (n: int) =
    if n < 10 then n
    else
        let lastDigit = n % 10
        let restMin = minDigit (n / 10)
        if lastDigit < restMin then lastDigit else restMin


// Функция для генерации последовательности из случайных чисел
let generateNaturalNumbers (count: int) : seq<int> =
    let rnd = Random()
    Seq.init count (fun _ -> rnd.Next(1, 100)) 

// Функция для ввода последовательности чисел с клавиатуры
let rec inputNaturalNumbers () =
    printMessage "Введите натуральные числа через пробел (или 'q' для выхода):"
    let input = Console.ReadLine()
    if input.Trim().ToLower() = "q" then Seq.empty
    else
        let numbers = 
            input.Split(' ') 
            |> Array.choose (fun s -> 
                match Int32.TryParse(s) with
                | (true, n) when n > 0 -> Some n
                | _ -> 
                    printMessage "Некорректный ввод: '%s'." (s)
                    None)
        if Array.isEmpty numbers then
            inputNaturalNumbers() 
        else
            Seq.ofArray numbers

// Функция для ввода количества случайных чисел
let rec inputNaturalCount () : int =
    printMessage "Введите количество случайных чисел (натуральное число) или 'q' для выхода:"
    let input = Console.ReadLine()
    if input.Trim().ToLower() = "q" then
        exit 0
    else
        match Int32.TryParse(input) with
        | (true, count) when count > 0 -> count
        | _ -> 
            printMessage "Некорректный ввод. Пожалуйста, введите натуральное число."
            inputNaturalCount() 

// Функция для чтения чисел из файла
let readNaturalNumbersFromFile (filePath: string) =
    try
        let lines = File.ReadAllLines(filePath) 
        let validNumbers = 
            lines
            |> Array.collect (fun line -> 
                line.Split(' ') 
                |> Array.choose (fun s -> 
                    match Int32.TryParse(s.Trim()) with
                    | (true, n) when n > 0 ->       
                        Some n
                    | _ -> 
                        printMessage "Некорректная строка в файле: '%s'." s
                        None))
        if Array.isEmpty validNumbers then
            printMessage "Нет корректных чисел в файле."
            Seq.empty
        else
            validNumbers |> Seq.ofArray
    with
    | :? FileNotFoundException ->
        printMessage "Файл не найден: %s" filePath
        Seq.empty

// РЕШЕНИЕ ЗАДАЧИ 2 
// Функция для генерации последовательности из случайных действительных чисел
let generateRealNumbers (count: int) : seq<float> =
    let rnd = Random()
    Seq.init count (fun _ -> Math.Round(rnd.NextDouble() * 99.0 + 1.0, 3)) 


// Функция для ввода последовательности действительных чисел с клавиатуры
let rec inputRealNumbers () : seq<float> =
    printMessage "Введите действительные числа через пробел (или 'q' для выхода):"
    let input = Console.ReadLine()
    if input.Trim().ToLower() = "q" then Seq.empty
    else
        let numbers = 
            input.Split(' ') 
            |> Array.choose (fun s -> 
                match Double.TryParse(s) with
                | (true, n) when n > 0.0 -> Some n
                | _ -> 
                    printMessage "Некорректный ввод: '%s'." s
                    None)
        if Array.isEmpty numbers then
            inputRealNumbers() 
        else
            Seq.ofArray numbers

// Функция для ввода количества случайных действительных чисел
let rec inputRealCount () : int =
    printMessage "Введите количество случайных чисел (натуральное число) или 'q' для выхода:"
    let input = Console.ReadLine()
    if input.Trim().ToLower() = "q" then
        exit 0
    else
        match Int32.TryParse(input) with
        | (true, count) when count > 0 -> count
        | _ -> 
            printMessage "Некорректный ввод. Пожалуйста, введите натуральное число."
            inputRealCount() // Исправлено на inputRealCount

// Функция для чтения действительных чисел из файла
let readRealNumbersFromFile (filePath: string) : seq<float> =
    try
        let lines = File.ReadAllLines(filePath) // Чтение строк из файла
        let validNumbers = 
            lines
            |> Array.collect (fun line -> 
                line.Split(' ') // Разделяем строку на части по пробелам
                |> Array.choose (fun s -> 
                    match Double.TryParse(s.Trim()) with
                    | (true, n) when n > 0.0 -> 
                        printMessage "Число: '%.3f'." n
                        Some n
                    | _ -> 
                        printMessage "Некорректная строка в файле: '%s'." s
                        None))
        if Array.isEmpty validNumbers then
            printMessage "Нет корректных чисел в файле."
            Seq.empty
        else
            validNumbers |> Seq.ofArray
    with
    | :? FileNotFoundException ->
        printMessage "Файл не найден: %s" filePath
        Seq.empty

// функция нахождения цифры в числах последовательности 
let searchDigit (numbers: seq<float>) (digit: int) : int = 
    if Seq.isEmpty numbers then
        0 
    else
        let charDigit = char (digit + int '0') 
        
        Seq.fold (fun acc n -> 
            let str = n.ToString() 
            if str.Contains(charDigit) then  
                acc + 1 
            else 
                acc 
        ) 0 numbers 


// РЕШЕНИЕ ЗАДАЧИ 3 
let findShortestFileName (directoryPath: string) =
    if Directory.Exists(directoryPath) then
        let files = Directory.GetFiles(directoryPath, "*", SearchOption.AllDirectories)
      
        if files.Length > 0 then
            let shortestFile = 
                files
                |> Seq.map Path.GetFileName 
                |> Seq.minBy String.length 
            
            printMessage "Самое короткое название файла: %s" shortestFile
        else
            printMessage "В указанном каталоге нет файлов."
    else
        printMessage "Указанный каталог не существует."

// Главная функция 
[<EntryPoint>]
let main argv =
    printMessage"Выберите задачу:"
    printMessage "1. Найти минимальные цифры в последовательности натуральных чисел."
    printMessage "2. Найти количество элементов последовательности, в которых встречается заданная цифра."
    printMessage "3. Вывести самое короткое название файла в указанном каталоге."
    let taskChoice = Console.ReadLine()

    match taskChoice with
    | "1" -> 
        printMessage "Выберите способ заполнения:"
        printMessage "1. Ввод с клавиатуры"
        printMessage "2. Генерация случайных чисел"
        printMessage "3. Чтение из файла"
        let inputChoice = Console.ReadLine()

        let numbers =
            match inputChoice with
            | "1" -> inputNaturalNumbers()
            | "2" -> 
                let count = inputNaturalCount()
                generateNaturalNumbers count
            | "3" -> 
                printMessage "Введите путь к файлу: "
                let filePath = Console.ReadLine()
                if File.Exists(filePath) then
                    readNaturalNumbersFromFile filePath
                else
                    printMessage "Файл не найден. Пожалуйста, проверьте путь."
                    Seq.empty 
            | _ -> 
                printMessage "Некорректный выбор. Программа завершена."
                exit 1

        if Seq.isEmpty numbers then
            printMessage "Последовательность пуста. Невозможно найти минимальные цифры."
        else
            printMessage "Минимальные цифры: "
            Seq.iter (fun n -> printMessage "Число: %d, Минимальная цифра: %d" n (minDigit n)) numbers
    | "2" -> 
        printMessage "Выберите способ заполнения:"
        printMessage "1. Ввод с клавиатуры"
        printMessage "2. Генерация случайных чисел"
        printMessage "3. Чтение из файла"
        let inputChoice = Console.ReadLine()

        let numbers =
            match inputChoice with
            | "1" -> inputRealNumbers()
            | "2" -> 
                printMessage "Введите количество случайных чисел (натуральное число) или 'q' для выхода:"
                let countInput = Console.ReadLine()
                match countInput with
                | "q" -> exit 0
                | _ -> 
                    match Int32.TryParse(countInput) with
                    | (true, count) when count > 0 ->  
                        let generatedNumbers = generateRealNumbers count
                        generatedNumbers 
                    | _ -> 
                        printMessage "Некорректный ввод. Пожалуйста, введите положительное натуральное число."
                        Seq.empty
            | "3" -> 
                printMessage "Введите путь к файлу: "
                let filePath = Console.ReadLine()
                if File.Exists(filePath) then
                    readRealNumbersFromFile filePath
                else
                    printMessage "Файл не найден. Пожалуйста, проверьте путь."
                    Seq.empty 
            | _ -> 
                printMessage "Некорректный выбор. Программа завершена."
                exit 1

        if Seq.isEmpty numbers then
            printMessage "Последовательность пуста. Невозможно найти цифры."
        else
   
            Seq.iter (fun n -> printMessage "Сгенерированное число: %.3f" n) numbers
    
            printMessage "Введите цифру для поиска (от 0 до 9):"
            let digitInput = Console.ReadLine()
            match Int32.TryParse(digitInput) with
            | (true, digit) when digit >= 0 && digit <= 9 -> 
                let count = searchDigit numbers digit
                printMessage "Количество элементов, содержащих цифру %d: %d" digit count
            | _ -> 
                printMessage "Некорректный ввод. Пожалуйста, введите цифру от 0 до 9."





     | "3" -> 

        printMessage "Введите путь к каталогу: "
        let directoryPath = Console.ReadLine()
        findShortestFileName directoryPath

    | _ -> 
        printMessage "Некорректный выбор задачи."

    0 
