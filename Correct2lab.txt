// МАЛЫШЕНКО ЯРОСЛАВ КМБ-2 ВАРИАНТ 5 
open System 

// РЕШЕНИЕ ЗАДАЧИ 1
// Функция для нахождения минимальной цифры в числе
let rec minDigit (n: int) : int =
    // База рекурсии: если число меньше 10, то это и есть минимальная цифра
    if n < 10 then n
    else
        // Находим последнюю цифру
        let lastDigit = n % 10
        // Находим минимум между последней цифрой и минимальной цифрой оставшейся части числа
        let restMin = minDigit (n / 10)
        if lastDigit < restMin then lastDigit else restMin


// функция ввода натуральных чисел с помощью клавиатуры 
let sms1() = 
    printfn(" ")
    printfn ("Введите натуральные числа через пробел (или 'q' для завершения ввода):")

// Функция для ввода натуральных чисел с клавиатуры
let inputNaturalNumbers () : int list = 
    sms1()
    let input = Console.ReadLine()

    if input.Trim().ToLower() = "q" then
        [] // Возвращаем пустой список, если пользователь ввел 'q'
    else
        input.Split(' ') // Разделяем строку по пробелам
        |> Array.toList // Преобразуем массив в список
        |> List.choose (fun str -> 
            match Int32.TryParse(str) with
            | (true, n) when n > 0 -> Some n // Возвращаем число, если оно корректное
            | _ -> None) // Игнорируем некорректные вводы

// Функция для генерации случайных чисел от 1 до 100
let generateRandomNumbers count =
    let rnd = Random() // Создаем экземпляр генератора случайных чисел
    [ for _ in 1 .. count -> rnd.Next(1, 100) ] // Генерируем список случайных чисел от 1 до 99

// РЕШЕНИЕ ЗАДАЧИ 2 
// Функция для вывода сообщения, если список пуст
let sms2() =
    printfn ("Список пуст!")

// Функция нахождения количества элементов списка, в которых встречается заданная цифра
let searchDigit (numbers: float list) (digit: int) : int = 
    if List.isEmpty numbers then
        sms2() // Если список пуст, выводим сообщение пользователю
        0 // Возвращаем 0, так как нет элементов для подсчета
    else
        let charDigit = char (digit + int '0') // Преобразуем цифру в символ для удобства поиска
        
        // Используем инструмент List.fold для подсчета совпадений
        List.fold (fun acc n -> 
            let str = n.ToString() // переводим число в строку
            if str.Contains(charDigit) then  // ищем совпа
                acc + 1 // Увеличиваем аккумулятор, если цифра найдена
            else 
                acc // Возвращаем текущий аккумулятор, если не найдено
        ) 0 numbers // Начальное значение счетчика - 0

    // Констуркция вида List.Fold folder state list 
    // folder - ф-ия, которая принимает текущее сост. аккум. и эл.списка, и возвращает новое состояние
    // state - начальное значение аккумулятора 
    // list список, над которым будет выполнена последовательность операций

let sms3() = 
    printfn "Введите действительные числа через пробел (или 'q' для завершения ввода):"
    printfn("(знак разделитель - запятая)")
// Функция для ввода действительных чисел с клавиатуры
let inputRealNumbers () : float list = 
    sms3()
    let input = Console.ReadLine()

    if input.Trim().ToLower() = "q" then
        [] // Возвращаем пустой список, если пользователь ввел 'q'
    else
        input.Split(' ') // Разделяем строку по пробелам
        |> Array.toList // Преобразуем массив в список
        |> List.choose (fun str -> 
            match Double.TryParse(str) with
            | (true, n) -> Some n // Возвращаем число, если оно корректное
            | _ -> None) // Игнорируем некорректные вводы

// Функция для генерации случайных действительных чисел
let generateRandomNumbers2 count =
    let rnd = Random() // Создаем экземпляр генератора случайных чисел
    [ for _ in 1 .. count -> Math.Round((rnd.NextDouble() * 200.0) - 100.0, 3)] // Генерируем список случайных чисел от -100 до 100

// функция ввода значений с клавиатуры 
let Input() =
    let zxc = Console.ReadLine()

    // Преобразуем строку в целое число
    match Int32.TryParse(zxc) with
    | (true, value) -> value // Если преобразование успешно, возвращаем значение
    | (false, _) -> 
        printfn "Неверный ввод. Используем значение по умолчанию (10)."
        10 // Значение по умолчанию, если ввод неверный


let sms4() = 
    printfn ("Выберите, какую задачу нам решить.")
let sms5() = 
    printfn ("Задача 1: Найти минимальные цифры в списке, состоящий из натуральных числах.")
let sms6() = 
    printfn ("Задача 2: Найти количество элементов списка, в которых встречается заданная цифра.")
let sms7() = 
    printfn (" ")
let sms8() = 
    printf ("Ваш выбор (1 или 2): ")
let sms9() = 
    printfn ("Как вы хотите ввести натуральные числа?")
let sms10() = 
    printfn ("1 - С клавиатуры")
let sms11() = 
    printfn ("2 - Генерация случайных чисел")
let sms12() = 
    printf ("Ваш выбор (1 или 2): ")
let sms13() =
    printfn ("Неверный выбор, используем случайные числа по умолчанию.")
let sms14(naturalNumbers) =
    printfn("Список: %A") (naturalNumbers)
let sms15(minDigits) = 
      printfn ("Минимальные цифры натуральных чисел: %A") (minDigits)
let sms16() = 
    printfn ("Как вы хотите ввести действительные числа?")
let sms17() = 
    printfn ("2 - Генерация случайных действительных чисел")
let sms18() = 
    printfn ("Введите цифру для поиска (0-9):")
let sms19() = 
    printfn ("Некорректный ввод. Пожалуйста, введите цифру от 0 до 9.")
let sms20() =
    printfn ("Неверный выбор, используем случайные числа по умолчанию.")
let sms21(digit, count) = 
    printfn ("Количество элементов, содержащих цифру %d: %d") (digit) (count)
let sms22() =
    printfn ("Неверный выбор задачи.")
let sms23(realNumbers) =
     printfn("Список: %A") (realNumbers)
let sms24() = 
    printf("Введите количество чисел списка: ")

// функция ввода числа с клавиатуры для генерации действ. чисел 
let rec getDigitInput () =
    sms18()
    let digitInput = Console.ReadLine()
    
    match Int32.TryParse(digitInput) with
    | (true, d) when d >= 0 && d <= 9 -> d // Если ввод корректен, возвращаем цифру
    | _ -> 
        sms19()
        getDigitInput () // Запрашиваем ввод снова


[<EntryPoint>]
let main argv =
    // Выбор задачи
    sms4()
    sms5()
    sms6()
    sms7()
    sms8()
    
    let taskChoice = Console.ReadLine()

    // Обработка выбора задачи
    match taskChoice with
    | "1" -> 
        // Запрос способа ввода для задачи 1
        sms9()
        sms10()
        sms11()
        sms12()
        
        let inputChoice = Console.ReadLine()
        
        // Получение натуральных чисел
        let naturalNumbers =
            match inputChoice with
            | "1" -> inputNaturalNumbers() // Ввод с клавиатуры
            | "2" ->  
                sms24()
                let zx = Input() // Ввод с клавиатуры 
                generateRandomNumbers zx // Генерация случайных натуральных чисел
            | _ -> 
                sms13()
                generateRandomNumbers 10

        // Получаем список минимальных цифр
        let minDigits = List.map (minDigit) (naturalNumbers)

        sms7()
        sms14(naturalNumbers)
        sms15(minDigits)

    | "2" -> 
        // Запрос способа ввода для задачи 2
        sms16()
        sms10()
        sms17()
        sms12()
        
        let inputChoice = Console.ReadLine()

        // Ввод цифры для поиска
        let digit = getDigitInput() // запрос числа пользователя 

        // Получение действительных чисел
        let realNumbers =
            match inputChoice with
            | "1" -> inputRealNumbers() // Ввод с клавиатуры
            | "2" ->
                sms24()
                let zx = Input() // Ввод с клавиатуры 
                generateRandomNumbers2 zx // Генерация случайных действительных чисел
            | _ -> 
                sms20()
                generateRandomNumbers2 10

        // Находим количество элементов, содержащих заданную цифру
        let count = searchDigit realNumbers digit
        sms23(realNumbers)
        sms21(digit, count)

    | _ -> 
        sms22()

    0 // Возвращаем 0, чтобы обозначить успешное завершение программы
