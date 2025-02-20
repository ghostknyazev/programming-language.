open System

// Задача 1: Список степеней числа 2
let CreateList(n: int) = 
    if n < 0 then
        [for i in n .. -1 -> 1.0 / (pown 2.0 (-i))] // Для отрицательных n
    else
        [for i in 0 .. n -> pown 2 i] // Для положительных n

// Задача 2: Нахождение первой цифры числа
let rec FirstDigit(n: int) =
    if n <= 0 then 
        printfn "Число не натуральное. Попробуйте снова."
        printf "Введите натуральное значение: "
        let input = Console.ReadLine() |> Int32.TryParse
        match input with
        | (true, newN) -> FirstDigit(newN) // Если ввод корректный, повторяем вызов
        | _ -> printfn "Некорректный ввод. Завершение программы."; 0 // Если ввод некорректный, завершаем
    else 
        if n < 10 then n // Если число меньше 10, это и есть первая цифра
        else FirstDigit(n / 10) // Рекурсивно делим число на 10
        

// Задача 3: Операции с комплексными числами
[<Struct>] // Атрибут класса, указывающий что тип Complex будет структурой 
type Complex = 
    { Real: double; // действительная часть числа 
    Imaginary: double } // мнимая часть числа 


// Функция создания комплексного числа 
let createComplex real imag = { Real = real; Imaginary = imag } // Возвращает структуру Complex с заданными действ. и мнимой частями

// Функция для сложения двух комплексных чисел 
let add (c1: Complex) (c2: Complex) = // c1 и c2 - комплексные числа 
    { Real = c1.Real + c2.Real; Imaginary = c1.Imaginary + c2.Imaginary } // складываем соответсвенно действ. и мнимые части 

// функция для разности двух комплексных чисел 
let subtract (c1: Complex) (c2: Complex) = // c1 и c2 - комплексные числа 
    { Real = c1.Real - c2.Real; Imaginary = c1.Imaginary - c2.Imaginary } // вычитаем соотвеств. действ. и мнимые части
 
// функция для умножения двух комплексных чисел 
let multiply (c1: Complex) (c2: Complex) = // c1 и c2 - комплексные числа
    // производим умножение по правилам
    { Real = c1.Real * c2.Real - c1.Imaginary * c2.Imaginary; 
      Imaginary = c1.Real * c2.Imaginary + c1.Imaginary * c2.Real }

// функция для деления комлпексных чисел 
let divide (c1: Complex) (c2: Complex) = // c1 и c2 - комплексные числа
    let denominator = c2.Real * c2.Real + c2.Imaginary * c2.Imaginary // вычисляет знаменатель
    if denominator = 0.0 then printfn("Нельзя делить на 0") // Проверка деления на 0
    // делим комплексные числа
    { Real = (c1.Real * c2.Real + c1.Imaginary * c2.Imaginary) / denominator;
    Imaginary = (c1.Imaginary * c2.Real - c1.Real * c2.Imaginary) / denominator }
      

// функция для возведения комплексного числа в степень 
let power (c: Complex) (n: int) = // c - комплексное число, n - число для возведения комплекс. числа в степень 
    // рекурсия для вычисления степени 
    let rec powerInner c n =
        if n = 0 then { Real = 1.0; Imaginary = 0.0 } // если n = 0, то число равно 1
        elif n = 1 then c // если у числа степень 1, то это и есть само число 
        else
            let temp = powerInner c (n / 2) // для ускоренных вычислеинй делим степень на 2
            // квадрат комплексного числа 
            let result = { Real = temp.Real * temp.Real - temp.Imaginary * temp.Imaginary; // действительная часть числа 
                           Imaginary = 2.0 * temp.Real * temp.Imaginary } // мнимая часть числа 
            if n % 2 = 0 then result // если n четное то возвращаем result 
            // умножение двух комлпескных чисел 
            else { Real = result.Real * c.Real - result.Imaginary * c.Imaginary; 
                   Imaginary = result.Real * c.Imaginary + result.Imaginary * c.Real } // В этом блоке создается новое комплексное число, которое является произведением result и c
    powerInner c n // рекурсивно вызываем 

// Функция для форматирования комплексного числа в строку
let formatComplex (complex: Complex) =
    // Если мнимая часть равна 0, возвращаем только действительную часть
    if complex.Imaginary = 0.0 then sprintf "%.2f" complex.Real
    // Если мнимая часть положительна, форматируем как "a + bi"
    elif complex.Imaginary > 0.0 then sprintf "%.2f + %.2fi" complex.Real complex.Imaginary
    // Если мнимая часть отрицательна, форматируем как "a - bi"
    else sprintf "%.2f - %.2fi" complex.Real (abs complex.Imaginary)


// Функция для ввода действительных значений с обработкой ошибок
let rec Input_double() = 
    let input = Console.ReadLine() // Чтение введенного значения
    // Проверка, удалось ли преобразовать введенное значение в действительное число
    match Double.TryParse(input) with
    | (true, result) -> result // Если преобразование успешно, возвращаем результат
    | (false, _) -> 
        printfn "Не правильное значение. Попробуйте снова." // Если не удалось, выводим сообщение об ошибке
        Input_double() // Рекурсивный вызов для повторного ввода

// Функция для чтения комплексного числа от пользователя
let readComplex () =
    printfn "Введите действительную часть комплексного числа:" // Запрос на ввод действительной части
    let real = Input_double() // Используем функцию для ввода действительного числа
    printfn "Введите мнимую часть комплексного числа:" // Запрос на ввод мнимой части
    let imag = Input_double() // Используем функцию для ввода действительного числа
    createComplex real imag // Создание комплексного числа с введенными значениями

// Функция для ввода целочисленных значений с обработкой ошибок
let rec Input() = 
    printfn "Введите целое значение: " // Запрос на ввод значения
    let input = Console.ReadLine() // Чтение введенного значения
    // Проверка, удалось ли преобразовать введенное значение в целое число
    match Int32.TryParse(input) with
    | (true, result) -> result // Если преобразование успешно, возвращаем результат
    | (false, _) -> 
        printfn "Не целое значение. Попробуйте снова." // Если не удалось, выводим сообщение об ошибке
        Input() // Рекурсивный вызов для повторного ввода

// Главная функция
let main() = 
    // Бесконечный цикл для постоянного запроса выбора задачи
    while true do 
        // Вывод меню с доступными задачами
        printfn "\nВыберите задачу (введите номер):"
        printfn "1. Сформировать список из степеней числа 2"
        printfn "2. Найти первую цифру числа"
        printfn "3. Операции с комплексными числами"
        printfn "4. Выход"
        
        // Чтение выбора пользователя
        let choice = Console.ReadLine()
        
        // Обработка выбора пользователя с помощью конструкции match
        match choice with
        | "1" -> 
            // Задача 1: Список степеней числа 2
            let n = Input() // Запрос количества степеней у пользователя
            let numbers = CreateList(n) // Создание списка степеней числа 2
            printfn "Список степеней числа 2: %A" numbers // Вывод списка

        | "2" -> 
            // Задача 2: Нахождение первой цифры числа
            printfn "Введите число:" // Запрос числа у пользователя
            let number = Input() // Получение введенного числа
            let firstDigit = FirstDigit(number) // Нахождение первой цифры
            printfn "Первая цифра числа %d: %d" number firstDigit // Вывод первой цифры

        | "3" -> 
            // Задача 3: Операции с комплексными числами
            printfn("Примечание: если вводите число с плавающей точкой, то ставьте запятую как знак раздетиль целой и дробной части.")
            printfn "Введите первое комплексное число:" // Запрос первого комплексного числа
            let c1 = readComplex() // Чтение первого комплексного числа
            printfn "Введите второе комплексное число:" // Запрос второго комплексного числа
            let c2 = readComplex() // Чтение второго комплексного числа

            // Вывод результатов операций с комплексными числами
            printfn "Результаты операций:"
            printfn "Сумма: %s" (formatComplex (add c1 c2)) // Вывод суммы
            printfn "Разность: %s" (formatComplex (subtract c1 c2)) // Вывод разности
            printfn "Произведение: %s" (formatComplex (multiply c1 c2)) // Вывод произведения
            printfn "Частное: %s" (formatComplex (divide c1 c2)) // Вывод частного
            
            // Запрос степени для возведения первого комплексного числа
            printfn "Введите степень для возведения первого комплексного числа:"
            let exponent = Input() // Получение степени
            printfn "Первое комплексное число в степени %d: %s" exponent (formatComplex (power c1 exponent)) // Вывод результата возведения в степень
            printfn "Второе комплексное число в степени %d: %s" exponent (formatComplex (power c2 exponent)) // Вывод результата возведения в степень

        | "4" -> 
            // Завершение работы программы
            printfn "Выход из программы."
            Environment.Exit(0) // Выход из приложения

        | _ -> 
            // Обработка неверного выбора
            printfn "Неверный выбор. Пожалуйста, попробуйте снова." // Сообщение об ошибке

// Запуск главной функции
main()
