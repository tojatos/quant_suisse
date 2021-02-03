namespace Lectures

open System

module Lecture1 =
    let fst (x, _, _) = x
    let mid (_, x, _) = x
    let lst (_, _, x) = x

    let flipX (x: float, y: float) = (-x, y)
    let flipY (x: float, y: float) = (x, -y)

    let rotate (x: float, y: float) angle =
        (cos (angle) * x - sin (angle) * y, sin (angle) * x + cos (angle) * y)

    let transpose (x1: float, y1: float) (x2: float, y2: float) = (x1, x2), (y1, y2)

    let round5 (x: float) = Math.Round(x, 5)

    let isOrthogonal (x1: float, y1: float) (x2: float, y2: float) =
        x1 ** 2. + y1 ** 2. |> round5 = 1.
        && x2 ** 2. + y2 ** 2. |> round5 = 1.
        && x1 * x2 + y1 * y2 |> round5 = 0.

    type Department =
        | IT
        | SALES
        | PR
        | HR

    type Employee =
        { Name: string
          Salary: float
          Department: Department }

    let employees: Employee list =
        [ { Name = "Ryszard"
            Salary = 15000.
            Department = IT }
          { Name = "Zygfryd"
            Salary = 2000.
            Department = SALES }
          { Name = "Krzysztof"
            Salary = 10000.
            Department = PR }
          { Name = "Ania"
            Salary = 9000.
            Department = SALES }
          { Name = "Rafał"
            Salary = 5000.
            Department = HR }
          { Name = "Adam"
            Salary = 20000.
            Department = IT }
          { Name = "Mateusz"
            Salary = 3000.
            Department = SALES }
          { Name = "Alexa"
            Salary = 10000.
            Department = SALES }
          { Name = "Monika"
            Salary = 13000.
            Department = IT }
          { Name = "Joanna"
            Salary = 5000.
            Department = HR } ]

    let printItRecords () =
        let itFilter =
            function
            | { Department = IT } -> true
            | _ -> false

        List.filter itFilter employees |> printfn "%A"

    let printSalarySum (department: Department) =
        (List.sumBy (fun e -> e.Salary) (employees
        |> List.filter (fun e -> e.Department = department)))
        |> printfn "%f"

    let removeDups (xs: int list) =
        let rec r (ys: int list) acc =
            match ys with
            | h :: t ->
                r
                    t
                    (if List.contains h acc then
                         acc
                     else
                         h :: acc)
            | [] -> List.rev acc

        r xs []

    // let removeDups2 (xs: int list) =
    //     let f = fun x (i, acc) -> (i-1, if List.contains x xs.[0..i] then acc else x :: acc)
    //     List.foldBack f xs (List.length xs - 2,[]) |> snd

    let removeDups2 lst =
        List.foldBack (fun elem state -> elem :: List.filter (fun e -> e <> elem) state) lst []
