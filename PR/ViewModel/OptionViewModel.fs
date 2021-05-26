namespace ViewModel

//type OptionKind =
//    | Call
//    | Put
//    override x.ToString() = match x with | Call -> "Call" | _ -> "Put"

 
//Representation of a Option to the UI
type OptionViewModel(input : OptionRecord) = 
    inherit ViewModelBase()

    let mutable userInput = input
    let mutable optionPrice : float option = None
    let mutable optionDelta : float option = None

    member this.Kind 
        with get() = userInput.Kind
        and set(x) = 
            userInput <- {userInput with Kind = x }
            base.Notify("Kind")

    member this.StockPrice
        with get() = userInput.StockPrice
        and set(x) = 
            userInput <- {userInput with StockPrice = x }
            base.Notify("StockPrice")

    member this.Strike
        with get() = userInput.Strike
        and set(x) = 
            userInput <- {userInput with Strike = x }
            base.Notify("Strike")

    member this.Expiry 
        with get() = userInput.Expiry
        and set(x) = 
            userInput <- {userInput with Expiry = x }
            base.Notify("Expiry")

    member this.r 
        with get() = userInput.r
        and set(x) = 
            userInput <- {userInput with r = x }
            base.Notify("r")

    member this.v
        with get() = userInput.v
        and set(x) = 
            userInput <- {userInput with v = x }
            base.Notify("v")

    member this.OptionPrice
        with get() = optionPrice
        and set(x) = 
            optionPrice <- x
            base.Notify("OptionPrice")

    member this.OptionDelta
        with get() = optionDelta
        and set(x) = 
            optionDelta <- x
            base.Notify("OptionDelta")


    // Invoke the valuation based on user input
    member this.CalculateOption() = 
        let o : OptionRecord =
            {
                Kind = this.Kind
                StockPrice = this.StockPrice
                Strike = this.Strike
                Expiry = this.Expiry
                r = this.r
                v = this.v
            }
        
        //calculate
        let calc = OptionCalculationModel(o).Calculate()

        let price = fst calc
        let delta = snd calc

        //present to the user
        this.OptionPrice <- Option.Some (price)
        this.OptionDelta <- Option.Some (delta)
        //this.OptionPrice <- Option.Some (System.Math.Round(price, 2))
        //this.OptionDelta <- Option.Some (System.Math.Round(delta, 2))
        //this.OptionDelta <- Option.Some (snd calc)
        //this.OptionPrice <- Option.Some ((fst calc * 1000. |> round) / 1000.)
        //this.OptionDelta <- Option.Some ((snd calc * 1000. |> round) / 1000.)
        //this.OptionDelta <- Option.Some (snd calc)

